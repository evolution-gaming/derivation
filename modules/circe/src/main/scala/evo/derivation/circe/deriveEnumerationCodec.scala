package evo.derivation.circe

import evo.derivation.LazySummon
import evo.derivation.config.Config
import evo.derivation.internal.underiveableError
import evo.derivation.internal.showType
import evo.derivation.internal.Matching
import io.circe.*

import scala.compiletime.*
import scala.compiletime.error
import scala.deriving.Mirror

private inline def underiveableEnumerationCodec[A]: Nothing =
    error("could not derive " + showType[Codec[A]] + ", " + showType[A] + "'s, children cannot have fields.")

inline def deriveEnumerationCodec[A]: Codec[A] = EnumerationCodec.derived[A]

private trait EnumerationCodec[A] extends Codec[A]
private object EnumerationCodec:
    inline def derived[A]: EnumerationCodec[A] =
        summonFrom {
            case given Mirror.ProductOf[A] =>
                deriveForProduct[A]
            case given Mirror.SumOf[A]     =>
                // val config: Config[A] = Config.derived[A]
                // def hasFields = config.constructors.values.exists(_.fieldNames.nonEmpty)
                // if hasFields then underiveableEnumerationCodec[A]
                // else deriveForSum[A]
                deriveForSum[A]
            case _                         => underiveableError[EvoDecoder[A], A]
        }
    end derived

    case class ProductImpl[A](name: String)(using mirror: Mirror.ProductOf[A]) extends EnumerationCodec[A] {
        def apply(c: HCursor): Decoder.Result[A] =
            Decoder.decodeString.apply(c) match
                case Right(value) if value == name => Right(mirror.fromProduct(EmptyTuple))
                case Right(value)                  => Left(DecodingFailure(s"got $value, expected $name", c.history))
                case Left(err)                     => Left(err)
        end apply

        def apply(a: A): Json = Json.fromString(name)
    }

    private inline def deriveForProduct[A](using mirror: Mirror.ProductOf[A]): EnumerationCodec[A] =
        ProductImpl(
          constValue[mirror.MirroredLabel],
        )

    private inline def summonSumOf[T <: Tuple]: List[EnumerationCodec[?]] = inline erasedValue[T] match
        case _: (t *: ts)  =>
            summonFrom {
                case given Mirror.SumOf[`t`]     =>
                    deriveForSum[`t`] :: summonSumOf[ts]
                case given Mirror.ProductOf[`t`] =>
                    deriveForProduct[`t`] :: summonSumOf[ts]
            }
        case _: EmptyTuple => Nil

    private inline def deriveForSum[A](using mirror: Mirror.SumOf[A]): EnumerationCodec[A] = {
        val codecs                = summonSumOf[mirror.MirroredElemTypes].asInstanceOf[List[EnumerationCodec[A]]]
        val names                 = constValueTuple[mirror.MirroredElemLabels].toList.asInstanceOf[List[String]]
        val matching: Matching[A] = Matching.create[A]

        val decoder: Decoder[A] = codecs.reduceLeft(_ or _)
        new EnumerationCodec[A] {
            def apply(c: HCursor): Decoder.Result[A] =
                c.value.asString match
                    case None        => Left(DecodingFailure(s"Expected string value, got ${c.value}", c.history))
                    case Some(value) =>
                        decoder.apply(c) match
                            case Right(decoded) => Right(decoded)
                            case _              => Left(DecodingFailure(s"$value is not one of $names", c.history))

            def apply(a: A): Json = Json.fromString(matching.matched(a))
        }
    }
end EnumerationCodec
