package evo.derivation.circe

import io.circe.Decoder
import scala.deriving.Mirror
import evo.derivation.Config
import scala.compiletime.*
import evo.derivation.internal.underiveableError
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import cats.data.Validated
import cats.data.NonEmptyList
import evo.derivation.Config.FieldInfo

trait ConfiguredDecoder[A] extends Decoder[A]

object ConfiguredDecoder:
    inline def derived[A](using => Config[A]): ConfiguredDecoder[A] =
        summonFrom {
            case given Mirror.ProductOf[A] => deriveForProduct[A]
            case given Mirror.SumOf[A]     => deriveForSum[A]
            case _                         => underiveableError[ConfiguredDecoder[A], A]
        }

    private inline def deriveForProduct[A](using
        config: => Config[A],
        mirror: Mirror.ProductOf[A],
    ): ConfiguredDecoder[A] =
        lazy val infos = config.top.fieldInfos
        val fieldInstances = LazySummon.all[mirror.MirroredElemLabels, Decoder, mirror.MirroredElemTypes]
        new:

            private def onField(
                cur: HCursor,
            )(decoder: LazySummon[_, Decoder, _], info: FieldInfo): Decoder.Result[decoder.FieldType] =
                val cursor = if info.embed then cur else cur.downField(info.name)
                decoder.use(cursor.as[decoder.FieldType])

            def apply(cur: HCursor): Decoder.Result[A] =
                fieldInstances.useEitherFast(infos)(onField(cur)) match
                    case Left(err)    => Left(err)
                    case Right(tuple) => Right(mirror.fromProduct(tuple))

            override def decodeAccumulating(cur: HCursor): Decoder.AccumulatingResult[A] =
                fieldInstances.useEithers(infos)(onField(cur)) match
                    case Left(head +: rest) => Validated.Invalid(NonEmptyList(head, rest.toList))
                    case Left(_)            => Validated.invalidNel(DecodingFailure("unknown error", Nil))
                    case Right(tuple)       => Validated.Valid(mirror.fromProduct(tuple))

    end deriveForProduct

    private inline def deriveForSum[A](using Mirror.SumOf[A]): ConfiguredDecoder[A] = ???
