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
import java.awt.Cursor
import scala.collection.immutable.Iterable
import evo.derivation.LazySummon
import evo.derivation.LazySummon.LazySummonByConfig
import java.util.Arrays

trait ConfiguredDecoder[A] extends Decoder[A]

object ConfiguredDecoder:

    inline def derived[A](using config: => Config[A]): ConfiguredDecoder[A] =
        summonFrom {
            case given Mirror.ProductOf[A] => deriveForProduct[A].instance
            case given Mirror.SumOf[A]     => deriveForSum[A]
            case _                         => underiveableError[ConfiguredDecoder[A], A]
        }

    private inline given deriveForProduct[A](using
        mirror: Mirror.ProductOf[A],
    ): LazySummonByConfig[ConfiguredDecoder, A] =
        val fieldInstances = LazySummon.all[mirror.MirroredElemLabels, A, Decoder, mirror.MirroredElemTypes]
        new:
            def instance(using config: => Config[A]) = new:

                lazy val infos = config.top.fieldInfos

                private def onField(
                    cur: HCursor,
                )(decoder: LazySummon.Of[Decoder], info: FieldInfo): Decoder.Result[decoder.FieldType] =
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

    extension [A](oa: Option[A]) def toFailure(s: => String): Decoder.Result[A] = oa.toRight(DecodingFailure(s, Nil))

    private def constName(cur: HCursor, discriminator: Option[String]): Decoder.Result[String] =
        cur.keys.collect { case it if it.size == 1 => it.head }.toFailure("expecting an object with a single key")

    private inline def deriveForSum[A](using config: => Config[A], mirror: Mirror.SumOf[A]): ConfiguredDecoder[A] =
        val constInstances = LazySummon.all[mirror.MirroredElemLabels, A, Decoder, mirror.MirroredElemTypes]
        val names = constValueTuple[mirror.MirroredElemLabels].toIArray.map(_.asInstanceOf[String])

        val subDecoders = constInstances.toMap[A](names)
        lazy val cfg = config
        lazy val all = cfg.down.keys.mkString(", ")
        cur =>
            for
                name <- constName(cur, cfg.discriminator)
                sub <- subDecoders.get(name).toFailure(s"constructor $name not found expected one of: $all")
                result <- sub.tryDecode(cur)
            yield result
