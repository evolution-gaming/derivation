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
import io.circe.ACursor
import io.circe.Decoder.Result

trait ConfiguredDecoder[A] extends Decoder[A]

object ConfiguredDecoder:

    inline def derived[A](using config: => Config[A]): ConfiguredDecoder[A] =
        summonFrom {
            case given Mirror.ProductOf[A] => deriveForProduct[A].instance
            case given Mirror.SumOf[A]     => deriveForSum[A]
            case _                         => underiveableError[ConfiguredDecoder[A], A]
        }

    inline def deriveForProduct[A](using
        mirror: Mirror.ProductOf[A],
    ): LazySummonByConfig[ConfiguredDecoder, A] =
        val fieldInstances =
            LazySummon.all[mirror.MirroredElemLabels, A, Decoder, ConfiguredDecoder, mirror.MirroredElemTypes]
        ProductDecoder[A](mirror)(fieldInstances)

    end deriveForProduct

    extension [A](oa: Option[A]) def toFailure(s: => String): Decoder.Result[A] = oa.toRight(DecodingFailure(s, Nil))

    private def constName(cur: HCursor, discriminator: Option[String]): Decoder.Result[(String, ACursor)] =
        discriminator match
            case Some(field) => for (sub <- cur.get[String](field)) yield (sub, cur)
            case None        =>
                for sub <- cur.keys
                               .collect { case it if it.size == 1 => it.head }
                               .toFailure("expecting an object with a single key")
                yield (sub, cur.downField(sub))

    inline given [A: Mirror.ProductOf]: LazySummonByConfig[ConfiguredDecoder, A] = deriveForProduct[A]

    private inline def deriveForSum[A](using config: => Config[A], mirror: Mirror.SumOf[A]): ConfiguredDecoder[A] =
        val constInstances =
            LazySummon.all[mirror.MirroredElemLabels, A, Decoder, ConfiguredDecoder, mirror.MirroredElemTypes]
        val names          = constValueTuple[mirror.MirroredElemLabels].toIArray.toVector.asInstanceOf[Vector[String]]

        SumDecoder(config, mirror)(constInstances.toMap[A](names), names)

    class ProductDecoder[A](mirror: Mirror.ProductOf[A])(
        fieldInstances: LazySummon.All[Decoder, mirror.MirroredElemTypes],
    ) extends LazySummonByConfig[ConfiguredDecoder, A]:
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

    class SumDecoder[A](config: => Config[A], mirror: Mirror.SumOf[A])(
        mkSubDecoders: => Map[String, Decoder[A]],
        names: Vector[String],
    ) extends ConfiguredDecoder[A]:

        lazy val cfg                       = config
        lazy val subDecoders               = mkSubDecoders
        lazy val all                       = cfg.constrFromRenamed.keys.mkString(", ")
        def apply(cur: HCursor): Result[A] =
            for
                subDown           <- constName(cur, cfg.discriminator)
                (subRenamed, down) = subDown
                subName           <- cfg.constrFromRenamed
                                         .get(subRenamed)
                                         .toFailure(s"constructor $subRenamed not found expected one of: $all")
                sub               <- subDecoders
                                         .get(subName)
                                         .toFailure(s"Internal error: should not happend, could not found $subName constructor info")
                result            <- sub.tryDecode(down)
            yield result
