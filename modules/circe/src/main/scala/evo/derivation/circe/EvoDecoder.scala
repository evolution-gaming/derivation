package evo.derivation.circe

import io.circe.Decoder
import evo.derivation.template.Template
import evo.derivation.LazySummon
import evo.derivation.config.Config
import scala.deriving.Mirror
import evo.derivation.internal.Matching
import evo.derivation.ValueClass
import io.circe.HCursor
import evo.derivation.config.ForField
import cats.data.Validated
import cats.data.NonEmptyList
import io.circe.DecodingFailure
import io.circe.Decoder.Result
import io.circe.ACursor
import evo.derivation.LazySummon.LazySummonByConfig

trait EvoDecoder[A] extends Decoder[A]

object EvoDecoder extends Template:
    type OfNewtype[A] = Decoder[A]
    type OfField[A]   = Decoder[A]
    type OfSubtype[A] = Decoder[A]

    type Provide[A] = EvoDecoder[A]

    inline given [A: Mirror.ProductOf]: LazySummonByConfig[EvoDecoder, A] = lazySummonForProduct

    def product[A](using mirror: Mirror.ProductOf[A])(
        fields: LazySummon.All[Decoder, mirror.MirroredElemTypes],
    )(using config: => Config[A], ev: A <:< Product): EvoDecoder[A] = new:

        lazy val infos = config.top.fields.map(_._2)

        private def onField(
            cur: HCursor,
        )(decoder: LazySummon.Of[Decoder], info: ForField): Decoder.Result[decoder.FieldType] =
            val cursor = if info.embed then cur else cur.downField(info.name)
            decoder.use(cursor.as[decoder.FieldType])
        end onField

        def apply(cur: HCursor): Decoder.Result[A] =
            fields.useEitherFast(infos)(onField(cur)) match
                case Left(err)    => Left(err)
                case Right(tuple) => Right(mirror.fromProduct(tuple))

        override def decodeAccumulating(cur: HCursor): Decoder.AccumulatingResult[A] =
            fields.useEithers(infos)(onField(cur)) match
                case Left(head +: rest) => Validated.Invalid(NonEmptyList(head, rest.toList))
                case Left(_)            => Validated.invalidNel(DecodingFailure("unknown error", Nil))
                case Right(tuple)       => Validated.Valid(mirror.fromProduct(tuple))

    def sum[A](using mirror: Mirror.SumOf[A])(
        subs: LazySummon.All[Decoder, mirror.MirroredElemTypes],
        mkSubMap: => Map[String, Decoder[A]],
    )(using config: => Config[A], matching: Matching[A]): EvoDecoder[A] = new:
        lazy val cfg                       = config
        lazy val subDecoders               = mkSubMap
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
                                         .toFailure(s"Internal error: should not happen, could not found $subName constructor info")
                result            <- sub.tryDecode(down)
            yield result

    def newtype[A](using nt: ValueClass[A])(using enc: Decoder[nt.Representation]): EvoDecoder[A] =
        c => c.as[nt.Representation].map(nt.from)

    extension [A](oa: Option[A])
        private def toFailure(s: => String): Decoder.Result[A] = oa.toRight(DecodingFailure(s, Nil))

    private def constName(cur: HCursor, discriminator: Option[String]): Decoder.Result[(String, ACursor)] =
        discriminator match
            case Some(field) => for (sub <- cur.get[String](field)) yield (sub, cur)
            case None        =>
                for sub <- cur.keys
                               .collect { case it if it.size == 1 => it.head }
                               .toFailure("expecting an object with a single key")
                yield (sub, cur.downField(sub))
end EvoDecoder
