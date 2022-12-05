package evo.derivation.play.json

import scala.deriving.Mirror
import scala.compiletime.*
import evo.derivation.internal.underiveableError

import scala.collection.immutable.Iterable
import evo.derivation.LazySummon
import evo.derivation.LazySummon.LazySummonByConfig
import LazySummon.useEitherFast

import java.util.Arrays
import evo.derivation.internal.mirroredNames
import evo.derivation.ValueClass
import evo.derivation.config.{Config, ForField}
import play.api.libs.json.{
    JsError,
    JsNull,
    JsObject,
    JsPath,
    JsResult,
    JsSuccess,
    JsValue,
    Json,
    JsonValidationError,
    Reads,
}

import evo.derivation.template.ConsistentTemplate

import scala.collection.Seq
import scala.util.Either
import evo.derivation.internal.Matching
import evo.derivation.template.SummonForProduct
import evo.derivation.template.SummonHierarchy

trait EvoReads[A] extends Reads[A]

object EvoReads extends ConsistentTemplate[Reads, EvoReads] with SummonHierarchy:

    def newtype[A](using nt: ValueClass[A])(using reads: Reads[nt.Representation]): EvoReads[A] =
        json => json.validate[nt.Representation].map(nt.from)

    def product[A](using mirror: Mirror.ProductOf[A])(
        fields: LazySummon.All[Reads, mirror.MirroredElemTypes],
    )(using => Config[A], A <:< Product): EvoReads[A] = new ProductReadsMake(fields)

    def sum[A](using mirror: Mirror.SumOf[A])(
        subs: LazySummon.All[Reads, mirror.MirroredElemTypes],
        mkSubMap: => Map[String, Reads[A]],
    )(using config: => Config[A], matching: Matching[A]): EvoReads[A] =
        new SumReads(mkSubMap)

    extension [A](oa: Option[A]) private def toFailure(s: => String): JsResult[A] = oa.fold(JsError(s))(JsSuccess(_))

    private def constName(json: JsValue, discriminator: Option[String]): JsResult[(String, JsValue)] =
        discriminator match
            case Some(field) =>
                for {
                    value <- (json \ field).validate[JsValue]
                    str   <- value.validate[String]
                } yield (str, json)
            case None        =>
                for {
                    obj    <- json.validate[JsObject]
                    keys    = obj.keys
                    result <- if keys.size == 1 then JsSuccess(keys.head)
                              else JsError("Expecting an object with a single key")
                } yield (result, obj.apply(result))

    class ProductReadsMake[A](using mirror: Mirror.ProductOf[A])(
        fieldInstances: LazySummon.All[Reads, mirror.MirroredElemTypes],
    )(using config: => Config[A])
        extends EvoReads[A]:

        lazy val infos = IArray(config.top.fields.map(_._2)*)

        private def onField(
            json: JsValue,
        )(
            decoder: LazySummon.Of[Reads],
            info: ForField[_],
        ): Either[Seq[(JsPath, Seq[JsonValidationError])], decoder.FieldType] =
            val js = if info.embed then json else (json \ info.name).getOrElse(JsNull)
            decoder.use(js.validate[decoder.FieldType].asEither)
        end onField

        override def reads(json: JsValue): JsResult[A] =
            fieldInstances.useEitherFast[ForField[_], Seq[(JsPath, Seq[JsonValidationError])]](infos)(
              onField(json),
            ) match
                case Left(err)    => JsError(err)
                case Right(tuple) => JsSuccess(mirror.fromProduct(tuple))
    end ProductReadsMake

    class SumReads[A](using config: => Config[A], mirror: Mirror.SumOf[A])(mkSubDecoders: => Map[String, Reads[A]])
        extends EvoReads[A]:

        lazy val cfg                                   = config
        lazy val subDecoders                           = mkSubDecoders
        lazy val all                                   = cfg.constrFromRenamed.keys.mkString(", ")
        override def reads(json: JsValue): JsResult[A] =
            for
                subDown              <- constName(json, cfg.discriminator)
                (discriminator, down) = subDown
                subName              <- cfg.constrFromRenamed
                                            .get(discriminator)
                                            .toFailure(s"Constructor $discriminator not found; expected one of:\n $all")
                sub                  <-
                    subDecoders
                        .get(subName)
                        .toFailure(
                          s"Internal error: could not found $subName constructor info.\n This is 99% a bug, contact library authors",
                        )
                result               <- sub.reads(down)
            yield result
    end SumReads
end EvoReads
