package evo.derivation.play.json

import scala.compiletime.{erasedValue, summonFrom, summonInline}
import evo.derivation.internal.underiveableError

import scala.deriving.Mirror
import evo.derivation.LazySummon.{All, LazySummonByConfig, LazySummonByInstance, useCollect}
import evo.derivation.LazySummon
import evo.derivation.internal.tupleFromProduct
import EvoWrites.SumWrites
import evo.derivation.internal.Matching
import evo.derivation.internal.mirroredNames
import evo.derivation.ValueClass
import evo.derivation.config.{Config, ForField}
import evo.derivation.template.{ConsistentTemplate, SummonForProduct}
import play.api.libs.json.*
import play.api.libs.json.Writes
import evo.derivation.template.SummonHierarchy

trait EvoWrites[A] extends Writes[A]

object EvoWrites extends ConsistentTemplate[Writes, EvoWrites] with SummonHierarchy:

    override def product[A](using mirror: Mirror.ProductOf[A])(
        fields: All[Writes, mirror.MirroredElemTypes],
    )(using => Config[A], A <:< Product): EvoWrites[A] =
        ProductWrites(fields)

    /** called for the sealed trait \ enum derivation
      */
    override def sum[A](using mirror: Mirror.SumOf[A])(
        subs: All[Writes, mirror.MirroredElemTypes],
        mkSubMap: => Map[String, Writes[A]],
    )(using config: => Config[A], matching: Matching[A]): EvoWrites[A] =
        SumWrites(mkSubMap)

    /** called for the newtype \ value class derivation
      */
    override def newtype[A](using nt: ValueClass[A])(using write: Writes[nt.Representation]): EvoWrites[A] =
        o => write.writes(nt.to(o))

    class ProductWrites[A](using mirror: Mirror.ProductOf[A], config: => Config[A])(using A <:< Product)(
        fieldInstances: LazySummon.All[Writes, mirror.MirroredElemTypes],
    ) extends EvoWrites[A]:

        private def encodeField(info: ForField[_ <: A], json: JsValue): Vector[(String, JsValue)] =
            json.validate[JsObject].asOpt match
                case Some(obj) if info.embed => obj.value.toVector
                case _                       => Vector(info.name -> json)

        lazy val infos = config.top.fields.map(_._2)

        override def writes(a: A): JsValue =

            val fields = tupleFromProduct(a)
            type Res = Vector[(String, JsValue)]
            val results = fieldInstances.useCollect[Res, ForField[_ <: A]](fields, infos)(
              [X] => (info: ForField[_ <: A], a: X, enc: Writes[X]) => encodeField(info, enc.writes(a)),
            )
            JsObject(results.flatten)
        end writes
    end ProductWrites

    class SumWrites[A](
        mkSubWritess: => Map[String, Writes[A]],
    )(using config: => Config[A], mirror: Mirror.SumOf[A], matching: Matching[A])
        extends EvoWrites[A]:
        lazy val cfg      = config
        lazy val encoders = mkSubWritess

        override def writes(a: A): JsValue =
            val constructor  = matching.matched(a)
            val discrimValue = cfg.name(constructor)
            val json         = encoders.get(constructor).fold[JsValue](JsObject.empty)(_.writes(a))

            cfg.discriminator.zip(json.validate[JsObject].asOpt) match
                case Some(field -> obj) =>
                    obj + (field -> JsString(discrimValue))
                case None               =>
                    JsObject.apply(Seq(discrimValue -> json))
            end match
        end writes
    end SumWrites
end EvoWrites
