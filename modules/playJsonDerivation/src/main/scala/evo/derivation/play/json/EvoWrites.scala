package evo.derivation.play.json

import evo.derivation.Config
import scala.compiletime.{summonFrom, erasedValue, summonInline}
import evo.derivation.internal.underiveableError
import scala.deriving.Mirror
import evo.derivation.LazySummon.LazySummonByInstance
import evo.derivation.LazySummon.LazySummonByConfig
import evo.derivation.LazySummon
import evo.derivation.LazySummon.useCollect

import evo.derivation.internal.tupleFromProduct

import evo.derivation.Config.FieldInfo

import EvoWrites.SumWrites
import evo.derivation.internal.Matching
import evo.derivation.internal.mirroredNames
import evo.derivation.ValueClass
import play.api.libs.json.*
import play.api.libs.json.Writes

trait EvoWrites[A] extends Writes[A]

object EvoWrites:
    inline def derived[A](using config: => Config[A]): EvoWrites[A] =
        summonFrom {
            case mirror: Mirror.ProductOf[A] => deriveForProduct[A].instance
            case given Mirror.SumOf[A]       => deriveForSum[A]
            case given ValueClass[A]         => deriveForNewtype[A]
            case _                           => underiveableError[EvoWrites[A], A]
        }

    inline given [A: Mirror.ProductOf]: LazySummonByConfig[EvoWrites, A] = deriveForProduct[A]

    private[json] inline def deriveForSum[A](using
        config: => Config[A],
        mirror: Mirror.SumOf[A],
    ): EvoWrites[A] =
        given Matching[A] = Matching.create[A]

        val fieldInstances =
            LazySummon.all[mirror.MirroredElemLabels, A, Writes, EvoWrites, mirror.MirroredElemTypes]

        val names = mirroredNames[A]

        new SumWrites[A](fieldInstances.toMap(names))

    private[json] inline def deriveForProduct[A](using
        mirror: Mirror.ProductOf[A],
    ): LazySummonByConfig[EvoWrites, A] =
        val fieldInstances =
            LazySummon.all[mirror.MirroredElemLabels, A, Writes, EvoWrites, mirror.MirroredElemTypes]

        ProductWritesMake[A](using mirror)(using summonInline[A <:< Product])(fieldInstances)

    private inline def deriveForNewtype[A](using nt: ValueClass[A]): EvoWrites[A] =
        given Writes[nt.Representation] = summonInline

        NewtypeWrites[A]()

    class ProductWritesMake[A](using mirror: Mirror.ProductOf[A])(using A <:< Product)(
        fieldInstances: LazySummon.All[Writes, mirror.MirroredElemTypes],
    ) extends LazySummonByConfig[EvoWrites, A]:

        private def encodeField(info: FieldInfo, json: JsValue): Vector[(String, JsValue)] =
            json.validate[JsObject].asOpt match
                case Some(obj) if info.embed => obj.value.toVector
                case _                       => Vector(info.name -> json)

        def instance(using config: => Config[A]): EvoWrites[A] =
            lazy val infos = config.top.fieldInfos
            a =>
                val fields = tupleFromProduct(a)
                type Res = Vector[(String, JsValue)]
                val results = fieldInstances.useCollect[Res, FieldInfo](fields, infos)(
                  [X] => (info: FieldInfo, a: X, enc: Writes[X]) => encodeField(info, enc.writes(a)),
                )
                JsObject(results.flatten)

    end ProductWritesMake

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
    end SumWrites

    class NewtypeWrites[A](using nt: ValueClass[A])(using enc: Writes[nt.Representation]) extends EvoWrites[A]:
        def writes(o: A): JsValue = enc.writes(nt.to(o))

end EvoWrites
