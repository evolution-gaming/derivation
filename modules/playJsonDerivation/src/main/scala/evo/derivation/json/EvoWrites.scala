package evo.derivation.json


import evo.derivation.Config
import scala.compiletime.{summonFrom, erasedValue, summonInline}
import evo.derivation.internal.underiveableError
import scala.deriving.Mirror
import evo.derivation.LazySummon.LazySummonByInstance
import evo.derivation.LazySummon.LazySummonByConfig
import evo.derivation.LazySummon

import evo.derivation.internal.tupleFromProduct

import evo.derivation.Config.FieldInfo

import EvoWrites.SumEncoder
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

    private[circe] inline def deriveForSum[A](using
        config: => Config[A],
        mirror: Mirror.SumOf[A],
    ): EvoObjectEncoder[A] =
        given Matching[A] = Matching.create[A]

        val fieldInstances =
            LazySummon.all[mirror.MirroredElemLabels, A, Encoder, EvoWrites, mirror.MirroredElemTypes]

        val names = mirroredNames[A]

        new SumEncoder[A](fieldInstances.toMap(names))

    private[circe] inline def deriveForProduct[A](using
        mirror: Mirror.ProductOf[A],
    ): LazySummonByConfig[EvoObjectEncoder, A] =
        val fieldInstances =
            LazySummon.all[mirror.MirroredElemLabels, A, Encoder, EvoWrites, mirror.MirroredElemTypes]

        ProductEncoder[A](using mirror)(using summonInline[A <:< Product])(fieldInstances)

    private inline def deriveForNewtype[A](using nt: ValueClass[A]): EvoWrites[A] =
        given Encoder[nt.Representation] = summonInline

        NewtypeEncoder[A]()

    class ProductEncoder[A](using mirror: Mirror.ProductOf[A])(using A <:< Product)(
        fieldInstances: LazySummon.All[Encoder, mirror.MirroredElemTypes],
    ) extends LazySummonByConfig[EvoObjectEncoder, A]:

        private def encodeField(info: FieldInfo, json: Json): Vector[(String, Json)] =
            json.asObject match
                case Some(obj) if info.embed => obj.toVector
                case _                       => Vector(info.name -> json)

        def instance(using config: => Config[A]): EvoObjectEncoder[A] =
            lazy val infos = config.top.fieldInfos
            a =>
                val fields = tupleFromProduct(a)
                type Res = Vector[(String, Json)]
                val results = fieldInstances.useCollect[Res, FieldInfo](fields, infos)(
                  [X] => (info: FieldInfo, a: X, enc: Encoder[X]) => encodeField(info, enc(a)),
                )
                JsonObject.fromIterable(results.flatten)

    end ProductEncoder

    class SumEncoder[A](
        mkSubEncoders: => Map[String, Encoder[A]],
    )(using config: => Config[A], mirror: Mirror.SumOf[A], matching: Matching[A])
        extends EvoObjectEncoder[A]:
        lazy val cfg      = config
        lazy val encoders = mkSubEncoders

        override def encodeObject(a: A): JsonObject =
            val constructor = matching.matched(a)
            val prod        = cfg.constructor(constructor).top

            val discrimValue = cfg.name(constructor)

            val json = encoders.get(constructor).fold(Json.fromJsonObject(JsonObject.empty))(_.apply(a))

            cfg.discriminator.zip(json.asObject) match
                case Some(field -> obj) =>
                    (field -> Json.fromString(discrimValue)) +: obj
                case None               =>
                    JsonObject.singleton(discrimValue, json)

    end SumEncoder

    class NewtypeEncoder[A](using nt: ValueClass[A])(using enc: Encoder[nt.Representation]) extends EvoWrites[A]:
        def apply(a: A): Json = enc(nt.to(a))

end EvoWrites

trait EvoObjectEncoder[A] extends EvoWrites[A] with Encoder.AsObject[A]

object EvoObjectEncoder:
    inline def derived[A](using config: => Config[A]): EvoObjectEncoder[A] =
        summonFrom {
            case mirror: Mirror.ProductOf[A] => EvoWrites.deriveForProduct[A].instance
            case given Mirror.SumOf[A]       => EvoWrites.deriveForSum[A]
            case given ValueClass[A]         => deriveForNewtype[A]
            case _                           => underiveableError[EvoWrites[A], A]
        }

    private inline def deriveForNewtype[A](using nt: ValueClass[A]): EvoObjectEncoder[A] =
        given Encoder.AsObject[nt.Representation] = summonInline

        NewtypeEncoder[A]()

    class NewtypeEncoder[A](using nt: ValueClass[A])(using enc: Encoder.AsObject[nt.Representation])
        extends EvoObjectEncoder[A]:
        def encodeObject(a: A): JsonObject = enc.encodeObject(nt.to(a))
