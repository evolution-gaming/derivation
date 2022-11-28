package evo.derivation.circe

import scala.deriving.Mirror

import evo.derivation.LazySummon.LazySummonByConfig
import evo.derivation.config.{Config, ForField}
import evo.derivation.internal.{Matching, tupleFromProduct}
import evo.derivation.template.Template
import evo.derivation.{LazySummon, ValueClass}
import io.circe.{Encoder, Json, JsonObject}
import evo.derivation.template.SummonForProduct

trait EvoEncoder[A] extends Encoder[A]

abstract class EvoTemplateEncoder extends Template:
    type OfNewtype[A] >: Encoder.AsObject[A] <: Encoder[A]
    type OfField[A]   = Encoder[A]
    type OfSubtype[A] = Encoder[A]

    type Provide[A] >: EvoObjectEncoder[A] <: EvoEncoder[A]

    def product[A](using mirror: Mirror.ProductOf[A])(
        fieldInstances: LazySummon.All[Encoder, mirror.MirroredElemTypes],
    )(using config: => Config[A], ev: A <:< Product): EvoObjectEncoder[A] = new:
        lazy val infos = config.top.fields.map(_._2)

        private def encodeField(info: ForField[_], json: Json): Vector[(String, Json)] =
            json.asObject match
                case Some(obj) if info.embed => obj.toVector
                case _                       => Vector(info.name -> json)

        def encodeObject(a: A): JsonObject =
            val fields = tupleFromProduct(a)
            type Res = Vector[(String, Json)]
            val results = fieldInstances.useCollect[Res, ForField[_]](fields, infos)(
              [X] => (info: ForField[_], a: X, enc: Encoder[X]) => encodeField(info, enc(a)),
            )
            JsonObject.fromIterable(results.flatten)
        end encodeObject

    def sum[A](using mirror: Mirror.SumOf[A])(
        subs: LazySummon.All[Encoder, mirror.MirroredElemTypes],
        mkEncoders: => Map[String, Encoder[A]],
    )(using config: => Config[A], matching: Matching[A]): EvoObjectEncoder[A] = new:
        lazy val cfg                               = config
        lazy val encoders: Map[String, Encoder[A]] = mkEncoders

        override def encodeObject(a: A): JsonObject =
            val constructor = matching.matched(a)

            val discrimValue = cfg.name(constructor)

            val json = encoders.get(constructor).fold(Json.fromJsonObject(JsonObject.empty))(_.apply(a))

            cfg.discriminator.zip(json.asObject) match
                case Some(field -> obj) =>
                    (field -> Json.fromString(discrimValue)) +: obj
                case None               =>
                    JsonObject.singleton(discrimValue, json)
            end match
        end encodeObject
end EvoTemplateEncoder

object EvoEncoder extends EvoTemplateEncoder with SummonForProduct:
    final type OfNewtype[A] = Encoder[A]

    final type Provide[A] = EvoEncoder[A]

    def newtype[A](using nt: ValueClass[A])(using enc: Encoder[nt.Representation]): EvoEncoder[A] =
        a => enc(nt.to(a))

end EvoEncoder

trait EvoObjectEncoder[A] extends EvoEncoder[A] with Encoder.AsObject[A]

object EvoObjectEncoder extends EvoTemplateEncoder with SummonForProduct:
    final type OfNewtype[A] = Encoder.AsObject[A]

    final type Provide[A] = EvoObjectEncoder[A]

    def newtype[A](using nt: ValueClass[A])(using enc: Encoder.AsObject[nt.Representation]): EvoObjectEncoder[A] =
        a => enc.encodeObject(nt.to(a))

end EvoObjectEncoder
