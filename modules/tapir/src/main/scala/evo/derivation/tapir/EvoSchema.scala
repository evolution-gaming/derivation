package evo.derivation
package tapir

import sttp.tapir.Schema
import evo.derivation.template.SummonForProduct
import scala.deriving.Mirror
import evo.derivation.config.Config
import evo.derivation.internal.Matching
import evo.derivation.template.Template
import sttp.tapir.SchemaType.SProduct
import evo.derivation.config.ForField
import sttp.tapir.SchemaType.SProductField
import sttp.tapir.FieldName

object EvoSchema extends Template with SummonForProduct:
    opaque type Provide[A] <: Schema[A] = Schema[A]
    type OfField[A]                     = Schema[A]
    type OfSubtype[A]                   = Schema[A]
    type OfNewtype[A]                   = Schema[A]

    /** called for the case classes derivation
      */
    def product[A](using mirror: Mirror.ProductOf[A])(
        fields: LazySummon.All[Schema, mirror.MirroredElemTypes],
    )(using cfg: => Config[A], ev: A <:< Product): Provide[A] =
        lazy val infos = cfg.top.fields
        Schema(
          schemaType = SProduct(
            fields
                .useOnFields(cfg.top) {
                    [a] =>
                        (schema: Schema[a], label: String, field: ForField) =>
                            SProductField[A, a](FieldName(label, field.name), schema, t => ???)
                }
                .toList,
          ),
        )
    end product

    /** called for the sealed trait \ enum derivation
      *
      * WARNING: mkSubMap makes assumption that you can use your subtype instances as instances for the target type, use
      * with great caution!
      */
    def sum[A](using mirror: Mirror.SumOf[A])(
        subs: LazySummon.All[Schema, mirror.MirroredElemTypes],
        mkSubMap: => Map[String, Schema[A]],
    )(using config: => Config[A], matching: Matching[A]): Provide[A] = ???

    /** called for the newtype \ value class derivation
      */
    def newtype[A](using nt: ValueClass[A])(using repr: Schema[nt.Representation]): Provide[A] =
        repr.map(r => Some(nt.from(r)))(nt.to)
end EvoSchema
