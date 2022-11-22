package evo.derivation

import sttp.tapir.Schema
import evo.derivation.template.ConsistentTemplate
import evo.derivation.template.SummonForProduct
import scala.deriving.Mirror
import evo.derivation.config.Config
import evo.derivation.internal.Matching

trait EvoSchema[A] extends Schema[A]

object EvoSchema extends ConsistentTemplate[Schema, EvoSchema] with SummonForProduct:
    // /** called for the case classes derivation
    //   */
    // def product[A](using mirror: Mirror.ProductOf[A])(
    //     fields: LazySummon.All[OfField, mirror.MirroredElemTypes],
    // )(using => Config[A], A <:< Product): Provide[A] = ???

    // /** called for the sealed trait \ enum derivation
    //   *
    //   * WARNING: mkSubMap makes assumption that you can use your subtype instances as instances for the target type, use
    //   * with great caution!
    //   */
    // def sum[A](using mirror: Mirror.SumOf[A])(
    //     subs: LazySummon.All[OfSubtype, mirror.MirroredElemTypes],
    //     mkSubMap: => Map[String, OfSubtype[A]],
    // )(using config: => Config[A], matching: Matching[A]): Provide[A] = ???

    // /** called for the newtype \ value class derivation
    //   */
    // def newtype[A](using nt: ValueClass[A])(using enc: OfNewtype[nt.Representation]): Provide[A] = ???

end EvoSchema
