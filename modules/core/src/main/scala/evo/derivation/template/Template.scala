package evo.derivation.template

import scala.deriving.Mirror
import scala.compiletime.{summonInline, summonFrom}
import evo.derivation.LazySummon
import evo.derivation.config.Config
import evo.derivation.internal.{Matching, mirroredNames, underiveableError}
import evo.derivation.ValueClass
import evo.derivation.LazySummon.LazySummonByConfig

/** template for simple instance derivation you can mix this trait into your companion object and get relatively easy
  * derivation template define your types, implement product, sum and newtype
  *
  * see EvoDecoder as an example
  *
  * ATTENTION: to be able to derive automaticall instances for inner cases of enums you should add something like this
  * {{{
  *     inline given [A: Mirror.ProductOf]: LazySummonByConfig[YourTypeclass, A] = lazySummonForProduct
  * }}}
  */
trait Template:
    /** resulting typeclass, usually equvalent to the type, your companion is bound
      */
    type Provide[A]

    /** requirement typeclass for the each of the object fields
      */
    type OfField[A]

    /** requirement typeclass for the each of the enum subtypes
      */
    type OfSubtype[A]

    /** requirement typeclass for base type wrapped in new type or value class
      */
    type OfNewtype[A]

    /** called for the case classes derivation
      */
    def product[A](using mirror: Mirror.ProductOf[A])(
        fields: LazySummon.All[OfField, mirror.MirroredElemTypes],
    )(using => Config[A], A <:< Product): Provide[A]

    /** called for the sealed trait \ enum derivation
      *
      * WARNING: mkSubMap makes assumption that you can use your subtype instances as instances for the target type, use
      * with great caution!
      */
    def sum[A](using mirror: Mirror.SumOf[A])(
        subs: LazySummon.All[OfSubtype, mirror.MirroredElemTypes],
        mkSubMap: => Map[String, OfSubtype[A]],
    )(using config: => Config[A], matching: Matching[A]): Provide[A]

    /** called for the newtype \ value class derivation
      */
    def newtype[A](using nt: ValueClass[A])(using enc: OfNewtype[nt.Representation]): Provide[A]

    /** this is the method that would be called when the derivation occurs you can redefine it to add some custom logic
      */
    inline def derived[A](using config: => Config[A]): Provide[A] =
        summonFrom {
            case mirror: Mirror.ProductOf[A] => deriveForProduct[A]
            case given Mirror.SumOf[A]       => deriveForSum[A, A]
            case given ValueClass[A]         => deriveForNewtype[A]
            case _                           => underiveableError[Provide[A], A]
        }

    inline def lazySummonForProduct[From, A: Mirror.ProductOf]: LazySummonByConfig[Provide, From, A] =
        new:
            def instance(using => Config[A]): Provide[A] = deriveForProduct[A]

    inline def lazySummonForSum[From, A: Mirror.SumOf]: LazySummonByConfig[Provide, From, A] =
        new:
            def instance(using => Config[A]): Provide[A] = deriveForSum[From, A]

    private[template] inline def deriveForSum[From, A](using
        config: => Config[A],
        mirror: Mirror.SumOf[A],
    ): Provide[A] =
        given Matching[A] = Matching.create[A]

        val fieldInstances =
            LazySummon.all[mirror.MirroredElemLabels, From, OfSubtype, Provide, mirror.MirroredElemTypes]

        val names = mirroredNames[A]

        sum[A](fieldInstances, fieldInstances.toMap(names))
    end deriveForSum

    private[template] inline def deriveForProduct[A](using
        config: => Config[A],
        mirror: Mirror.ProductOf[A],
    ): Provide[A] =
        type F[A] = OfField[A]

        val fieldInstances =
            LazySummon.all[mirror.MirroredElemLabels, A, F, Provide, mirror.MirroredElemTypes]

        product[A](fieldInstances)(using config, summonInline)
    end deriveForProduct

    private[template] inline def deriveForNewtype[A](using nt: ValueClass[A]): Provide[A] =
        val encoder: OfNewtype[nt.Representation] = summonInline

        newtype[A](using nt)(using encoder)

end Template

/** simplified trait, useful when your require the same typeclass in all cases
  */
trait ConsistentTemplate[TC[_], Prov[_]] extends Template:
    type OfField[A]   = TC[A]
    type OfSubtype[A] = TC[A]
    type OfNewtype[A] = TC[A]
    type Provide[A]   = Prov[A]
end ConsistentTemplate

/** event more simplified trait, useful when your define derivation for your own typeclass
  */
trait HomogenicTemplate[TC[_]] extends ConsistentTemplate[TC, TC]

trait SummonForProduct extends Template:
    inline given [From, A: Mirror.ProductOf]: LazySummonByConfig[Provide, From, A] = lazySummonForProduct

trait SummonForSubtype extends Template:
    inline given [From, A: Mirror.SumOf]: LazySummonByConfig[Provide, From, A] = lazySummonForSum

trait SummonHierarchy extends SummonForProduct with SummonForSubtype
