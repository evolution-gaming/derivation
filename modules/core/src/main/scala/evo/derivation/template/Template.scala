package evo.derivation.template

import scala.deriving.Mirror
import scala.compiletime.{summonInline, summonFrom}
import evo.derivation.LazySummon
import Template.Typeclass
import evo.derivation.config.Config
import evo.derivation.internal.{Matching, mirroredNames, underiveableError}
import evo.derivation.ValueClass
import evo.derivation.LazySummon.LazySummonByConfig

/** a template for simple instance derivation */
trait Template:
    type Provide[A]
    type OfField[A]
    type OfSubtype[A]
    type OfNewtype[A]

    def product[A](using mirror: Mirror.ProductOf[A])(
        names: Vector[String],
        fields: LazySummon.All[OfField, mirror.MirroredElemTypes],
    )(using => Config[A], A <:< Product): Provide[A]

    def sum[A](using mirror: Mirror.SumOf[A])(
        names: Vector[String],
        subs: LazySummon.All[OfSubtype, mirror.MirroredElemTypes],
        mkSubMap: => Map[String, OfSubtype[A]],
    )(using config: => Config[A], matching: Matching[A]): Provide[A]

    def newtype[A](using nt: ValueClass[A])(using enc: OfNewtype[nt.Representation]): Provide[A]

    inline def derived[A](using config: => Config[A]): Provide[A] =
        summonFrom {
            case mirror: Mirror.ProductOf[A] => deriveForProduct[A]
            case given Mirror.SumOf[A]       => deriveForSum[A]
            case given ValueClass[A]         => deriveForNewtype[A]
            case _                           => underiveableError[Provide[A], A]
        }

    inline def lazySummonForProduct[A: Mirror.ProductOf]: LazySummonByConfig[Provide, A] =
        new:
            def instance(using => Config[A]): Provide[A] = deriveForProduct[A]

    private[template] inline def deriveForSum[A](using
        config: => Config[A],
        mirror: Mirror.SumOf[A],
    ): Provide[A] =
        given Matching[A] = Matching.create[A]

        val fieldInstances =
            LazySummon.all[mirror.MirroredElemLabels, A, OfSubtype, Provide, mirror.MirroredElemTypes]

        val names = mirroredNames[A]

        sum[A](names, fieldInstances, fieldInstances.toMap(names))
    end deriveForSum

    private[template] inline def deriveForProduct[A](using
        config: => Config[A],
        mirror: Mirror.ProductOf[A],
    ): Provide[A] =
        type F[A] = OfField[A]    

        val fieldInstances =
            LazySummon.all[mirror.MirroredElemLabels, A, F, Provide, mirror.MirroredElemTypes]

        product[A](mirroredNames[A], fieldInstances)(using config, summonInline)
    end deriveForProduct

    private[template] inline def deriveForNewtype[A](using nt: ValueClass[A]): Provide[A] =
        val encoder: OfNewtype[nt.Representation] = summonInline

        newtype[A](using nt)(using encoder)

end Template

object Template:
    type Typeclass
    type FieldTC <: Typeclass
    type SubtypeTC <: Typeclass
    type NewtypeTC <: Typeclass
    type ProvideTC <: Typeclass

end Template
