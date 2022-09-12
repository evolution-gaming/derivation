package evo.derivation.cats

import evo.derivation.LazySummon.LazySummonByConfig
import evo.derivation.cats.EvoEq.Requirement
import evo.derivation.{LazySummon, ValueClass}
import evo.derivation.config.Config
import evo.derivation.internal.{Matching, mirroredNames, underiveableError}

import scala.compiletime.{summonFrom, summonInline}
import scala.deriving.Mirror

/** A typeclass providing both cats.Eq and scala.Equiv instances requiring any of them for each of subordinates */
trait EvoEq[A] extends Requirement[A] with cats.kernel.Eq[A] with Equiv[A]:
    def equiv(x: A, y: A): Boolean = eqv(x, y)

object EvoEq:

    trait Requirement[A]:
        def eqv(x: A, y: A): Boolean

    object Requirement:
        inline given [A]: Requirement[A] =
            summonFrom {
                case byCats: cats.kernel.Eq[A] =>
                    new:
                        def eqv(a: A, b: A) = byCats.eqv(a, b)
                case byScala: Equiv[A]         =>
                    new:
                        def eqv(a: A, b: A) = byScala.equiv(a, b)
            }
    end Requirement

    inline def derived[A](using cfg: Config[A]): EvoEq[A] =
        summonFrom {
            case mirror: Mirror.ProductOf[A] => deriveForProduct[A].instance
            case given Mirror.SumOf[A]       => deriveForSum[A]
            case given ValueClass[A]         => deriveForValueClass[A]
            case _                           => underiveableError[EvoEq[A], A]
        }

    inline def deriveForProduct[A](using
        mirror: Mirror.ProductOf[A],
    ): LazySummonByConfig[EvoEq, A] =
        val fieldInstances =
            LazySummon.all[mirror.MirroredElemLabels, A, Requirement, EvoEq, mirror.MirroredElemTypes]
        ProductEq[A](mirror)(using summonInline[A <:< Product])(fieldInstances)

    end deriveForProduct

    private[cats] inline def deriveForSum[A](using
        config: => Config[A],
        mirror: Mirror.SumOf[A],
    ): EvoEq[A] =
        given Matching[A] = Matching.create[A]

        val fieldInstances =
            LazySummon.all[mirror.MirroredElemLabels, A, Requirement, EvoEq, mirror.MirroredElemTypes]

        val names = mirroredNames[A]

        new SumEq[A](fieldInstances.toMap(names))
    end deriveForSum

    private inline def deriveForValueClass[A](using nt: ValueClass[A]): EvoEq[A] =
        val requirement: Requirement[nt.Representation] = summonInline

        ValueClasEq[A](using nt)(using requirement)

    class ProductEq[A](mirror: Mirror.ProductOf[A])(using A <:< Product)(
        instances: LazySummon.All[Requirement, mirror.MirroredElemTypes],
    ) extends LazySummonByConfig[EvoEq, A]:
        def instance(using => Config[A]): EvoEq[A] = new:
            lazy val stricts = instances.toVector[Any]

            def eqv(x: A, y: A): Boolean =
                (0 until stricts.length).forall(i => stricts(i).eqv(x.productElement(i), y.productElement(i)))

    end ProductEq

    class SumEq[A](
        mkSubEncoders: => Map[String, Requirement[A]],
    )(using config: => Config[A], mirror: Mirror.SumOf[A], matching: Matching[A])
        extends EvoEq[A]:

        lazy val subs                = mkSubEncoders
        def eqv(x: A, y: A): Boolean =
            val xname = matching.matched(x)
            matching.matched(y) == xname && {
                subs(xname).eqv(x, y)
            }
    end SumEq

    class ValueClasEq[A](using nt: ValueClass[A])(using reprEq: Requirement[nt.Representation]) extends EvoEq[A]:
        def eqv(x: A, y: A): Boolean = reprEq.eqv(nt.to(x), nt.to(y))

end EvoEq
