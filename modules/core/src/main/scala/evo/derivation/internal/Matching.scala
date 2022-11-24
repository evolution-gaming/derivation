package evo.derivation.internal

import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.ClassTag
import scala.reflect.TypeTest
import scala.quoted.*

private inline def summonNamesRecursively[T <: Tuple]: List[String] = inline erasedValue[T] match
    case _: (t *: ts)  =>
        summonFrom {
            case mirror: Mirror.SumOf[`t`]     =>
                summonNamesRecursively[mirror.MirroredElemTypes] ::: summonNamesRecursively[ts]
            case mirror: Mirror.ProductOf[`t`] =>
                constValue[mirror.MirroredLabel] :: summonNamesRecursively[ts]
        }
    case _: EmptyTuple => Nil

private inline def summonMatchersRecursively[A, T <: Tuple]: List[TypeTest[A, ?]] = inline erasedValue[T] match
    case _: (t *: ts)  =>
        summonFrom {
            case mirror: Mirror.SumOf[`t`]     =>
                summonMatchersRecursively[A, mirror.MirroredElemTypes] ::: summonMatchersRecursively[A, ts]
            case mirror: Mirror.ProductOf[`t`] =>
                summon[TypeTest[A, t]] :: summonMatchersRecursively[A, ts]
        }
    case _: EmptyTuple => Nil

trait Matching[A]:
    def matched(a: A): String

object Matching:
    inline def create[A](using mirror: Mirror.SumOf[A]): Matching[A] =
        val names    = IArray.from(summonNamesRecursively[mirror.MirroredElemTypes])
        val matchers = IArray.from(summonMatchersRecursively[A, mirror.MirroredElemTypes])

        a => find(matchers, names, a)

    private def find[A](matchers: IArray[Any], names: IArray[Any], a: A): String =
        def go(i: Int): String =
            if i >= matchers.length || i >= names.length then ""
            else if matchers(i).asInstanceOf[TypeTest[A, A]].unapply(a).isDefined then names(i).asInstanceOf[String]
            else go(i + 1)
        go(0)
    end find
end Matching
