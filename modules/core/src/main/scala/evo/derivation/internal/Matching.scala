package evo.derivation.internal

import scala.deriving.Mirror
import scala.compiletime.{constValueTuple, summonAll}
import scala.reflect.ClassTag
import scala.reflect.TypeTest

trait Matching[A]:
    def matched(a: A): String

object Matching:
    inline def create[A](using mirror: Mirror.SumOf[A]): Matching[A] =
        val names    = constValueTuple[mirror.MirroredElemLabels].toIArray
        val matchers = summonAll[Tuple.Map[mirror.MirroredElemTypes, [b] =>> TypeTest[A, b]]].toIArray

        a => find(matchers, names, a)

    private def find[A](matchers: IArray[Any], names: IArray[Any], a: A): String =
        def go(i: Int): String =
            if i >= matchers.length || i >= names.length then ""
            else if matchers(i).asInstanceOf[TypeTest[A, A]].unapply(a).isDefined then names(i).asInstanceOf[String]
            else go(i + 1)
        go(0)
    end find
end Matching
