package evo.derivation.internal

import scala.deriving.Mirror
import scala.compiletime.{constValueTuple, summonAll}
import scala.reflect.ClassTag

trait Matching[A]:
    def matched(a: A): String

object Matching:
    inline def create[A](using mirror: Mirror.SumOf[A]): Matching[A] =
        val names    = constValueTuple[mirror.MirroredElemLabels].toIArray
        val matchers = summonAll[Tuple.Map[mirror.MirroredElemTypes, Matcher]].toIArray

        a => find(matchers, names, a)

    private def find(matchers: IArray[Any], names: IArray[Any], a: Any): String =
        def go(i: Int): String =
            if i >= matchers.length || i >= names.length then ""
            else if matchers(i).asInstanceOf[Any => Boolean](a) then names(i).asInstanceOf[String]
            else go(i + 1)
        go(0)

        
sealed abstract class Matcher[A] extends (Any => Boolean)

object Matcher:
    given [A <: Matchable: ClassTag]: Matcher[A] with
        def apply(x: Any) =
            x.asInstanceOf[Matchable] match
                case _: A => true
                case _    => false
