package evo.derivation.internal

import scala.deriving.Mirror
import scala.compiletime.constValueTuple

def tupleFromProduct[A](a: A)(using m: Mirror.ProductOf[A])(using A <:< Product): m.MirroredElemTypes =
    val coerce = subtypeToIntersectionEq[A, Product]
    type MP[a] = Mirror.ProductOf[a] { type MirroredElemTypes = m.MirroredElemTypes }
    val coeMirror = coerce.liftCo[MP](m)
    Tuple.fromProductTyped[A & Product](coerce(a))(using coeMirror)
end tupleFromProduct

private def subtypeToIntersectionEq[A, B](using ev: A <:< B): A =:= (A & B) =
    given (A <:< (A & B)) = ev.liftCo[[x] =>> A & x]
    <:<.antisymm

inline def mirroredNames[A](using mirror: Mirror.Of[A]): Vector[String] =
    constValueTuple[mirror.MirroredElemLabels].toIArray.toVector.asInstanceOf[Vector[String]]
