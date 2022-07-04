package evo.derivation.internal

import scala.quoted.*
import scala.compiletime.error

inline def underiveableError[I, T]: Nothing =
    error("could not derive " + showType[I] + " look's like " + showType[T] + "is neither case class or enum")
    //  ${ underiveableMacro[I, T] }

def underiveableMacro[I: Type, T: Type](using q: Quotes): Expr[Nothing] =

    import q.reflect.*

    val T = Type.show[T]
    val I = Type.show[I]

    val message = s"could not derive $I, look's like $T is neither case class or enum"

    report.errorAndAbort(message)
