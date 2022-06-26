package evo.derivation.internal

import scala.quoted.*

inline def underiveableError[I, T]: Nothing = ${ underiveableMacro[I, T] }


def underiveableMacro[I: Type, T: Type](using q: Quotes): Expr[Nothing] =

    import q.reflect.*

    val T = Type.show[T]
    val I = Type.show[I]

    val message = s"could not derive $I, look's like $T is neither case class or enum"

    report.errorAndAbort(message)
