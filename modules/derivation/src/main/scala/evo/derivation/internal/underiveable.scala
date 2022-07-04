package evo.derivation.internal

import scala.quoted.*
import scala.compiletime.error

inline def underiveableError[I, T]: Nothing =
    error("could not derive " + showType[I] + ", look's like " + showType[T] + " is neither case class or enum")
