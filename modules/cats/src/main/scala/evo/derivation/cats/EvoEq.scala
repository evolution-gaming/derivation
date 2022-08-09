package evo.derivation.cats

import cats.kernel.Eq
import evo.derivation.config.Config

trait EvoEq[A] extends Equiv[A] with Eq[A]

object EvoEq:
    inline def derived[A](using cfg: Config[A]): EvoEq[A] = ???
