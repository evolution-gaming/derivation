package evo.derivation.internal

import evo.derivation.Config
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonFrom, error}

inline def summonWithConfig[T <: Tuple, names <: Tuple, TC[_]](
    inline cfg: Config[Any],
    inline f: [A] => (Config[Any], Mirror.ProductOf[A]) => TC[A],
): Tuple.Map[T, TC] =
    inline erasedValue[T] match
        case _: EmptyTuple => EmptyTuple
        case _: (h *: tail) =>
            inline erasedValue[names] match
                case _: EmptyTuple => error("names tuple is too short")
                case (name *: _): (String *: rest) =>
                    type H = h
                    val subcfg = cfg.constructor(name)
                    summonFrom {
                        case x: TC[H]               => x
                        case x: Mirror.ProductOf[H] => f(subcfg, x)
                        case _ => error("could not summon or derive configured instance of " + showType[TC[H]])
                    } *: summonWithConfig[tail, rest, TC](cfg, f)
