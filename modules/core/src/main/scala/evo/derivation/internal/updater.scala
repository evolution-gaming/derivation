package evo.derivation.internal

import scala.quoted.*
import scala.collection.SeqOps

type Updater[C, A] = (A => A) => (C => C)

inline def updater[C <: Product, A](inline f: C => A): Updater[C, A] = ${ updaterMacro[C, A]('f) }

def updaters[A, B](first: Updater[A, B], rest: Updater[A, B]*): Updater[A, B] =
    rest.foldLeft(first)((prev, cur) => f => prev(f) compose cur(f))

def update[C <: Product] = new MkUpdate[C]

class MkUpdate[C <: Product]:
    inline def apply[A](inline f: C => A): Updater[C, A] = updater(f)

def updateMap[K, V](key: K): Updater[Map[K, V], V] = f => _.updatedWith(key)(_.map(f))

def filter[A](cond: A => Boolean): Updater[A, A] = f => a => if (cond(a)) f(a) else a

def mapValues[K, V]: Updater[Map[K, V], V] = f => _.view.mapValues(f).toMap

def mapItems[T[a] <: SeqOps[a, T, T[a]], I]: Updater[T[I], I] = f => _.map(f)

def mapVector[I]: Updater[Vector[I], I] = mapItems

def mapSecond[A, B]: Updater[(A, B), B] = f => { case (a, b) => (a, (f(b))) }

private def updaterMacro[C: Type, A: Type](f: Expr[C => A])(using q: Quotes): Expr[(A => A) => C => C] =
    import q.reflect.*

    def getName(term: Term): String = term match
        case Inlined(_, _, expr)        => getName(expr)
        case Lambda(_, Select(_, name)) => name
        case _                          =>
            report.errorAndAbort(s"expecting a lambda in the form (_: CaseClass).field, got ${f.asTerm.toString}")

    val name = getName(f.asTerm)

    '{ (f: A => A) => (c: C) => ${ updaterCopy('f, 'c, name) } }
end updaterMacro

private def updaterCopy[C: Type, A: Type](f: Expr[A => A], c: Expr[C], name: String)(using q: Quotes): Expr[C] =
    import q.reflect.*

    val typeParams = TypeRepr.of[C].dealias match
        case AppliedType(t, params) => params
        case _                      => Nil

    val `f(c.field)` = Apply(Select.unique(f.asTerm, "apply"), List(Select.unique(c.asTerm, name)))
    val term         = Select.overloaded(c.asTerm, "copy", typeParams, List(NamedArg(name, `f(c.field)`)))

    term.asExprOf[C]
end updaterCopy
