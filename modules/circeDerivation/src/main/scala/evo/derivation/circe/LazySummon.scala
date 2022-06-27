package evo.derivation.circe

import scala.annotation.implicitNotFound
import scala.compiletime.summonAll

@implicitNotFound("can't find given instance of $Req that's required for field or constructor $Name")
trait LazySummon[Name, TC[_], A]:
    def tc: TC[A]

object LazySummon:
    type Applied[TC[_], NR] = NR match
        case (name, req) => LazySummon[name, TC, req]

    opaque type All[TC[_], Fields <: Tuple] = IArray[LazySummon[_, TC, _]]

    given [Name, TC[_], A](using instance: => TC[A]): LazySummon[Name, TC, A] with
        lazy val tc = instance

    inline def all[Names <: Tuple, TC[_], Fields <: Tuple]: All[TC, Fields] =
        type Summons = Tuple.Map[Tuple.Zip[Names, Fields], [A] =>> Applied[TC, A]]
        summonAll[Summons].toIArray.asInstanceOf[IArray[LazySummon[_, TC, _]]]

    extension [TC[_], Fields <: Tuple](all: All[TC, Fields])
        def useOn[Src[_], Res[_]](fields: Tuple.Map[Fields, Src])(
            f: [A] => (TC[A], Src[A]) => Res[A],
        ): Tuple.Map[Fields, Res] =
            val elements = all.zip(fields.toIArray).map { case (inst: LazySummon[_, TC, a], field) =>
                f(inst.tc, field.asInstanceOf[Src[a]])
            }

            Tuple.fromIArray(elements).asInstanceOf[Tuple.Map[Fields, Res]]
