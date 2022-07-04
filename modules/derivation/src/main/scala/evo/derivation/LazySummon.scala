package evo.derivation

import scala.annotation.implicitNotFound
import scala.compiletime.{summonAll, uninitialized, erasedValue}
import evo.derivation.Config

@implicitNotFound(
  "can't find given instance of ${TC} for ${A} that's required for field or constructor ${Name} of type ${From}",
)
trait LazySummon[+Name, +From, TC[_], +TCC[x] <: TC[x], A]:
    type FieldType = A
    def tc: TC[A]

    final def use[R](f: TC[A] ?=> R): R =
        given TC[A] = tc
        f

object LazySummon:
    type Of[TC[_]] = LazySummon[_, _, TC, _, _]

    type Applied[TC[_], TCC[x] <: TC[x], From, NR] = NR match
        case (name, req) => LazySummon[name, From, TC, TCC, req]

    opaque type All[TC[_], Fields <: Tuple] = IArray[Of[TC]]

    trait LazySummonByInstance[TC[_], A] extends LazySummon[Nothing, Nothing, TC, Nothing, A]

    trait LazySummonByConfig[+TCC[_], A]:
        def instance(using => Config[A]): TCC[A]

    given [TC[_], A](using instance: => TC[A]): LazySummonByInstance[TC, A] with
        lazy val tc = instance

    given [Name <: String, From, TC[_], TCC[x] <: TC[x], A](using
        constructor: LazySummonByConfig[TCC, A],
        config: => Config[From],
        name: ValueOf[Name],
    ): LazySummon[Name, From, TC, TCC, A] with
        def tc: TC[A] = constructor.instance(using config.constructor(name.value).as[A])

    inline def all[Names <: Tuple, From, TC[_], TCC[x] <: TC[x], Fields <: Tuple]: All[TC, Fields] =
        type Summons = Tuple.Map[Tuple.Zip[Names, Fields], [A] =>> Applied[TC, TCC, From, A]]
        summonAll[Summons].toIArray.map(_.asInstanceOf[Of[TC]])

    extension [TC[_], Fields <: Tuple](all: All[TC, Fields])
        def useOn[Src[_], Res[_]](fields: Tuple.Map[Fields, Src])(
            f: [A] => (TC[A], Src[A]) => Res[A],
        ): Tuple.Map[Fields, Res] =
            val elements = all.zip(fields.toIArray).map { case (inst: LazySummon[_, _, TC, tcc, a], field) =>
                f(inst.tc, field.asInstanceOf[Src[a]])
            }

            Tuple.fromIArray(elements).asInstanceOf[Tuple.Map[Fields, Res]]

        def useOnPlain[Res[_]](fields: Fields)(f: [A] => (TC[A], A) => Res[A]): Tuple.Map[Fields, Res] =
            useOn[[a] =>> a, Res](fields.asInstanceOf[Tuple.Map[Fields, [a] =>> a]])(f)

        def useEitherFast[Info, E](infos: Vector[Info])(
            f: (summon: Of[TC], info: Info) => Either[E, summon.FieldType],
        ): Either[E, Fields] =
            var error: Left[E, Nothing] | Null = null
            val elements                       = all.zip(infos).map[Any] { (inst, info) =>
                f(inst, info) match
                    case err @ Left(e) =>
                        error = err.asInstanceOf[Left[E, Nothing]]
                        null
                    case Right(a)      => a
            }
            error match
                case null                 => Right(Tuple.fromIArray(elements).asInstanceOf[Fields])
                case err: Left[E, Fields] => err
        end useEitherFast

        def useEithers[Info, E](infos: Vector[Info])(
            f: (summon: Of[TC], info: Info) => Either[E, summon.FieldType],
        ): Either[Vector[E], Fields] =
            val errors   = Vector.newBuilder[E]
            var err      = false
            val elements = all.zip(infos).map[Any] { (inst, info) =>
                f(inst, info) match
                    case Left(e)  =>
                        err = true
                        errors += e
                        null
                    case Right(a) => a
            }
            if err
            then Left(errors.result)
            else Right(Tuple.fromIArray(elements).asInstanceOf[Fields])
        end useEithers

        inline def toMap[A](names: Vector[String]): Map[String, TC[A]] =
            inline erasedValue[Tuple.Union[Fields]] match
                case _: A =>
                    names.zip(all).map((name, lz) => name -> lz.asInstanceOf[LazySummon[_, _, TC, _, A]].tc).toMap
