package evo.derivation

import scala.quoted.*

trait ValueClass[A]:
    type AccessorName <: String
    type TypeName <: String
    type Representation
    def accessorName: AccessorName

    def to(value: A): Representation
    def from(repr: Representation): A

object ValueClass:
    transparent inline given derived[A]: ValueClass[A] = ${ derivedMacro[A] }

    def derivedMacro[A: Type](using q: Quotes): Expr[ValueClass[A]] = ValueClassMacro().result

private class ValueClassMacro[A: Type](using q: Quotes):
    import q.reflect.*

    private val aType = TypeRepr.of[A].typeSymbol

    if !aType.isClassDef && !aType.flags.is(Flags.Case) then
        report.errorAndAbort(s"${Type.show[A]} is not a case class")

    private val ValDef(name, tpe, _) = aType.primaryConstructor.paramSymss match
        case List(List(single)) => single.tree
        case _                  => report.errorAndAbort("should contain a single field")

    def result: Expr[ValueClass[A]] =

        val accNameType = ConstantType(StringConstant(name)).asType

        val reprType = tpe.tpe.asType

        (accNameType, reprType) match
            case ('[accNameT], '[reprT]) =>
                val nameExpr = Expr(name).asExprOf[accNameT]

                // TODO find a way to inline expressions directly without lambdas
                val fromRepr: Expr[reprT => A] = '{ (expr: reprT) => ${ fromExpr[reprT]('expr) } }

                val toRepr: Expr[A => reprT] = '{ (a: A) => ${ toExpr[reprT]('a) } }

                '{
                    new ValueClass[A] {
                        type AccessorName = accNameT

                        type Representation = reprT

                        def accessorName: AccessorName = $nameExpr

                        def from(repr: Representation): A = $fromRepr(repr)

                        def to(value: A): Representation = $toRepr(value)
                    }
                }

    private def termRef(s: Symbol): TermRef =
        try
            val methods = q.reflect.SymbolMethods

            val termRef = methods.getClass.getMethods.nn.collect {
                case m if m.nn.getName.nn.contains("termRef") => m.nn
            }.head

            termRef.invoke(methods, s).asInstanceOf[TermRef]
        catch
            case _: Exception =>
                report.errorAndAbort(s"Internal error: can't get reference of $s")

    private def typeRef(s: Symbol): TypeRef =
        try
            val methods = q.reflect.SymbolMethods

            val termRef = methods.getClass.getMethods.nn.collect {
                case m if m.nn.getName.nn.contains("typeRef") => m.nn
            }.head

            termRef.invoke(methods, s).asInstanceOf[TypeRef]
        catch
            case _: Exception =>
                report.errorAndAbort(s"Internal error: can't get reference of $s")

    private def fromExpr[Repr: Type](repr: Expr[Repr]): Expr[A] =

        val companion = aType.companionClass

        val apply = companion
            .declaredMethod("apply")
            .headOption
            .getOrElse(
              report.errorAndAbort(s"could not find apply method in $companion"),
            )

        val prefix = TypeRepr.of[A] match
            case TypeRef(prefix, _) => prefix
            case t                  => report.errorAndAbort(s"can't derive that for the type $t, sorry")

        val ref = Select(Ident(TermRef(prefix, aType.name)), apply)

        val xxx = Apply(ref, List(repr.asTerm)).asExpr.asExprOf[A]

        xxx

    private def toExpr[Repr: Type](value: Expr[A]): Expr[Repr] =
        val term = value.asTerm

        val valueField = term.symbol.declaredField(name)

        Select(term, valueField).asExpr.asExprOf[Repr]

trait IntValueClass[A] extends ValueClass[A]:
    type Representation <: Int

    def to(value: A): Representation

    def from(repr: Representation): A

trait LongValueClass[A] extends ValueClass[A]:
    type Representation <: Long

    def to(value: A): Representation

    def from(repr: Representation): A

trait ShortValueClass[A] extends ValueClass[A]:
    type Representation <: Short

    def to(value: A): Representation

    def from(repr: Representation): A

trait ByteValueClass[A] extends ValueClass[A]:
    type Representation <: Byte

    def to(value: A): Representation

    def from(repr: Representation): A

trait BooleanValueClass[A] extends ValueClass[A]:
    type Representation <: Boolean

    def to(value: A): Representation

    def from(repr: Representation): A

trait DoubleValueClass[A] extends ValueClass[A]:
    type Representation <: Double

    def to(value: A): Representation

    def from(repr: Representation): A

trait FloatValueClass[A] extends ValueClass[A]:
    type Representation <: Float

    def to(value: A): Representation

    def from(repr: Representation): A
