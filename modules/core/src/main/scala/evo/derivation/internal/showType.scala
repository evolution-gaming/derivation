package evo.derivation.internal

import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.Expr
import java.lang.reflect.Method

inline def showType[T <: AnyKind]: String = ${ showTypeMacro[T] }

private def showTypeMacro[T <: AnyKind: Type](using q: Quotes): Expr[String] =
    import q.reflect.*
    Expr(TypeRepr.of[T].dealias.widen.show)

// for the internal debugging purposes
def showFlags(using q: Quotes)(flags: q.reflect.Flags): String =
    import q.reflect.Flags
    val methods                    = Flags.getClass.getDeclaredMethods().nn
    def flagMethod(method: Method) =
        method.getReturnType() == classOf[Long] && flags.is(method.invoke(Flags).asInstanceOf[Flags])
    methods
        .collect { case m: Method if flagMethod(m) => m.getName.nn }
        .toVector
        .mkString(",")
end showFlags

inline def showExpr(inline x: Any): Unit = ${ showExprMacro('x) }

private def showExprMacro(x: Expr[Any])(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    report.info(x.asTerm.show)
    '{ () }

inline def printWarning(inline x: String): Unit = ${ printWarningMacro('x) }

private def printWarningMacro(x: Expr[String])(using q: Quotes): Expr[Unit] =
    x.value.foreach(q.reflect.report.warning)
    '{ () }
