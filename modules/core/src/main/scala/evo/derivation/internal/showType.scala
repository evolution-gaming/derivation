package evo.derivation.internal

import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.Expr
import java.lang.reflect.Method

inline def showType[T]: String = ${ showTypeMacro[T] }

private def showTypeMacro[T: Type](using Quotes): Expr[String] = Expr(Type.show[T])

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

def showExprMacro(x: Expr[Any])(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    report.info(x.asTerm.show)
    '{ () }
