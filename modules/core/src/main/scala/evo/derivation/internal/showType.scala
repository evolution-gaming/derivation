package evo.derivation.internal

import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.Expr

inline def showType[T]: String = ${ showTypeMacro[T] }

private def showTypeMacro[T: Type](using Quotes): Expr[String] = Expr(Type.show[T])
