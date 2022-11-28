package evo.derivation.config

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type, Varargs}
import evo.derivation.internal.showFlags

class ConfigMacro(using q: Quotes):

    import q.reflect.*

    def allAnnotations[T: Type]: Expr[AllAnnotations[T]] = annotations(TypeRepr.of[T].typeSymbol)

    private def annotations[T: Type](sym: Symbol): Expr[AllAnnotations[T]] =
        '{
            AllAnnotations(
              top = ${ topAnnotations(sym) },
              subtypes = ${ subtypeAnnotations(sym) },
            )
        }

    private def annotationTree(tree: Tree): Option[Expr[DA]] =
        Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[DA]).map(_.asExprOf[DA])

    private def fieldAnnotations(s: Symbol): Expr[(String, Vector[DA])] =
        val annots = Varargs(s.annotations.flatMap(annotationTree))
        val name   = Expr(s.name)

        '{ $name -> Vector($annots: _*) }
    end fieldAnnotations

    private def topAnnotations[T: Type](sym: Symbol): Expr[Annotations[T]] =
        val topAnns                    = Varargs(sym.annotations.flatMap(annotationTree))
        val caseParams                 = sym.primaryConstructor.paramSymss.take(1).flatten
        val fieldAnns                  = Varargs(caseParams.map(fieldAnnotations))
        val name                       = Expr(sym.name)
        val singleton: Expr[Option[T]] = sym.tree match
            case _: ValDef =>
                val ref = Ref(sym).asExpr.asExprOf[T]
                '{ Some($ref) }
            case _         => Expr(None)

        '{
            Annotations(
              name = $name,
              forType = Vector($topAnns: _*),
              fields = Vector($fieldAnns: _*),
              singleton = $singleton,
            )
        }
    end topAnnotations

    private def subtypeAnnotation[T: Type](sym: Symbol): Expr[(String, AllAnnotations[T])] =
        val name   = Expr(sym.name)
        val annots = annotations(sym)

        '{ ($name, $annots) }
    end subtypeAnnotation

    private def subtypeAnnotations[T: Type](sym: Symbol): Expr[Vector[(String, AllAnnotations[T])]] =
        val subtypes = Varargs(sym.children.map(subtypeAnnotation))

        '{ Vector($subtypes: _*) }
end ConfigMacro
