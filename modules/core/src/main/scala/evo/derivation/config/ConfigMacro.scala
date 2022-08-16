package evo.derivation.config

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type, Varargs}

class ConfigMacro(using q: Quotes):

    import q.reflect.*

    def allAnnotations[T: Type]: Expr[AllAnnotations] =
        val sym = TypeRepr.of[T].typeSymbol

        '{
            AllAnnotations(
              top = ${ topAnnotations(sym) },
              subtypes = ${ subtypeAnnotations(sym) },
            )
        }
    end allAnnotations

    private def annotationTree(tree: Tree): Option[Expr[DA]] =
        Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[DA]).map(_.asExprOf[DA])

    private def fieldAnnotations(s: Symbol): Expr[(String, Vector[DA])] =
        val annots = Varargs(s.annotations.flatMap(annotationTree))
        val name   = Expr(s.name)

        '{ $name -> Vector($annots: _*) }
    end fieldAnnotations

    private def topAnnotations(sym: Symbol): Expr[Annotations] =
        val topAnns    = Varargs(sym.annotations.flatMap(annotationTree))
        val caseParams = sym.primaryConstructor.paramSymss.take(1).flatten
        val fields     = Varargs(caseParams.map(sym => Expr(sym.name)))
        val fieldAnns  = Varargs(caseParams.map(fieldAnnotations))
        val name       = Expr(sym.name)

        '{
            Annotations(
              name = $name,
              forType = Vector($topAnns: _*),
              byField = Map($fieldAnns: _*),
              fields = Vector($fields: _*),
            )
        }
    end topAnnotations

    private def subtypeAnnotation(sym: Symbol): Expr[(String, Annotations)] =
        val name   = Expr(sym.name)
        val annots = topAnnotations(sym)

        '{ ($name, $annots) }
    end subtypeAnnotation

    private def subtypeAnnotations(sym: Symbol): Expr[Map[String, Annotations]] =
        val subtypes = Varargs(sym.children.map(subtypeAnnotation))

        '{ Map($subtypes: _*) }
end ConfigMacro
