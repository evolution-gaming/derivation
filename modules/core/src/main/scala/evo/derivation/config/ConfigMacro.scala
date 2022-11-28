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

    private def fieldAnnotations[C: Type](s: Symbol): Expr[(String, Vector[DA], Option[FieldValueInfo[C, _]])] =
        val annots = Varargs(s.annotations.flatMap(annotationTree))
        val name   = Expr(s.name)

        def readExpr[field: Type](p: Expr[C]): Expr[field] =
            Select.unique(p.asTerm, s.name).asExpr.asExprOf[field]

        val infoExpr: Expr[Option[FieldValueInfo[C, _]]] = s.tree match
            case vd: ValDef =>
                val info = vd.tpt.tpe.asType match
                    case '[field] =>
                        '{
                            FieldValueInfo[C, field](
                              default = None,
                              readF = (p: C) => ${ readExpr[field]('{ p }) },
                              updateF = (p: C, f: field) => p,
                            )
                        }
                '{ Some($info) }
            case _          => '{ None }

        '{ ($name, Vector($annots: _*), $infoExpr) }

    end fieldAnnotations

    private def topAnnotations[T: Type](sym: Symbol): Expr[Annotations[T]] =
        val topAnns    = Varargs(sym.annotations.flatMap(annotationTree))
        val caseParams = sym.primaryConstructor.paramSymss.take(1).flatten
        val fieldAnns  = sym.typeRef.asType match
            case '[constr] =>
                Varargs(caseParams.map(fieldAnnotations[constr]))

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
