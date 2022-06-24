package evo.derivation
import scala.compiletime.summonFrom

import Config.Renaming
import scala.deriving.Mirror
import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.Expr
import scala.quoted.Varargs
import evo.derivation.Config.ForProduct

case class Config[+T](
    top: Config.ForProduct = Config.ForProduct(),
    discriminator: Option[String] = None,
    constructorRenaming: Renaming = identity,
    constructors: Map[String, ForProduct] = Map.empty,
):
    def mapAllProducts(f: ForProduct => ForProduct): Config[T] =
        copy(top = f(top), constructors = constructors.view.mapValues(f).toMap)

    def withRenaming(renaming: String => String): Config[T] =
        copy(constructorRenaming = renaming).mapAllProducts(_.copy(fieldRenaming = renaming))

    def applyAnnotation(annotation: DerivationAnnotation, fieldName: Option[String] = None): Config[T] =
        annotation match
            case SnakeCase()        => withRenaming(Config.snakeCase)
            case Discriminator(dis) => copy(discriminator = Some(dis))
            case Rename(newName)    => copy(top = top.applyRenaming(fieldName, newName))
            case Embed() =>
                fieldName match
                    case None            => this
                    case Some(fieldName) => copy(top = top.embedField(fieldName))

end Config

object Config {
    type Renaming = String => String

    val default: Config[Nothing] = Config()

    val snakeCase: Renaming = _.replace("(?<=p{Lower})(\\p{Upper})", "_").nn.toLowerCase.nn

    case class ForProduct(
        renamed: Option[String] = None,
        fieldNames: Map[String, String] = Map.empty,
        embedFields: Set[String] = Set.empty,
        fieldRenaming: Renaming = identity,
    ):
        def applyRenaming(field: Option[String], newName: String) = field match
            case Some(oldName) => copy(fieldNames = fieldNames + (oldName -> newName))
            case None          => copy(renamed = Some(newName))

        def embedField(field: String) = copy(embedFields = embedFields + field)

    end ForProduct

    inline def derived[T]: Config[T] = fromAnnots(readAnnotations[T])

    private def fromAnnots(annotations: Annotations): Config[Nothing] =
        println(annotations)

        val topApplied = annotations.forType.foldLeft(default)(_.applyAnnotation(_))

        annotations.byField.foldLeft(topApplied) { case (prev, (name, annotations)) =>
            annotations.foldLeft(prev) { _.applyAnnotation(_, Some(name)) }
        }

    private type DA = DerivationAnnotation

    private inline def readAnnotations[T]: Annotations = ${ topAnnotations[T] }

    private case class Annotations(
        forType: Vector[DA],
        byField: Map[String, Vector[DA]],
    )

    private def topAnnotations[T: Type](using q: Quotes): Expr[Annotations] =
        import q.reflect.*
        val sym = TypeRepr.of[T].typeSymbol

        def annotationTree(tree: Tree): Option[Expr[DA]] =
            Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[DA]).map(_.asExprOf[DA])

        def fieldAnnotations(s: Symbol): Expr[(String, Vector[DA])] =
            val annots = Varargs(s.annotations.flatMap(annotationTree))
            val name = Expr(s.name)

            '{ $name -> Vector($annots: _*) }

        val topAnns = Varargs(sym.annotations.flatMap(annotationTree))

        val caseParams = sym.primaryConstructor.paramSymss.take(1).flatten
        
        val fieldAnns = Varargs(caseParams.map(fieldAnnotations))

        '{
            Annotations(
              forType = Vector($topAnns: _*),
              byField = Map($fieldAnns: _*),
            )
        }
    end topAnnotations

}
