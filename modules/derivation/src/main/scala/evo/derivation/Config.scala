package evo.derivation
import scala.compiletime.{summonFrom, constValueTuple}

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
            case Embed()            =>
                fieldName match
                    case None            => this
                    case Some(fieldName) => copy(top = top.embedField(fieldName))

    def name(constructor: String): String =
        constructors.get(constructor).flatMap(_.renamed).getOrElse(constructorRenaming(constructor))

    inline def constructorNames[S](using p: Mirror.SumOf[S]): IArray[String] =
        constValueTuple[p.MirroredElemLabels].toIArray.asInstanceOf[IArray[String]].map(name)


    lazy val constrFromRenamed: Map[String, String] =
        constructors.map((name, prod) => prod.renamed.getOrElse(constructorRenaming(name)) -> name)

    def constructor(name: String): Config[Any] =         
        constructors.get(name).fold(Config.default)(p => Config(top = p))

    def as[T]: Config[T] = asInstanceOf[Config[T]]

end Config

object Config:
    type Renaming = String => String

    val default: Config[Nothing] = Config()

    val snakeCase: Renaming = _.split("(?<=\\p{Lower})(?=\\p{Upper})").nn.mkString("_").nn.toLowerCase.nn

    case class ForProduct(
        fields: Vector[String] = Vector(),
        renamed: Option[String] = None,
        fieldNames: Map[String, String] = Map.empty,
        embedFields: Set[String] = Set.empty,
        fieldRenaming: Renaming = identity,
    ):
        def applyRenaming(field: Option[String], newName: String) = field match
            case Some(oldName) => copy(fieldNames = fieldNames + (oldName -> newName))
            case None          => copy(renamed = Some(newName))

        def embedField(field: String) = copy(embedFields = embedFields + field)

        def name(field: String) = fieldNames.getOrElse(field, fieldRenaming(field))

        def fieldInfo(field: String): FieldInfo = FieldInfo(name = name(field), embed = embedFields(field))

        def embedded: Vector[Boolean] = fields.map(embedFields)

        def fieldInfos: Vector[FieldInfo] = fields.map(fieldInfo)

    end ForProduct

    case class FieldInfo(name: String, embed: Boolean)

    inline def derived[T]: Config[T] =
        fromAllAnnots(readAnnotations[T])

    private def fromAnnots(annotations: Annotations): Config[Nothing] =
        val start      =
            if annotations.fields.isEmpty then default
            else Config(top = ForProduct(fields = annotations.fields))
        val topApplied = annotations.forType.foldLeft(start)(_.applyAnnotation(_))

        annotations.byField.foldLeft(topApplied) { case (prev, (name, annotations)) =>
            annotations.foldLeft(prev) { _.applyAnnotation(_, Some(name)) }
        }

    private def fromAllAnnots(annotations: AllAnnotations): Config[Nothing] =
        val config = fromAnnots(annotations.top)

        val byConstructor = annotations.subtypes.view.mapValues(fromAnnots(_).top).toMap

        config.copy(constructors = byConstructor)

    private type DA = DerivationAnnotation

    private inline def readAnnotations[T]: AllAnnotations = ${ allAnnotations[T] }

    private case class Annotations(
        forType: Vector[DA],
        byField: Map[String, Vector[DA]],
        fields: Vector[String],
    )

    private case class AllAnnotations(
        top: Annotations,
        subtypes: Map[String, Annotations],
    )

    private def allAnnotations[T: Type](using Quotes): Expr[AllAnnotations] = ConfigMacro().allAnnotations[T]

    private class ConfigMacro(using q: Quotes):
        import q.reflect.*
        def allAnnotations[T: Type]: Expr[AllAnnotations] =
            val sym = TypeRepr.of[T].typeSymbol

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

        private def topAnnotations(sym: Symbol): Expr[Annotations] =
            val topAnns    = Varargs(sym.annotations.flatMap(annotationTree))
            val caseParams = sym.primaryConstructor.paramSymss.take(1).flatten
            val fields     = Varargs(caseParams.map(sym => Expr(sym.name)))
            val fieldAnns  = Varargs(caseParams.map(fieldAnnotations))

            '{
                Annotations(
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

        private def subtypeAnnotations(sym: Symbol): Expr[Map[String, Annotations]] =
            val subtypes = Varargs(sym.children.map(subtypeAnnotation))

            '{ Map($subtypes: _*) }

        end subtypeAnnotations
    end ConfigMacro

end Config
