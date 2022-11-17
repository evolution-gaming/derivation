package evo.derivation.config

import evo.derivation.*
import evo.derivation.internal.{updater, update, updaters, Updater, mapValues}

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type, Varargs}

case class Config[+T](
    top: ForProduct,
    discriminator: Option[String] = None,
    constructors: Map[String, ForProduct] = Map.empty,
):

    def applyAnnotation(annotation: DerivationAnnotation, fieldName: Option[String] = None): Config[T] =
        annotation match
            case ct: CaseTransformation =>
                val transformation = Config.caseConfiguration(ct)
                fieldName match
                    // for field: transform only this field
                    case Some(field) => copy(top = top.applyRenaming(Some(field), transformation(field)))
                    // for class/enum: transform everything
                    case None        => Config.renaming(transformation)(this)
            case Discriminator(dis)     => copy(discriminator = Some(dis))
            case Rename(newName)        => copy(top = top.applyRenaming(fieldName, newName))
            case Embed()                =>
                fieldName match
                    case None            => this
                    case Some(fieldName) => copy(top = top.embedField(fieldName))
            case custom: Custom         => custom(this)

    def name(constructor: String): String =
        constructors.get(constructor).map(_.name).getOrElse(constructor)

    lazy val constrFromRenamed: Map[String, String] =
        constructors.map((name, prod) => prod.name -> name)

    def constructor(name: String): Config[Any] =
        constructors.get(name).fold(Config.named(name))(p => Config(top = p))

    def as[T]: Config[T] = asInstanceOf[Config[T]]

    lazy val isSimpleEnum = top.fields.isEmpty && !constructors.isEmpty && constructors.values.forall(_.isSingleton)

end Config

object Config:
    def named(name: String) = Config(top = ForProduct(name))

    val default: Config[Nothing] = named("")

    inline def caseConfiguration(ct: CaseTransformation): Renaming = ct match
        case SnakeCase()  => snakeCase
        case KebabCase()  => kebabCase
        case PascalCase() => pascalCase

    inline def derived[T]: Config[T] =
        fromAllAnnots(readAnnotations[T])

    def products[A]: Updater[Config[A], ForProduct] =
        updaters(update[Config[A]](_.top), update[Config[A]](_.constructors) compose mapValues)
    def renaming[A]: Updater[Config[A], String]     = products[A] compose ForProduct.renaming

    private def fromAnnots(annotations: Annotations, initial: Config[Nothing]): Config[Nothing] =
        val topApplied = annotations.forType.foldLeft(initial)(_.applyAnnotation(_))

        annotations.byField.foldLeft(topApplied) { case (prev, (name, annotations)) =>
            annotations.foldLeft(prev) { _.applyAnnotation(_, Some(name)) }
        }
    end fromAnnots

    private def basicProduct(annotations: Annotations): ForProduct =
        def field(name: String) = ForField(
          name = name,
          annotations = annotations.byField.getOrElse(name, Vector.empty),
        )

        ForProduct(
          name = annotations.name,
          fieldNames = annotations.fields,
          fields = annotations.fields.map(name => name -> field(name)).toMap,
          annotations = annotations.forType,
          isSingleton = annotations.isSingleton,
        )
    end basicProduct

    private def fromAllAnnots(annotations: AllAnnotations): Config[Nothing] =
        val constructors = annotations.subtypes.map((name, annots) => name -> basicProduct(annots))

        val byConstructor = Config(top = basicProduct(annotations.top), constructors = constructors)

        val base = fromAnnots(annotations.top, initial = byConstructor)

        val updatedConstructors =
            base.constructors.map((name, product) =>
                name -> fromAnnots(annotations.subtypes(name), initial = Config(top = product)).top,
            )

        base.copy(constructors = updatedConstructors)
    end fromAllAnnots

    private inline def readAnnotations[T]: AllAnnotations = ${ allAnnotations[T] }

    private def allAnnotations[T: Type](using Quotes): Expr[AllAnnotations] = ConfigMacro().allAnnotations[T]

end Config
