package evo.derivation.config

import evo.derivation.*
import evo.derivation.internal.{updater, update, updaters, Updater, mapItems, mapSecond}

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type, Varargs}
import internal.vectors.given

case class Config[+T](
    top: ForProduct,
    discriminator: Option[String] = None,
    subtypes: Vector[(String, Config[T])] = Vector.empty,
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
        byConstructor.get(constructor).map(_.name).getOrElse(constructor)

    def constructor(name: String): Config[Any] =
        byConstructor.get(name).fold(Config.named(name))(p => Config(top = p))

    def as[T]: Config[T] = asInstanceOf[Config[T]]

    def isConstructor = subtypes.isEmpty

    lazy val constructors: Vector[(String, ForProduct)] =
        subtypes.flatMap { case (name, t) => if t.isConstructor then Array(name -> t.top) else t.constructors }

    lazy val constrFromRenamed: Map[String, String] =
        byConstructor.map((name, prod) => prod.name -> name)

    lazy val byConstructor: Map[String, ForProduct] = constructors.toMap

    lazy val isSimpleEnum = top.fields.isEmpty && !constructors.isEmpty && constructors.forall(_._2.isSingleton)

end Config

object Config:
    def named(name: String) = Config(top = ForProduct(name))

    val default: Config[Nothing] = named("")

    inline def caseConfiguration(ct: CaseTransformation): Renaming = ct match
        case SnakeCase()  => snakeCase
        case KebabCase()  => kebabCase
        case PascalCase() => pascalCase

    inline def derived[T]: Config[T] =
        fromAllAnnotations(readAnnotations[T])

    def products[A]: Updater[Config[A], ForProduct] =
        updaters(
          update[Config[A]](_.top),
          update[Config[A]](_.subtypes) compose mapItems compose mapSecond compose products,
        )

    def renaming[A]: Updater[Config[A], String] = products[A] compose ForProduct.renaming

    private def fromAnnotations(annotations: Annotations, initial: Config[Nothing]): Config[Nothing] =
        val topApplied = annotations.forType.foldLeft(initial)(_.applyAnnotation(_))

        annotations.fields.foldLeft(topApplied) { case (prev, (name, annotations)) =>
            annotations.foldLeft(prev) { _.applyAnnotation(_, Some(name)) }
        }
    end fromAnnotations

    private def basicProduct(annotations: Annotations): ForProduct =
        ForProduct(
          name = annotations.name,
          fields = annotations.fields.map { (name, anns) => name -> ForField(name = name, annotations = anns) },
          annotations = annotations.forType,
          isSingleton = annotations.isSingleton,
        )
    end basicProduct

    private def basicStructure(annotations: AllAnnotations): Config[Nothing] =
        Config(
          top = basicProduct(annotations.top),
          subtypes = annotations.subtypes.map((name, annots) => name -> basicStructure(annots)),
        )

    private def applyAnnotations(annotations: AllAnnotations, initial: Config[Nothing]): Config[Nothing] =
        val base = fromAnnotations(annotations.top, initial = initial)

        base.copy(subtypes =
            base.subtypes.map((name, sub) => name -> applyAnnotations(annotations.bySubtype(name), sub)),
        )
    end applyAnnotations

    private def fromAllAnnotations(annotations: AllAnnotations): Config[Nothing] =
        applyAnnotations(annotations, basicStructure(annotations))

    private inline def readAnnotations[T]: AllAnnotations = ${ allAnnotations[T] }

    private def allAnnotations[T: Type](using Quotes): Expr[AllAnnotations] = ConfigMacro().allAnnotations[T]

end Config
