package evo.derivation.config

import evo.derivation.*
import evo.derivation.internal.{updater, update, updaters, Updater, mapItems, mapSecond}

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type, Varargs}

case class Config[+T](
    top: ForProduct[T, _],
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

    lazy val constructors: Vector[(String, ForProduct[T, _ <: T])] =
        subtypes.flatMap((name, t) => if t.isConstructor then Array(name -> t.top) else t.constructors)

    lazy val constrFromRenamed: Map[String, String] =
        byConstructor.map((name, prod) => prod.name -> name)

    lazy val byConstructor: Map[String, ForProduct[T, _]] = constructors.toMap

    lazy val isSimpleEnum = top.fields.isEmpty && !constructors.isEmpty && constructors.forall(_._2.isSingleton)

    def enumValues: Vector[(String, T)] =
        constructors.flatMap((name, prod) => prod.singleton.map(name -> _))

    def modConfig[T1 >: T](f: Config[T1] => Config[T1]): Config[T1] =
        f(copy(subtypes = subtypes.map { case (name, sub) => name -> sub.modConfig(f) }))

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

    /* enumerating all the subconfigs */
    def configs[T]: Updater[Config[T], Config[T]] = f => _.modConfig(f)

    def products[T]: Updater[Config[T], ForProduct[T, _]] = configs compose updater(_.top)

    def renaming[T]: Updater[Config[T], String] = products[T] compose ForProduct.renaming

    private def fromAnnotations[A](annotations: Annotations[A], initial: Config[A]): Config[A] =
        val topApplied = annotations.forType.foldLeft(initial)(_.applyAnnotation(_))

        annotations.fields.foldLeft(topApplied) { case (prev, (name, annotations, _)) =>
            annotations.foldLeft(prev) { _.applyAnnotation(_, Some(name)) }
        }
    end fromAnnotations

    private def basicProduct[T](annotations: Annotations[T]): ForProduct[T, T] =
        ForProduct(
          name = annotations.name,
          fields = annotations.fields.map { (name, anns, info) =>
              name -> ForField(name = name, annotations = anns, info = info.asInstanceOf[Option[FieldValueInfo[T, _]]])
          },
          annotations = annotations.forType,
          singleton = annotations.singleton,
        )
    end basicProduct

    private def basicStructure[T](annotations: AllAnnotations[T]): Config[T] =
        Config(
          top = basicProduct(annotations.top),
          subtypes = annotations.subtypes.map((name, annots) => name -> basicStructure(annots)),
        )

    private def applyAnnotations[A](annotations: AllAnnotations[A], initial: Config[A]): Config[A] =
        val base = fromAnnotations(annotations.top, initial = initial)

        base.copy(subtypes =
            base.subtypes.map((name, sub) => name -> applyAnnotations(annotations.bySubtype(name), sub)),
        )
    end applyAnnotations

    private def fromAllAnnotations[A](annotations: AllAnnotations[A]): Config[A] =
        applyAnnotations(annotations, basicStructure(annotations))

    private inline def readAnnotations[T]: AllAnnotations[T] = ${ allAnnotations[T] }

    private def allAnnotations[T: Type](using Quotes): Expr[AllAnnotations[T]] = ConfigMacro().allAnnotations[T]

end Config
