package evo.derivation.config

import evo.derivation.DerivationAnnotation
import evo.derivation.internal.{Updater, update, updater, updaters, mapItems, mapSecond, filter}

case class ForProduct[+T, P <: T](
    name: String,
    fields: Vector[(String, ForField[P])] = Vector.empty,
    annotations: Vector[DerivationAnnotation] = Vector.empty,
    singleton: Option[P] = None,
):

    lazy val byField: Map[String, ForField[P]] = fields.toMap

    private def set[X](upd: Updater[ForProduct[T, P], X])(value: X): ForProduct[T, P] = upd(_ => value)(this)

    def applyRenaming(field: Option[String], newName: String): ForProduct[T, P] = field match
        case Some(oldName) => set(ForProduct.field(oldName) compose updater(_.name))(newName)
        case None          => copy(name = newName)

    def embedField(field: String): ForProduct[T, _] = set(ForProduct.field(field) compose updater(_.embed))(true)

    def isSingleton: Boolean = singleton.nonEmpty
end ForProduct

object ForProduct:
    def renaming[T]: Updater[ForProduct[T, _], String] =
        updaters(
          update[ForProduct[T, _]](_.fields) compose mapItems compose mapSecond compose updater(_.name),
          updater(_.name),
        )

    def field[T, P <: T](name: String): Updater[ForProduct[T, P], ForField[P]] =
        update[ForProduct[T, P]](_.fields) compose mapItems compose filter(_._1 == name) compose mapSecond
end ForProduct

case class ForField[T](
    name: String,
    embed: Boolean = false,
    annotations: Vector[DerivationAnnotation] = Vector.empty,
    info: Option[FieldValueInfo[T, _]] = None,
)

trait FieldValueInfo[T, Field]:
    def defaultValue: Option[() => Field]
    def read(p: T): Field
    def update(p: T, f: Field): T

object FieldValueInfo:
    def apply[T, Field](
        default: Option[() => Field],
        readF: T => Field,
        updateF: (T, Field) => T,
    ): FieldValueInfo[T, Field] = new:
        def defaultValue: Option[() => Field] = default
        def read(p: T): Field                 = readF(p)
        def update(p: T, f: Field): T         = updateF(p, f)
end FieldValueInfo
