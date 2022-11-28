package evo.derivation.config

import evo.derivation.DerivationAnnotation
import evo.derivation.internal.{Updater, update, updater, updaters, mapItems, mapSecond, filter}

case class ForProduct[+T](
    name: String,
    fields: Vector[(String, ForField)] = Vector.empty,
    annotations: Vector[DerivationAnnotation] = Vector.empty,
    singleton: Option[T] = None,
):

    lazy val byField: Map[String, ForField] = fields.toMap

    private def set[X](upd: Updater[ForProduct[T], X])(value: X): ForProduct[T] = upd(_ => value)(this)

    def applyRenaming(field: Option[String], newName: String): ForProduct[T] = field match
        case Some(oldName) => set(ForProduct.field(oldName) compose updater(_.name))(newName)
        case None          => copy(name = newName)

    def embedField(field: String): ForProduct[T] = set(ForProduct.field(field) compose updater(_.embed))(true)

    def isSingleton: Boolean = singleton.nonEmpty
end ForProduct

object ForProduct:
    def renaming[T]: Updater[ForProduct[T], String] =
        updaters(
          update[ForProduct[T]](_.fields) compose mapItems compose mapSecond compose updater(_.name),
          updater(_.name),
        )

    def field[T](name: String): Updater[ForProduct[T], ForField] =
        update[ForProduct[T]](_.fields) compose mapItems compose filter(_._1 == name) compose mapSecond
end ForProduct

case class ForField(
    name: String,
    embed: Boolean = false,
    annotations: Vector[DerivationAnnotation] = Vector.empty,
)
