package evo.derivation.config

import evo.derivation.DerivationAnnotation
import evo.derivation.internal.{Updater, update, updater, updaters, mapItems, mapSecond, filter}

case class ForProduct(
    name: String,
    fields: Vector[(String, ForField)] = Vector.empty,
    annotations: Vector[DerivationAnnotation] = Vector.empty,
    isSingleton: Boolean = false,
):

    lazy val byField: Map[String, ForField] = fields.toMap

    private def set[X](upd: Updater[ForProduct, X])(value: X): ForProduct = upd(_ => value)(this)

    def applyRenaming(field: Option[String], newName: String): ForProduct = field match
        case Some(oldName) => set(ForProduct.field(oldName) compose updater(_.name))(newName)
        case None          => copy(name = newName)

    def embedField(field: String): ForProduct = set(ForProduct.field(field) compose updater(_.embed))(true)
end ForProduct

object ForProduct:
    val renaming: Updater[ForProduct, String] =
        updaters(
          update[ForProduct](_.fields) compose mapItems compose mapSecond compose updater(_.name),
          updater(_.name),
        )

    def field(name: String): Updater[ForProduct, ForField] =
        update[ForProduct](_.fields) compose mapItems compose filter(_._1 == name) compose mapSecond
end ForProduct

case class ForField(
    name: String,
    embed: Boolean = false,
    annotations: Vector[DerivationAnnotation] = Vector.empty,
)
