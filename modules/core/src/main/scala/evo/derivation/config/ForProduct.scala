package evo.derivation.config

import evo.derivation.internal.{updater, update, updateMap, Updater, updaters, mapValues}

case class ForProduct(name: String, fields: Map[String, ForField] = Map.empty):
    lazy val (fieldNames, fieldInfos) = fields.toVector.unzip

    private def set[X](upd: Updater[ForProduct, X])(value: X): ForProduct = upd(_ => value)(this)

    def applyRenaming(field: Option[String], newName: String): ForProduct = field match
        case Some(oldName) => set(ForProduct.field(oldName) compose updater(_.name))(newName)
        case None          => copy(name = newName)

    def embedField(field: String): ForProduct = set(ForProduct.field(field) compose updater(_.embed))(true)
end ForProduct

object ForProduct:
    val renaming: Updater[ForProduct, String] =
        updaters(
          update[ForProduct](_.fields) compose mapValues compose updater(_.name),
          updater(_.name),
        )

    def field(name: String): Updater[ForProduct, ForField] = update[ForProduct](_.fields) compose updateMap(name)
end ForProduct
