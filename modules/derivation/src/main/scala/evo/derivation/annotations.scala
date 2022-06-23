package evo.derivation

import scala.annotation.StaticAnnotation

sealed trait DerivationAnnotation extends StaticAnnotation

/** all fields and constructor names of this case class enum will be renamed to snake case form */
case class SnakeCase() extends DerivationAnnotation

/** this enum will use discriminator field with given name */
case class Discriminator(name: String) extends DerivationAnnotation

/** this field or this constructor will be renamed to `name` */
case class Rename(name: String) extends DerivationAnnotation

/** this field content will be flattened */
case class Embed() extends DerivationAnnotation
