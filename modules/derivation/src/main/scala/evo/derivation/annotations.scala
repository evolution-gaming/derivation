package evo.derivation

import scala.annotation.StaticAnnotation

sealed trait DerivationAnnotation extends StaticAnnotation
sealed trait CaseTransformation extends DerivationAnnotation

/**  Transforms data to to snake_case_form
 *
 *   - If used on enum/value class/case class, it will change all field and constructor names
 *   - If used directly on field it will change this field only
 */
case class SnakeCase() extends CaseTransformation

/**  Transforms data to to kebab-case-form
 *
 *   - If used on enum/value class/case class, it will change all field and constructor names
 *   - If used directly on field it will change this field only
 */case class KebabCase() extends CaseTransformation

/** This enum will use discriminator field with given name */
case class Discriminator(name: String) extends DerivationAnnotation

/** this field or this constructor will be renamed to `name` */
case class Rename(name: String) extends DerivationAnnotation

/** this field content will be flattened */
case class Embed() extends DerivationAnnotation
