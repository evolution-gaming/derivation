package evo.derivation

import scala.annotation.StaticAnnotation

sealed trait DerivationAnnotation extends StaticAnnotation

/** Case transformation
  *
  *   - if used on enum/value class/case class, it will change all field and constructor names;
  *   - if used directly on field it will change this field only.
  *
  * @note
  *   It's expected that original names are in camelCaseForm.
  */
sealed trait CaseTransformation extends DerivationAnnotation

/** Transforms names to to snake_case_form
  *
  * @inheritdoc
  */
case class SnakeCase() extends CaseTransformation

/** Transforms names to to kebab-case-form
  *
  * @inheritdoc
  */
case class KebabCase() extends CaseTransformation

/** Transforms names to to PascalCaseForm
  *
  * @inheritdoc
  */
case class PascalCase() extends CaseTransformation

/** This enum will use discriminator field with given name */
case class Discriminator(name: String) extends DerivationAnnotation

/** this field or this constructor will be renamed to `name` */
case class Rename(name: String) extends DerivationAnnotation

/** this field content will be flattened */
case class Embed() extends DerivationAnnotation
