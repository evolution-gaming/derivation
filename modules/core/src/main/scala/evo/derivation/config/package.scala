package evo.derivation
package config

type Renaming = String => String

private val extractWords = (name: String) => name.split("(?<=\\p{Lower})(?=\\p{Upper})")

val snakeCase: Renaming = extractWords(_).nn.mkString("_").nn.toLowerCase.nn

val kebabCase: Renaming = extractWords(_).nn.mkString("-").nn.toLowerCase.nn

val pascalCase: Renaming = _.capitalize

private type DA = DerivationAnnotation

case class Annotations[X](
    name: String,
    forType: Vector[DA],
    fields: Vector[(String, Vector[DA])],
    singleton: Option[X],
):
    lazy val byField: Map[String, Vector[DA]] = fields.toMap
end Annotations

case class AllAnnotations[X](
    top: Annotations[X],
    subtypes: Vector[(String, AllAnnotations[X])],
):
    lazy val bySubtype = subtypes.toMap
