package evo.derivation
package config

type Renaming = String => String

private val extractWords = (name: String) => name.split("(?<=\\p{Lower})(?=\\p{Upper})")

val snakeCase: Renaming = extractWords(_).nn.mkString("_").nn.toLowerCase.nn

val kebabCase: Renaming = extractWords(_).nn.mkString("-").nn.toLowerCase.nn

val pascalCase: Renaming = _.capitalize

private type DA = DerivationAnnotation

case class Annotations(
    name: String,
    forType: Vector[DA],
    byField: Map[String, Vector[DA]],
    fields: Vector[String],
    isSingleton: Boolean,
)

case class AllAnnotations(
    top: Annotations,
    subtypes: Map[String, Annotations],
)
