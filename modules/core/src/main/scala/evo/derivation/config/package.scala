package evo.derivation
package config

type Renaming = String => String

private val extractWords = (name: String) => name.split("(?<=\\p{Lower})(?=\\p{Upper})")

val snakeCase: Renaming = extractWords(_).nn.mkString("_").nn.toLowerCase.nn

val kebabCase: Renaming = extractWords(_).nn.mkString("-").nn.toLowerCase.nn

val pascalCase: Renaming = _.capitalize

private type DA = DerivationAnnotation

case class Annotations[T](
    name: String,
    forType: Vector[DA],
    fields: Vector[(String, Vector[DA], Option[FieldValueInfo[_, _]])],
    singleton: Option[T],
):
    lazy val byField: Map[String, Vector[DA]] = fields.iterator.map((name, das, _) => name -> das).toMap
end Annotations

case class AllAnnotations[T](
    top: Annotations[T],
    subtypes: Vector[(String, AllAnnotations[T])],
):
    lazy val bySubtype = subtypes.toMap

end AllAnnotations
