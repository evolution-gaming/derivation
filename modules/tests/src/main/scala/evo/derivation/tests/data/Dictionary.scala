package evo.derivation.tests.data

import evo.derivation.cats.EvoEq
import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}
import play.api.libs.json.given
import sttp.tapir.SchemaType.SProduct
import sttp.tapir.SchemaType.SProductField
import sttp.tapir.FieldName
import sttp.tapir.Schema
import sttp.tapir.SchemaType.SRef
import sttp.tapir.Schema.SName

case class Dictionary(key: String, value: String, next: Option[Dictionary])
    derives Config,
      EvoDecoder,
      EvoEncoder,
      EvoReads,
      EvoWrites,
      EvoEq:
    def update(keyLookup: String, f: String => String): Dictionary =
        if (key == keyLookup) copy(value = f(value))
        else copy(next = next.map(_.update(keyLookup, f)))
end Dictionary

object Dictionary:
    val dictionaryJson =
        """{"key" : "a", "value" : "arbuz", "next" : {"key": "b", "value" : "baraban" }}"""

    val dictionaryJsonWithNull =
        """{"key" : "a", "value" : "arbuz", "next" : {"key": "b", "value" : "baraban" , "next": null }}"""

    val dictionary = Dictionary("a", "arbuz", Some(Dictionary("b", "baraban", None)))

    val mytype = SProduct[Dictionary](
      List(
        SProductField[Dictionary, String](FieldName("key"), implicitly, x => Some(x.key)),
        SProductField[Dictionary, String](FieldName("value"), implicitly, x => Some(x.value)),
        new SProductField[Dictionary] {
            type FieldType = Option[Dictionary]

            override def name: FieldName = FieldName("next")

            override def schema: Schema[FieldType] =
                dictSchema.name.fold(dictSchema)(name => dictSchema.copy(schemaType = SRef(name))).asOption

            override def get: Dictionary => Option[FieldType] = d => Some(d.next)
        },
      ),
    )

    given dictSchema: Schema[Dictionary] = Schema(mytype, Some(SName("Dictionary")))
end Dictionary
