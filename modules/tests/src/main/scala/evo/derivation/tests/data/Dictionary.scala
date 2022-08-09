package evo.derivation.tests.data

import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}
import play.api.libs.json.given

case class Dictionary(key: String, value: String, next: Option[Dictionary])
    derives Config,
      EvoDecoder,
      EvoEncoder,
      EvoReads,
      EvoWrites

object Dictionary:
    val dictionaryJson =
        """{"key" : "a", "value" : "arbuz", "next" : {"key": "b", "value" : "baraban" }}"""

    val dictionary = Dictionary("a", "arbuz", Some(Dictionary("b", "baraban", None)))
end Dictionary
