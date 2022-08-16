package evo.derivation.tests.data

import evo.derivation.cats.EvoEq
import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}
import play.api.libs.json.given

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
end Dictionary
