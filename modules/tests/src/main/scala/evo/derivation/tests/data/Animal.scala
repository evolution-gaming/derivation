package evo.derivation.tests.data

import evo.derivation.cats.EvoEq
import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}
import evo.derivation.circe
import evo.derivation.circe.deriveEnumerationCodec
import io.circe.Codec

enum Animal:
    case Dog, Cat

object Animal:
    given Codec[Animal]       = deriveEnumerationCodec[Animal]
    val example: List[Animal] = List(Animal.Dog, Animal.Cat)
    val exampleJson           =
        """[
          |  "Dog",
          |  "Cat"
          |]""".stripMargin
end Animal
