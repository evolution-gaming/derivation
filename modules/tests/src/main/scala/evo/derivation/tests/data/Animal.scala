package evo.derivation.tests.data

import evo.derivation.cats.EvoEq
import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}
import evo.derivation.circe
import evo.derivation.circe.deriveEnumerationCodec
import io.circe.Codec

sealed trait Animal
object Animal:
    sealed trait Mammal  extends Animal
    sealed trait Reptile extends Animal

    case object Dog    extends Mammal
    case object Cat    extends Mammal
    case object Turtle extends Reptile

    given Codec[Animal] = deriveEnumerationCodec[Animal]

    val example: List[Animal] = List(Dog, Cat, Turtle)
    val exampleJson           =
        """[
        |  "Dog",
        |  "Cat",
        |  "Turtle"
        |]""".stripMargin
end Animal
