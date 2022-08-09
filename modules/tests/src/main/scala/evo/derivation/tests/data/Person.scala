package evo.derivation.tests.data

import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}

case class Person(name: String, age: Int) derives Config, EvoDecoder, EvoEncoder, EvoReads, EvoWrites

object Person:
    val person = Person(name = "ololo", age = 11)

    val personJson = """{"name": "ololo", "age": 11}"""
