package evo.derivation.circe

import scala.compiletime.testing.typeCheckErrors
import scala.compiletime.testing.Error

import evo.derivation.Config
import munit.FunSuite
import EvoDecoderChecks.Hmm

import EvoDecoderChecks.{HmmTName, ImplicitTName}
import io.circe.parser.*
import evo.derivation.circe.EvoDecoderChecks.Person
import evo.derivation.{Rename, SnakeCase, Embed}
import evo.derivation.circe.EvoDecoderChecks.Document
import java.time.Instant
import java.util.UUID
import scala.CanEqual.derived

class EvoDecoderChecks extends FunSuite:
    test("Hmm is not deriveable") {
        assertEquals(
          List(s"could not derive $ImplicitTName, look's like $HmmTName is neither case class or enum"),
          typeCheckErrors("ConfiguredDecoder.derived[Hmm]").map(_.message),
        )
    }

    test("plain product decodes nicely") {
        val json = """{"name": "ololo", "age": 11}"""
        assertEquals(decode[Person](json), Right(Person(name = "ololo", age = 11)))
    }

    test("complex product decodes nicely") {
        val uuid = UUID.fromString("68ede874-fb8a-11ec-a827-00155d6320ce").nn
        val date = Instant.now.nn
        val json = s"""{"documentId": "$uuid", "issue_date": "$date", "name": "alala", "age": 74}"""

        assertEquals(
          decode[Document](json),
          Right(
            Document(
              id = uuid,
              issueDate = date,
              author = Person(name = "alala", age = 74),
            ),
          ),
        )
    }

object EvoDecoderChecks:
    val Package       = "evo.derivation.circe"
    val DecoderTName  = s"$Package.ConfiguredDecoder"
    val HmmTName      = s"$Package.EvoDecoderChecks.Hmm"
    val ImplicitTName = s"$DecoderTName[$HmmTName]"
    class Hmm derives Config

    case class Person(name: String, age: Int) derives Config, ConfiguredDecoder

    @SnakeCase case class Document(
        @Rename("documentId") id: UUID,
        issueDate: Instant,
        @Embed author: Person,
    ) derives Config,
          ConfiguredDecoder

          
