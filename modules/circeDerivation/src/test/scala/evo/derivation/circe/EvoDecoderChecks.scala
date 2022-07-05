package evo.derivation.circe

import evo.derivation.Config
import evo.derivation.Discriminator
import evo.derivation.Embed
import evo.derivation.LazySummon.LazySummonByConfig
import evo.derivation.Rename
import evo.derivation.SnakeCase
import evo.derivation.circe.CheckData.Dictionary
import evo.derivation.circe.CheckData.Document
import evo.derivation.circe.CheckData.Mode
import evo.derivation.circe.CheckData.Person
import evo.derivation.circe.CheckData.User
import evo.derivation.circe.CheckData.*
import io.circe.Decoder
import io.circe.parser.*
import munit.FunSuite

import java.time.Instant
import java.time.LocalDateTime
import java.util.UUID
import scala.CanEqual.derived
import scala.compiletime.testing.Error
import scala.compiletime.testing.typeCheckErrors

import CheckData.Hmm
import CheckData.{HmmTName, ImplicitTName}

class EvoDecoderChecks extends FunSuite:
    test("Hmm is not deriveable") {
        assertEquals(
          List(s"could not derive $ImplicitTName, look's like $HmmTName is neither case class or enum"),
          typeCheckErrors("ConfiguredDecoder.derived[Hmm]").map(_.message),
        )
    }

    test("plain product") {
        val json =
            assertEquals(decode[Person](personJson), Right(Person(name = "ololo", age = 11)))
    }

    test("complex product") {
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

    test("plain coproduct") {
        val json1 = s"""{"Authorized" : {"login": "ololo"}}"""

        val json2 = s"""{"Anonimous" : {}}"""

        assertEquals(
          decode[User](json1),
          Right(User.Authorized("ololo")),
        )

        assertEquals(
          decode[User](json2),
          Right(User.Anonimous),
        )
    }

    test("complex coproduct") {
        val json1 = s"""{"mode": "r", "b": false}"""

        val json2 = s"""{"mode" : "w", "append":true, "bin": true}"""

        assertEquals(
          decode[Mode](json1),
          Right(Mode.Read(false)),
        )

        assertEquals(
          decode[Mode](json2),
          Right(Mode.Write(append = true, bin = true)),
        )
    }

    test("recursive product") {
        val json = """{"key" : "a", "value" : "arbuz", "next" : {"key": "b", "value" : "baraban"}}"""

        assertEquals(
          decode[Dictionary](json),
          Right(Dictionary("a", "arbuz", Some(Dictionary("b", "baraban", None)))),
        )
    }

    test("recursive coproduct") {
        val json = """{
            "kind" : "branch", 
            "value": 1, 
            "left": {
              "kind" : "nil"
            }, 
            "right": {
              "kind" : "branch",
              "value" : 3,
              "left" : {
                "kind" : "nil"
              },
              "right" : {
                "kind" : "nil"
              }
            }
          }"""

        assertEquals(
          decode[BinTree](json),
          Right(
            BinTree.Branch(1, left = BinTree.Nil, right = BinTree.Branch(3, left = BinTree.Nil, right = BinTree.Nil)),
          ),
        )
    }

object CheckData:
    val Package       = "evo.derivation.circe"
    val DecoderTName  = s"$Package.ConfiguredDecoder"
    val HmmTName      = s"$Package.EvoDecoderChecks.Hmm"
    val ImplicitTName = s"$DecoderTName[$HmmTName]"
    class Hmm derives Config

    case class Person(name: String, age: Int) derives Config, ConfiguredDecoder

    val person = Person(name = "ololo", age = 11)

    val personJson = """{"name": "ololo", "age": 11}"""

    @SnakeCase case class Document(
        @Rename("documentId") id: UUID,
        issueDate: Instant,
        @Embed author: Person,
    ) derives Config,
          ConfiguredDecoder

    enum User derives Config, ConfiguredDecoder:
        case Authorized(login: String)
        case Anonimous

    @Discriminator("mode")
    enum Mode derives Config, ConfiguredDecoder:
        @Rename("r") case Read(@Rename("b") bin: Boolean)
        @Rename("w") case Write(append: Boolean = false, bin: Boolean)

    case class Dictionary(key: String, value: String, next: Option[Dictionary]) derives Config, ConfiguredDecoder

    @Discriminator("kind") @SnakeCase
    enum BinTree derives Config, ConfiguredDecoder:
        case Branch(value: Int, left: BinTree, right: BinTree)
        case Nil
