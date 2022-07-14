package evo.derivation.circe

import evo.derivation.Config
import evo.derivation.Discriminator
import evo.derivation.Embed
import evo.derivation.LazySummon.LazySummonByConfig
import evo.derivation.Rename
import evo.derivation.SnakeCase
import io.circe.Decoder
import io.circe.parser.*
import munit.FunSuite

import java.time.Instant
import java.time.LocalDateTime
import java.util.UUID
import scala.CanEqual.derived
import scala.compiletime.testing.Error
import scala.compiletime.testing.typeCheckErrors

import CheckData.TestClass
import CheckData._
import io.circe.syntax.given

class EvoDecoderChecks extends FunSuite:
    test("TestClass is not derivable because it is not a case class nor a enum") {
        assertEquals(
          List(s"could not derive $AppliedDecoderTypeName, look's like $TestClassName is neither case class or enum"),
          typeCheckErrors("EvoDecoder.derived[TestClass]").map(_.message),
        )
    }

    test("plain product") {
        assertEquals(decode[Person](personJson), Right(person))
    }

    test("complex product") {
        assertEquals(decode[Document](documentJson), Right(document))
    }

    test("plain coproduct") {
        assertEquals(decode[User](authorizedJson), Right(authorized))

        assertEquals(decode[User](adminJson), Right(admin))

        assertEquals(decode[User](anonymousJson), Right(User.Anonymous))
    }

    test("complex coproduct") {
        assertEquals(decode[Mode](readJson), Right(read))

        assertEquals(decode[Mode](writeJson), Right(write))
    }

    test("recursive product") {
        assertEquals(decode[Dictionary](dictionaryJson), Right(dictionary))
    }

    test("recursive coproduct") {
        assertEquals(decode[BinTree](binTreeJson), Right(binTree))
    }

class EvoEncoderChecks extends FunSuite:
    test("plain product") {
        assertEquals(parse(personJson), Right(person.asJson))
    }

    test("complex product") {
        assertEquals(parse(documentJson), Right(document.asJson))
    }

    test("plain coproduct") {
        assertEquals(parse(authorizedJson), Right(authorized.asJson))

        assertEquals(parse(anonymousJson), Right(User.Anonymous.asJson))
    }

    test("complex coproduct") {
        assertEquals(parse(readJson), Right(read.asJson))

        assertEquals(parse(writeJson), Right(write.asJson))
    }

    test("recursive product") {
        assertEquals(parse(dictionaryJson), Right(dictionary.asJson.deepDropNullValues))
    }

    test("recursive coproduct") {
        assertEquals(parse(binTreeJson), Right(binTree.asJson))
    }

object CheckData:
    class TestClass derives Config

    val Package                = "evo.derivation.circe"
    val DecoderTypeName        = s"$Package.EvoDecoder"
    val TestClassName          = s"$Package.CheckData.TestClass"
    val AppliedDecoderTypeName = s"$DecoderTypeName[$TestClassName]"

    case class Person(name: String, age: Int) derives Config, EvoDecoder, EvoEncoder

    val person = Person(name = "ololo", age = 11)

    val personJson = """{"name": "ololo", "age": 11}"""

    @SnakeCase
    case class Document(
        @Rename("documentId") id: UUID,
        issueDate: Instant,
        @Embed author: Person,
    ) derives Config,
          EvoDecoder,
          EvoEncoder

    val uuid = UUID.fromString("68ede874-fb8a-11ec-a827-00155d6320ce").nn
    val date = Instant.now.nn

    val document = Document(id = uuid, issueDate = date, author = Person(name = "alala", age = 74))

    val documentJson = s"""{"documentId": "$uuid", "issue_date": "$date", "name": "alala", "age": 74}"""

    enum User derives Config, EvoDecoder, EvoEncoder:
        case Authorized(login: String)
        case Anonymous
        case Admin(login: String, @Rename("access") rights: String)

    val authorized = User.Authorized("ololo")

    val admin = User.Admin("ololo", "kek")

    val authorizedJson = s"""{"Authorized" : {"login": "ololo"}}"""

    val adminJson = s"""{"Admin" : {"login": "ololo", "access": "kek"}}"""

    val anonymousJson = s"""{"Anonymous" : {}}"""

    @Discriminator("mode")
    enum Mode derives Config, EvoDecoder, EvoEncoder:
        @Rename("r") case Read(@Rename("b") bin: Boolean)
        @Rename("w") case Write(append: Boolean = false, bin: Boolean)

    val readJson = s"""{"mode": "r", "b": false}"""

    val read = Mode.Read(false)

    val write = Mode.Write(append = true, bin = true)

    val writeJson = s"""{"mode" : "w", "append":true, "bin": true}"""

    case class Dictionary(key: String, value: String, next: Option[Dictionary]) derives Config, EvoDecoder, EvoEncoder

    val dictionaryJson =
        """{"key" : "a", "value" : "arbuz", "next" : {"key": "b", "value" : "baraban" }}"""

    val dictionary = Dictionary("a", "arbuz", Some(Dictionary("b", "baraban", None)))

    @Discriminator("kind") @SnakeCase
    enum BinTree derives Config, EvoDecoder, EvoEncoder:
        case Branch(value: Int, left: BinTree, right: BinTree)
        case Nil

    val binTreeJson = """{
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

    val binTree =
        BinTree.Branch(1, left = BinTree.Nil, right = BinTree.Branch(3, left = BinTree.Nil, right = BinTree.Nil))
