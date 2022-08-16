package evo.derivation.tests

import evo.derivation.config.Config
import munit.FunSuite
import evo.derivation.tests.data.TestClass.*
import data.*
import io.circe.parser.*
import evo.derivation.circe.EvoDecoder

import scala.compiletime.testing.typeCheckErrors

class EvoDecoderChecks extends FunSuite:
    test("TestClass is not derivable because it is not a case class nor a enum") {
        assertEquals(
          List(s"could not derive $AppliedDecoderTypeName, look's like $TestClassName is neither case class or enum"),
          typeCheckErrors("EvoDecoder.derived[TestClass]").map(_.message),
        )
    }

    test("plain product") {
        assertEquals(decode[Person](Person.personJson), Right(Person.person))
    }

    test("complex product") {
        println(summon[Config[Document]])

        assertEquals(decode[Document](Document.documentJson), Right(Document.document))
    }

    test("plain coproduct") {
        assertEquals(decode[User](User.authorizedJson), Right(User.authorized))

        assertEquals(decode[User](User.adminJson), Right(User.admin))

        assertEquals(decode[User](User.anonymousJson), Right(User.Anonymous))
    }

    test("complex coproduct") {
        assertEquals(decode[Mode](Mode.readJson), Right(Mode.read))

        assertEquals(decode[Mode](Mode.writeJson), Right(Mode.write))
    }

    test("recursive product") {
        assertEquals(decode[Dictionary](Dictionary.dictionaryJson), Right(Dictionary.dictionary))
    }

    test("recursive coproduct") {
        assertEquals(decode[BinTree](BinTree.binTreeJson), Right(BinTree.binTree))
    }
end EvoDecoderChecks
