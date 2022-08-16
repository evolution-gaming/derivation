package evo.derivation.tests

import munit.FunSuite

import scala.compiletime.testing.typeCheckErrors
import evo.derivation.play.json.EvoReads
import evo.derivation.tests.data.TestClass.*
import play.api.libs.json.Json
import data.*

import scala.util.Try

class EvoReadsChecks extends FunSuite:

    def decode[A: EvoReads](str: String) = Try(Json.parse(str).validate[A].asEither).toEither.flatten

    test("TestClass is not derivable because it is not a case class nor a enum") {
        assertEquals(
          List(s"could not derive $AppliedReadsTypeName, look's like $TestClassName is neither case class or enum"),
          typeCheckErrors("EvoReads.derived[TestClass]").map(_.message),
        )
    }

    test("plain product") {
        assertEquals(decode[Person](Person.personJson), Right(Person.person))
    }

    test("complex product") {
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
end EvoReadsChecks
