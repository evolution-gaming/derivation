package evo.derivation.tests

import evo.derivation.play.json.EvoReads
import munit.FunSuite
import play.api.libs.json.{Json, Writes}
import data.*

import scala.util.Try

class EvoWritesChecks extends FunSuite:

    extension [A](a: A) def asJson(using writes: Writes[A]) = writes.writes(a)

    def parse(str: String) = Try(Json.parse(str)).toEither

    test("plain product") {
        assertEquals(parse(Person.personJson), Right(Person.person.asJson))
    }

    test("complex product") {
        assertEquals(parse(Document.documentJson), Right(Document.document.asJson))
    }

    test("plain coproduct") {
        assertEquals(parse(User.authorizedJson), Right(User.authorized.asJson))

        assertEquals(parse(User.anonymousJson), Right(User.Anonymous.asJson))
    }

    test("complex coproduct") {
        assertEquals(parse(Mode.readJson), Right(Mode.read.asJson))

        assertEquals(parse(Mode.writeJson), Right(Mode.write.asJson))
    }

    test("recursive product") {
        assertEquals(parse(Dictionary.dictionaryJsonWithNull), Right(Dictionary.dictionary.asJson))
    }

    test("recursive coproduct") {
        assertEquals(parse(BinTree.binTreeJson), Right(BinTree.binTree.asJson))
    }
end EvoWritesChecks
