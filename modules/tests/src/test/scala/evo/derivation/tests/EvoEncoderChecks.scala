package evo.derivation.tests

import munit.FunSuite
import io.circe.parser.*
import io.circe.syntax.given
import data.*

class EvoEncoderChecks extends FunSuite:
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
        assertEquals(parse(Dictionary.dictionaryJson), Right(Dictionary.dictionary.asJson.deepDropNullValues))
    }

    test("recursive coproduct") {
        assertEquals(parse(BinTree.binTreeJson), Right(BinTree.binTree.asJson))
    }
end EvoEncoderChecks
