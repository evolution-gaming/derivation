package evo.derivation.tests

import evo.derivation.play.json.CheckData.{
    User,
    anonymousJson,
    authorized,
    authorizedJson,
    binTree,
    binTreeJson,
    dictionary,
    dictionaryJson,
    document,
    documentJson,
    person,
    personJson,
    read,
    readJson,
    write,
    writeJson,
}
import evo.derivation.play.json.EvoReads
import munit.FunSuite
import play.api.libs.json.{Json, Writes}

import scala.util.Try

extension [A](a: A) def asJson(using writes: Writes[A]) = writes.writes(a)

def parse(str: String) = Try(Json.parse(str)).toEither

class EvoWritesChecks extends FunSuite:

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
        assertEquals(parse(dictionaryJson).toString, Right(dictionary.asJson).toString)
    }

    test("recursive coproduct") {
        assertEquals(parse(binTreeJson), Right(binTree.asJson))
    }
end EvoWritesChecks
