package evo.derivation.tests

import munit.FunSuite
import data.*
import cats.syntax.order.given

import java.util.UUID

class EvoEqChecks extends FunSuite:
    test("plain product") {
        assert(Person.person === Person.person)
        assert(Person.person =!= Person.person.copy(age = -1))
        assert(Person.person =!= Person.person.copy(name = "zu"))
    }

    test("complex product") {
        assert(Document.document === Document.document)
        assert(Document.document =!= Document.document.copy(id = UUID.randomUUID().nn))
    }

    test("plain coproduct") {
        assert(User.authorized === User.authorized)
        assert(User.authorized =!= User.Authorized("kekeke"))

        def admin = User.Admin("jane", "titan")
        assert(User.Anonymous === User.Anonymous)
        assert(User.authorized =!= User.Anonymous)
        assert(admin === admin)
        assert(admin =!= User.Anonymous)
    }

    test("complex coproduct") {
        assert(Mode.read === Mode.read)
        assert(Mode.write === Mode.write)
        assert(Mode.read =!= Mode.write)
        assert(Mode.write =!= Mode.read)
    }

    test("recursive product") {
        assert(Dictionary.dictionary === Dictionary.dictionary)
        assert(Dictionary.dictionary =!= Dictionary.dictionary.update("a", _ => "abrikos"))
        assert(Dictionary.dictionary =!= Dictionary.dictionary.update("b", _ => "bulka"))
        assert(Dictionary.dictionary === Dictionary.dictionary.update("ya", _ => "yabloko"))
    }

    test("recursive coproduct") {
        assert(BinTree.binTree === BinTree.binTree)
        assert(BinTree.binTree =!= BinTree.binTree.map(_ + 1))
        assert(BinTree.binTree === BinTree.binTree.map(_ + 1).map(_ - 1))
    }
end EvoEqChecks
