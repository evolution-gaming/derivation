package evo.derivation

import evo.derivation.ValueClassTest.AAA

class ValueClassTest extends munit.FunSuite:
    val aaaValue = ValueClass.derived[AAA]

    test("from") {
        assertEquals(aaaValue.to(AAA("123")), "123")
    }

    test("to") {
        assertEquals(aaaValue.from("123"), AAA("123"))
    }

end ValueClassTest

object ValueClassTest:
    case class AAA(xval: String) extends AnyVal

def lolKek[X <: Int](using X: ValueOf[X])(x: X) = x
