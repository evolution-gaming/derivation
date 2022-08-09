package evo.derivation
import evo.derivation.internal.{Updater, update, updater}

class UpdaterTest extends munit.FunSuite {

    test("update Person") {
        val name: Updater[Person, String] = updater(_.name)
        val person                        = Person("Johannes", 102)
        assertEquals(name(_ => "Oleg")(person), Person("Oleg", 102))
        assertEquals(update[Person](_.age)(_ / 2)(person), Person("Johannes", 51))
    }
}

case class Person(name: String, age: Int)
