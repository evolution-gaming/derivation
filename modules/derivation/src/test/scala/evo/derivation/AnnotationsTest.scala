package evo.derivation

import AnnotationTest.Izbushka

class AnnotationsTest extends munit.FunSuite {
    test("isbuzhka") {
        val cfg = summon[Config[Izbushka]]
        assertEquals(cfg.top.fieldRenaming, Config.snakeCase)
    }
}

object AnnotationTest {
    @SnakeCase()
    case class Izbushka() derives Config
}
