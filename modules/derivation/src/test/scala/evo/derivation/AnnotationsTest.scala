package evo.derivation

import AnnotationTest.Izbushka
import evo.derivation.AnnotationTest.Baba

class AnnotationsTest extends munit.FunSuite {
    test("isbuzhka") {
        val cfg = summon[Config[Izbushka]]
        assertEquals(cfg.top.fieldRenaming, Config.snakeCase)
        assertEquals(cfg.top.fieldNames, Map("pupa" -> "lupa"))
        assertEquals(cfg.top.embedFields, Set("azaza"))
    }

    test("baba") {
        val cfg = summon[Config[Baba]]
    }
}

object AnnotationTest {
    @SnakeCase()
    case class Izbushka(
        @Rename("lupa") pupa: String,
        @Embed azaza: Double,
    ) derives Config

    enum Baba derives Config:
        case Yaga
        case Kostyanaya(lol: String, kek: Double)
        case Noga(cheburek: Izbushka)
}
