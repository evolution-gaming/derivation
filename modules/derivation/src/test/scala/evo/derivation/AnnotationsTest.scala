package evo.derivation

import AnnotationTest.{Baba, Izbushka, Pech, Gosudarstvo}

class AnnotationsTest extends munit.FunSuite:
    test("isbuzhka") {
        val cfg = summon[Config[Izbushka]]
        assertEquals(cfg.top.fieldRenaming("FooFoo"), ("foo_foo"))
        assertEquals(cfg.top.transformedFields, Map("ping" -> "pong"))
        assertEquals(cfg.top.embedFields, Set("azaza"))
    }

    test("baba") {
        val cfg = summon[Config[Baba]]
        assertEquals(cfg.constructorRenaming("FooFoo"), "foo_foo")
        assertEquals(cfg.constructors("Yaga").renamed, Some("Jaga"))
        assertEquals(cfg.constructors("Noga").fieldRenaming("AbCd"), "ab_cd")
        assertEquals(cfg.constructors("Kostyanaya").fieldRenaming("AbCd"), "ab_cd")
    }

    test("pech") {
        val cfg = summon[Config[Pech]]
        assertEquals(cfg.constructorRenaming("Pech"), "Pech")
        assertEquals(cfg.top.transformedFields, Map("Ivan" -> "ivan"))
        assertEquals(cfg.top.fieldRenaming("Durak"), "Durak")
    }

    test("gosudarstvo") {
        val cfg = summon[Config[Gosudarstvo]]
        assertEquals(
          cfg.top.transformedFields,
          Map(
            "tridesatoyeGosudarstvo"  -> "TridesatoyeGosudarstvo",
            "tridevyatoyeGosudarstvo" -> "tridevyatoye-gosudarstvo",
          ),
        )
    }



object AnnotationTest:
    @SnakeCase
    case class Izbushka(
        @Rename("pong") ping: String,
        @Embed azaza: Double = 0,
    ) derives Config

    @SnakeCase
    enum Baba derives Config:
        @Rename("Jaga") case Yaga
        case Kostyanaya(lolJe: String = "", kekJi: Double = 0)
        case Noga(@Embed cheburek: Izbushka)

    case class Pech(
        @SnakeCase Ivan: String,
        Durak: true,
    ) derives Config

    case class Gosudarstvo(
        @PascalCase() tridesatoyeGosudarstvo: String,
        @KebabCase() tridevyatoyeGosudarstvo: String,
    ) derives Config
