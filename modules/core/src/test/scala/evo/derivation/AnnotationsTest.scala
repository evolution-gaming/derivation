package evo.derivation

import AnnotationTest.{Baba, Gosudarstvo, Izbushka, Pech}
import evo.derivation.config.{Config, ForField}
import scala.CanEqual.derived
import evo.derivation.AnnotationTest.Gorynych

class AnnotationsTest extends munit.FunSuite:
    test("isbuzhka") {
        val cfg = summon[Config[Izbushka]]
        assertEquals(cfg.top.fields("hutHut").name, "hut_hut")
        assertEquals(cfg.top.fields("ping"), ForField(name = "pong", annotations = Vector(Rename("pong"))))
        assert(cfg.top.fields("azaza").embed)
    }

    test("baba") {
        val cfg = summon[Config[Baba]]
        assertEquals(cfg.constructors("FooFoo").name, "foo_foo")
        assertEquals(cfg.constructors("Yaga").name, "Jaga")
        assertEquals(cfg.constructors("Noga").fields("rightLeg").name, "right_leg")
        assertEquals(cfg.constructors("Kostyanaya").fields("kekJi").name, "kek_ji")

    }

    test("baba singleton flag") {
        val cfg = summon[Config[Baba]]
        assert(!cfg.top.isSingleton, "enum Baba is not a singleton")
        assert(cfg.constructors("FooFoo").isSingleton, "case object FooFoo is a singleton")
        assert(cfg.constructors("Yaga").isSingleton, "case object Yaga is a singleton")
        assert(!cfg.constructors("Kostyanaya").isSingleton, "case class Kostyanaya is not a singleton")
    }

    test("pech") {
        val cfg = summon[Config[Pech]]
        assertEquals(cfg.top.name, "Pech")
        assertEquals(cfg.top.fields("Ivan"), ForField(name = "ivan", annotations = Vector(SnakeCase())))
        assertEquals(cfg.top.fields("Durak").name, "Durak")
    }

    test("gosudarstvo") {
        val cfg = summon[Config[Gosudarstvo]]
        assertEquals(
          cfg.top.fields,
          Map(
            "tridesatoyeGosudarstvo"  -> ForField(name = "TridesatoyeGosudarstvo", annotations = Vector(PascalCase())),
            "tridevyatoyeGosudarstvo" -> ForField(name = "tridevyatoye-gosudarstvo", annotations = Vector(KebabCase())),
          ),
        )
    }

    test("simple enum flag") {
        assert(!summon[Config[Izbushka]].isSimpleEnum, "case class is not a simple enum")
        assert(!summon[Config[Baba]].isSimpleEnum, "enum with non-object cases is not a simple enum")
        assert(summon[Config[Gorynych]].isSimpleEnum, "enum with only object cases is a simple enum")
    }

end AnnotationsTest

object AnnotationTest:
    @SnakeCase
    case class Izbushka(
        @Rename("pong") ping: String,
        @Embed azaza: Double = 0,
        hutHut: String = "",
    ) derives Config

    @SnakeCase
    enum Baba derives Config:
        @Rename("Jaga") case Yaga
        case Kostyanaya(lolJe: String = "", kekJi: Double = 0)
        case Noga(@Embed cheburek: Izbushka, rightLeg: Boolean = true)
        case FooFoo
    end Baba

    case class Pech(
        @SnakeCase Ivan: String,
        Durak: true,
    ) derives Config

    case class Gosudarstvo(
        @PascalCase() tridesatoyeGosudarstvo: String,
        @KebabCase() tridevyatoyeGosudarstvo: String,
    ) derives Config

    enum Gorynych derives Config:
        case FirstHead, SecondHead, ThirdHead
end AnnotationTest
