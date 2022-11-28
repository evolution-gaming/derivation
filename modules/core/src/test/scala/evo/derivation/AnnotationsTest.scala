package evo.derivation

import AnnotationTest.{Baba, Gosudarstvo, Izbushka, Pech}
import evo.derivation.config.{Config, ForField}
import scala.CanEqual.derived
import evo.derivation.AnnotationTest.Gorynych
import evo.derivation.config.ForProduct

class AnnotationsTest extends munit.FunSuite:
    extension [T](ff: ForField[T]) def noInfo = ff.copy(info = None)

    test("isbuzhka") {
        val cfg = summon[Config[Izbushka]]
        assertEquals(cfg.top.byField("hutHut").name, "hut_hut")
        assertEquals(cfg.top.byField("ping").noInfo, ForField(name = "pong", annotations = Vector(Rename("pong"))))
        assert(cfg.top.byField("azaza").embed)
    }

    test("baba") {
        val cfg = summon[Config[Baba]]
        assertEquals(cfg.byConstructor("FooFoo").name, "foo_foo")
        assertEquals(cfg.byConstructor("Yaga").name, "Jaga")
        assertEquals(cfg.byConstructor("Noga").byField("rightLeg").name, "right_leg")
        assertEquals(cfg.byConstructor("Kostyanaya").byField("kekJi").name, "kek_ji")

    }

    test("baba singleton flag") {
        val cfg = summon[Config[Baba]]
        assert(!cfg.top.isSingleton, "enum Baba is not a singleton")
        assert(cfg.byConstructor("FooFoo").isSingleton, "case object FooFoo is a singleton")
        assert(cfg.byConstructor("Yaga").isSingleton, "case object Yaga is a singleton")
        assert(!cfg.byConstructor("Kostyanaya").isSingleton, "case class Kostyanaya is not a singleton")
    }

    test("pech") {
        val cfg = summon[Config[Pech]]
        assertEquals(cfg.top.name, "Pech")
        assertEquals(cfg.top.byField("Ivan").noInfo, ForField(name = "ivan", annotations = Vector(SnakeCase())))
        assertEquals(cfg.top.byField("Durak").name, "Durak")
    }

    test("gosudarstvo") {
        val cfg = summon[Config[Gosudarstvo]]
        assertEquals(
          cfg.top.fields.map((name, ff) => name -> ff.noInfo),
          Vector(
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

    test("simple field get") {
        val izbushka = Izbushka(ping = "ping")
        val izCfg    = summon[Config[Izbushka]]
        assertEquals(
          izCfg.top match
              case fp: ForProduct[Izbushka, Izbushka] =>
                  fp.byField("ping").info.map(valI => valI.read(izbushka))
          ,
          Some("ping"),
        )
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
