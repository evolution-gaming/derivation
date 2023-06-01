package evo.derivation.logstage

import evo.derivation.config.Config
import io.circe.Json
import logstage.LogstageCodec
import izumi.logstage.api.rendering.json.LogstageCirceWriter
import EvoLogTest.{WithProps, OneOf, SimpleRec}
import evo.derivation.{Discriminator, Embed, Rename, SnakeCase}
import izumi.logstage.api.rendering.{LogstageCodec, LogstageWriter}
import io.circe.parser.parse

class LogstageTest extends munit.FunSuite:
    import evo.derivation.logstage.instances.given // arggh

    extension [A](a: A)
        def toLog(using codec: LogstageCodec[A]): Json =
            LogstageCirceWriter.write(codec, a)

    test("simple recursive data") {
        val chebKekLol =
            """
            |{
            |  "bazar" : {
            |    "foo" : {
            |      "bazar" : {
            |        "foo" : {
            |          "bazar" : {
            |            "foo" : {
            |              "no" : {}
            |            },
            |            "param" : "cheb"
            |          }
            |        },
            |        "param" : "kek"
            |      }
            |    },
            |    "param" : "lol"
            |  }
            |}
            |""".stripMargin

        assertEquals(
          parse(chebKekLol),
          Right(
            (SimpleRec.Bar(SimpleRec.Bar(SimpleRec.Bar(SimpleRec.No, "cheb"), "kek"), "lol"): SimpleRec).toLog,
          ),
        )
    }

    test("WithProps") {
        val case1 =
            """
            |{
            |  "type" : "Case1",
            |  "foo" : 1,
            |  "param" : "cheb",
            |  "props" : {
            |    "lol" : "kek",
            |    "sad" : "pet"
            |  }
            |}
            |""".stripMargin

        assertEquals(
          parse(case1),
          Right(WithProps(OneOf.Case1(1, "cheb"), Map("lol" -> "kek", "sad" -> "pet")).toLog),
        )

        val case2 =
            """
            |{
            |  "type" : "Case2",
            |  "foo": 10,
            |  "props" : {}
            |}
            |""".stripMargin

        assertEquals(
          parse(case2),
          Right(WithProps(OneOf.Case2(10), Map()).toLog),
        )
    }

end LogstageTest

object EvoLogTest:
    @SnakeCase
    enum SimpleRec derives Config, EvoLog:
        @Rename("bazar") case Bar(foo: SimpleRec, param: String)
        case No
        case Bazz(i: Int)

    @Discriminator("type")
    enum OneOf derives Config, EvoLog:
        case Case1(foo: Int, param: String)
        case Case2(foo: Int)
        case Case3(param: String)

    case class WithProps(@Embed variant: OneOf, props: Map[String, String]) derives Config, EvoLog
end EvoLogTest
