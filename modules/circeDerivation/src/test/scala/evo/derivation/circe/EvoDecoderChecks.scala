package evo.derivation.circe

import scala.compiletime.testing.typeCheckErrors
import scala.compiletime.testing.Error

import evo.derivation.Config
import munit.FunSuite
import EvoDecoderChecks.Hmm

import EvoDecoderChecks.{HmmTName, ImplicitTName}

class EvoDecoderChecks extends FunSuite:
    test("Hmm is not deriveable") {
        assertEquals(
          List(s"could not derive $ImplicitTName, look's like $HmmTName is neither case class or enum"),
          typeCheckErrors("ConfiguredDecoder.derived[Hmm]").map(_.message),
        )
    }

object EvoDecoderChecks:
    val Package = "evo.derivation.circe"
    val DecoderTName = s"$Package.ConfiguredDecoder"
    val HmmTName = s"$Package.EvoDecoderChecks.Hmm"
    val ImplicitTName = s"$DecoderTName[$HmmTName]"
    class Hmm derives Config
