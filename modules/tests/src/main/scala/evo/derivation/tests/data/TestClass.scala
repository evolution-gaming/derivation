package evo.derivation.tests.data

import evo.derivation.config.Config

class TestClass derives Config

object TestClass:
    val DecoderTypeName        = s"evo.derivation.circe.EvoDecoder"
    val ReadsTypeName          = s"evo.derivation.play.json.EvoReads"
    val TestClassName          = s"evo.derivation.tests.data.TestClass"
    val AppliedDecoderTypeName = s"$DecoderTypeName[$TestClassName]"
    val AppliedReadsTypeName   = s"$ReadsTypeName[$TestClassName]"
end TestClass
