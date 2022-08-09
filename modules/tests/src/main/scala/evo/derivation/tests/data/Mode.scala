package evo.derivation.tests.data

import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}
import evo.derivation.{Discriminator, Rename}

@Discriminator("mode")
enum Mode derives Config, EvoDecoder, EvoEncoder, EvoReads, EvoWrites:
    @Rename("r") case Read(@Rename("b") bin: Boolean)
    @Rename("w") case Write(append: Boolean = false, bin: Boolean)

object Mode:
    val readJson = s"""{"mode": "r", "b": false}"""

    val read = Mode.Read(false)

    val write = Mode.Write(append = true, bin = true)

    val writeJson = s"""{"mode" : "w", "append":true, "bin": true}"""
end Mode
