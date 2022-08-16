package evo.derivation.play.json

import evo.derivation.config.Config
import play.api.libs.json.*

trait EvoFormat[A] extends EvoWrites[A] with EvoReads[A] with Format[A]

object EvoFormat:
    inline def derived[A](using => Config[A]): EvoFormat[A] = EvoFormatImpl(EvoWrites.derived[A], EvoReads.derived[A])

    class EvoFormatImpl[A](writes: Writes[A], reads: Reads[A]) extends EvoFormat[A]:
        export writes.writes
        export reads.reads
end EvoFormat
