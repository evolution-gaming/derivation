package evo.derivation.circe

import evo.derivation.Config
import io.circe.Decoder.AccumulatingResult
import io.circe.{Codec, Decoder, Encoder, HCursor, Json}

trait EvoCodec[A] extends EvoEncoder[A] with EvoDecoder[A] with Codec.AsObject[A]

object EvoCodec:
    inline def derived[A](using => Config[A]): EvoCodec[A] =
        val encoder = EvoEncoder.derived[A]
        val decoder = EvoDecoder.derived[A]

        EvoCodecImpl(decoder, encoder)

    class EvoCodecImpl[A](decoder: Decoder[A], encoder: Encoder.AsObject[A]) extends EvoCodec[A]:
        export decoder.apply
        export encoder.encodeObject

        override def decodeAccumulating(c: HCursor) = decoder.decodeAccumulating(c)
end EvoCodec
