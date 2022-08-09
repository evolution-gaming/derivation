package evo.derivation.circe

import evo.derivation.config.Config
import io.circe.Decoder.AccumulatingResult
import io.circe.{Codec, Decoder, Encoder, HCursor, Json}

trait EvoCodec[A] extends EvoEncoder[A] with EvoDecoder[A] with Codec[A]

object EvoCodec:
    inline def derived[A](using => Config[A]): EvoCodec[A] =
        val encoder = EvoEncoder.derived[A]
        val decoder = EvoDecoder.derived[A]

        EvoCodecImpl(decoder, encoder)

    class EvoCodecImpl[A](decoder: Decoder[A], encoder: Encoder[A]) extends EvoCodec[A]:
        export decoder.apply

        def apply(a: A): Json = encoder(a)

        override def decodeAccumulating(c: HCursor) = decoder.decodeAccumulating(c)
    end EvoCodecImpl
end EvoCodec

trait EvoObjectCodec[A] extends EvoObjectEncoder[A] with EvoDecoder[A] with Codec.AsObject[A]

object EvoObjectCodec:
    inline def derived[A](using => Config[A]): EvoObjectCodec[A] =
        val encoder = EvoObjectEncoder.derived[A]
        val decoder = EvoDecoder.derived[A]

        EvoCodecImpl(decoder, encoder)

    class EvoCodecImpl[A](decoder: Decoder[A], encoder: Encoder.AsObject[A]) extends EvoObjectCodec[A]:
        export decoder.apply
        export encoder.encodeObject

        override def decodeAccumulating(c: HCursor) = decoder.decodeAccumulating(c)
    end EvoCodecImpl
end EvoObjectCodec
