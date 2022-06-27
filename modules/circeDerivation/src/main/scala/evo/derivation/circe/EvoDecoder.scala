package evo.derivation.circe

import io.circe.Decoder
import scala.deriving.Mirror
import evo.derivation.Config
import scala.compiletime.*
import evo.derivation.internal.underiveableError

trait ConfiguredDecoder[A] extends Decoder[A]

object ConfiguredDecoder:
    inline def derived[A](using => Config[A]): ConfiguredDecoder[A] =
        summonFrom {
            case given Mirror.ProductOf[A] => deriveForProduct[A]
            case given Mirror.SumOf[A]     => deriveForSum[A]
            case _                         => underiveableError[ConfiguredDecoder[A], A]
        }

    private inline def deriveForProduct[A](using
        config: => Config[A],
        product: Mirror.ProductOf[A],
    ): ConfiguredDecoder[A] =
        val names = constValueTuple[product.MirroredElemLabels]
        val fieldInstances = LazySummon.all[product.MirroredElemLabels, Decoder, product.MirroredElemTypes]
        ???

    end deriveForProduct

    private inline def deriveForSum[A](using Mirror.SumOf[A]): ConfiguredDecoder[A] = ???