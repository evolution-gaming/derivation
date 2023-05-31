package evo.derivation.logstage

import evo.derivation.LazySummon.All
import evo.derivation.{LazySummon, ValueClass}
import evo.derivation.config.{Config, ForField}
import evo.derivation.internal.{Matching, tupleFromProduct}
import evo.derivation.template.{ConsistentTemplate, HomogenicTemplate, SummonHierarchy, Template}
import izumi.logstage.api.rendering.{LogstageCodec, LogstageWriter}
import logstage.LogstageCodec

import scala.deriving.Mirror
import scala.deriving.Mirror.SumOf

// Unable to extend LogstageCodec here due to problems with derivations for contravariant type classes
trait EvoLog[A]:
    def write(writer: LogstageWriter, value: A): Unit

object EvoLog extends EvoLogTemplate:
    def fromLogstage[A](using codec: => LogstageCodec[A]): EvoLog[A] =
        (writer: LogstageWriter, value: A) => codec.write(writer, value)

    given [A](using LogstageCodec[A]): EvoLog[A] = fromLogstage
end EvoLog

trait EvoLogTemplate extends HomogenicTemplate[EvoLog], SummonHierarchy:
    override def product[A](using mirror: Mirror.ProductOf[A])(
        all: LazySummon.All[OfField, mirror.MirroredElemTypes],
    )(using config: => Config[A], ev: A <:< Product): EvoLog[A] =
        lazy val infos = config.top.fields.map(_._2)

        (writer, a) =>
            val fields = tupleFromProduct(a)

            // TODO: support embed
            writer.openMap()
            all.useForeach[Unit, ForField[_]](fields, infos) {
                [X] =>
                    (info: ForField[_], a: X, codec: EvoLog[X]) =>
                        writer.nextMapElementOpen()
                        LogstageCodec[String].write(writer, info.name)
                        writer.mapElementSplitter()
                        codec.write(writer, a)
                        writer.nextMapElementClose()
            }
            writer.closeMap()
    end product

    override def sum[A](using mirror: SumOf[A])(
        subs: All[EvoLog, mirror.MirroredElemTypes],
        mkSubMap: => Map[String, EvoLog[A]],
    )(using config: => Config[A], matching: Matching[A]): EvoLog[A] =
        lazy val cfg                            = config
        lazy val codecs: Map[String, EvoLog[A]] = mkSubMap

        (writer, a) =>
            val constructor  = matching.matched(a)
            val discrimValue = cfg.name(constructor)

            // TODO: support discriminator
            codecs
                .get(constructor)
                .foreach( // throw exception if empty?
                  codec =>
                      writer.openMap()
                      writer.nextMapElementOpen()
                      LogstageCodec[String].write(writer, discrimValue)
                      writer.mapElementSplitter()
                      codec.write(writer, a)
                      writer.nextMapElementClose()
                      writer.closeMap(),
                )
    end sum

    override def newtype[A](using nt: ValueClass[A])(using codec: EvoLog[nt.Representation]): EvoLog[A] =
        (writer, a) => codec.write(writer, nt.to(a))
end EvoLogTemplate
