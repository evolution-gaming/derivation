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
    def writeInner(writer: LogstageWriter, value: A): Unit

object EvoLog extends EvoLogTemplate:
    def fromLogstage[A](using codec: => LogstageCodec[A]): EvoLog[A] =
        new EvoLog[A]:
            override def writeInner(writer: LogstageWriter, value: A): Unit = codec.write(writer, value)
            override def write(writer: LogstageWriter, value: A): Unit      = codec.write(writer, value)

    given [A](using LogstageCodec[A]): EvoLog[A] = fromLogstage
end EvoLog

trait EvoLogTemplate extends HomogenicTemplate[EvoLog], SummonHierarchy:
    override def product[A](using mirror: Mirror.ProductOf[A])(
        all: LazySummon.All[OfField, mirror.MirroredElemTypes],
    )(using config: => Config[A], ev: A <:< Product): EvoLog[A] =
        lazy val infos = config.top.fields.map(_._2)

        new EvoLog[A]:
            override def write(writer: LogstageWriter, value: A): Unit =
                writer.openMap()
                writeInner(writer, value)
                writer.closeMap()

            override def writeInner(writer: LogstageWriter, value: A): Unit =
                val fields = tupleFromProduct(value)

                all.useForeach[Unit, ForField[_]](fields, infos) {
                    [X] =>
                        (info: ForField[_], a: X, codec: EvoLog[X]) =>
                            if (info.embed) codec.writeInner(writer, a)
                            else
                                writer.nextMapElementOpen()
                                LogstageCodec[String].write(writer, info.name)
                                writer.mapElementSplitter()
                                codec.write(writer, a)
                                writer.nextMapElementClose()
                }
            end writeInner
        end new
    end product

    override def sum[A](using mirror: SumOf[A])(
        subs: All[EvoLog, mirror.MirroredElemTypes],
        mkSubMap: => Map[String, EvoLog[A]],
    )(using config: => Config[A], matching: Matching[A]): EvoLog[A] =
        lazy val cfg                            = config
        lazy val codecs: Map[String, EvoLog[A]] = mkSubMap

        new EvoLog[A]:
            override def write(writer: LogstageWriter, value: A): Unit =
                writer.openMap()
                writeInner(writer, value)
                writer.closeMap()

            override def writeInner(writer: LogstageWriter, value: A): Unit =
                val constructor  = matching.matched(value)
                val discrimValue = cfg.name(constructor)

                (codecs.get(constructor), config.discriminator) match
                    case (Some(codec), Some(discr)) =>
                        writer.nextMapElementOpen()
                        LogstageCodec[String].write(writer, discr)
                        writer.mapElementSplitter()
                        LogstageCodec[String].write(writer, discrimValue)
                        writer.nextMapElementClose()
                        codec.writeInner(writer, value)
                    case (Some(codec), None)        =>
                        writer.nextMapElementOpen()
                        LogstageCodec[String].write(writer, discrimValue)
                        writer.mapElementSplitter()
                        codec.write(writer, value)
                        writer.nextMapElementClose()
                    case (_, _)                     => () // throw exception
                end match
            end writeInner
        end new
    end sum

    override def newtype[A](using nt: ValueClass[A])(using codec: EvoLog[nt.Representation]): EvoLog[A] =
        new EvoLog[A]:
            override def write(writer: LogstageWriter, value: A): Unit =
                codec.write(writer, nt.to(value))

            override def writeInner(writer: LogstageWriter, value: A): Unit =
                codec.write(writer, nt.to(value))
end EvoLogTemplate
