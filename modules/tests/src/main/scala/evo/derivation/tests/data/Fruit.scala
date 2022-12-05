package evo.derivation.tests.data
import evo.derivation.config.Config
import evo.derivation.circe.EvoCodec
import evo.derivation.play.json.EvoFormat
import evo.derivation.cats.EvoEq
import evo.derivation.LazySummon
import io.circe.Decoder

sealed trait Dessert derives Config

sealed trait Fruit extends Dessert derives Config, EvoCodec, EvoFormat, EvoEq

sealed trait Amygdaloideae extends Fruit

enum Prunus extends Amygdaloideae:
    case Cherry, Peach, Apricot

enum Malinae extends Amygdaloideae:
    case Apple, Pear

enum Citrus extends Fruit:
    case Lemon, Orange, Mandarin, Kumquat

enum Milky extends Dessert:
    case TiraMiSu
    case Yoghurt(fat: Double)

// object Fruit:
    // summon[LazySummon["Lemon", Citrus, Decoder, EvoDecoder, Citrus.Lemon.type]](
    //     using LazySummon.byConfig["Lemin", Citrus]
    // )
    // summon[LazySummon["Citrus", Fruit, Decoder, EvoDecoder, Citrus]]
