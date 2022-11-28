package evo.derivation.tests.data
import evo.derivation.config.Config

sealed trait Dessert derives Config

sealed trait Fruit extends Dessert derives Config

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
