package evo.derivation.tests

import evo.derivation.config.Config
import evo.derivation.tests.data.{Fruit, Dessert}

class HierarchyConfigChecks extends munit.FunSuite:
    test("Fruit is a simple enum") {
        assert(summon[Config[Fruit]].isSimpleEnum)
    }

    test("Dessert is not a simple enum") {
        assert(!summon[Config[Dessert]].isSimpleEnum)
    }

    test("Fruit constructors are correct") {
        val expected = "Cherry, Peach, Apricot, Apple, Pear, Lemon, Orange, Mandarin, Kumquat"
        assertEquals(
          summon[Config[Fruit]].constructors.map(_._1),
          expected.split(", ").nn.map(_.nn).toVector,
        )
    }

    test("Fruit hierarchy is correct") {
        def buildHierarchy(c: Config[Fruit]): Vector[(String, String)] =
            c.subtypes.flatMap((n, s) => (c.top.name -> n) +: buildHierarchy(s))

        val expected = Set(
          "Fruit"         -> "Amygdaloideae",
          "Fruit"         -> "Citrus",
          "Amygdaloideae" -> "Prunus",
          "Amygdaloideae" -> "Malinae",
          "Prunus"        -> "Cherry",
          "Prunus"        -> "Peach",
          "Prunus"        -> "Apricot",
          "Malinae"       -> "Apple",
          "Malinae"       -> "Pear",
          "Citrus"        -> "Lemon",
          "Citrus"        -> "Orange",
          "Citrus"        -> "Mandarin",
          "Citrus"        -> "Kumquat",
        )

        assertEquals(buildHierarchy(summon).toSet, expected)
    }
end HierarchyConfigChecks
