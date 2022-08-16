package evo.derivation.tests.data

import evo.derivation.cats.EvoEq
import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}
import evo.derivation.{Discriminator, SnakeCase}

@Discriminator("kind")
@SnakeCase
enum BinTree derives Config, EvoDecoder, EvoEncoder, EvoReads, EvoWrites, EvoEq:
    case Branch(value: Int, left: BinTree, right: BinTree)
    case Nil

    def map(f: Int => Int): BinTree = this match
        case Nil             => Nil
        case Branch(x, l, r) => Branch(f(x), l.map(f), r.map(f))
end BinTree

object BinTree:

    val binTreeJson =
        """{
            "kind" : "branch",
            "value": 1,
            "left": {
              "kind" : "nil"
            },
            "right": {
              "kind" : "branch",
              "value" : 3,
              "left" : {
                "kind" : "nil"
              },
              "right" : {
                "kind" : "nil"
              }
            }
          }"""

    val binTree =
        BinTree.Branch(1, left = BinTree.Nil, right = BinTree.Branch(3, left = BinTree.Nil, right = BinTree.Nil))
end BinTree
