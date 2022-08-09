package evo.derivation.tests.data

import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}
import evo.derivation.{Discriminator, SnakeCase}

@Discriminator("kind")
@SnakeCase
enum BinTree derives Config, EvoDecoder, EvoEncoder, EvoReads, EvoWrites:
    case Branch(value: Int, left: BinTree, right: BinTree)
    case Nil

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
