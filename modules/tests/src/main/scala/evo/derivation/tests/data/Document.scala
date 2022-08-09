package evo.derivation.tests.data

import evo.derivation.*
import evo.derivation.cats.EvoEq
import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}
import evo.derivation.tests.data.Person

import java.time.Instant
import java.util.UUID

import _root_.cats.instances.order.given
@SnakeCase
case class Document(
    @Rename("documentId") id: UUID,
    issueDate: Instant,
    @Embed author: Person,
) derives Config,
      EvoDecoder,
      EvoEncoder,
      EvoReads,
      EvoWrites,
      EvoEq

object Document:

    val uuid = UUID.fromString("68ede874-fb8a-11ec-a827-00155d6320ce").nn
    val date = Instant.now.nn

    val document = Document(id = uuid, issueDate = date, author = Person(name = "alala", age = 74))

    val documentJson = s"""{"documentId": "$uuid", "issue_date": "$date", "name": "alala", "age": 74}"""
end Document
