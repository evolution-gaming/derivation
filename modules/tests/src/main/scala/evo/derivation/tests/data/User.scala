package evo.derivation.tests.data

import evo.derivation.Rename
import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}

enum User derives Config, EvoDecoder, EvoEncoder, EvoReads, EvoWrites:
    case Authorized(login: String)
    case Anonymous
    case Admin(login: String, @Rename("access") rights: String)

object User:

    val authorized = User.Authorized("ololo")

    val admin = User.Admin("ololo", "kek")

    val authorizedJson = s"""{"Authorized" : {"login": "ololo"}}"""

    val adminJson = s"""{"Admin" : {"login": "ololo", "access": "kek"}}"""

    val anonymousJson = s"""{"Anonymous" : {}}"""
end User
