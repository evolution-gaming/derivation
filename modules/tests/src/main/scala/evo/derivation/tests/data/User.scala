package evo.derivation.tests.data

import evo.derivation.Rename
import evo.derivation.cats.EvoEq
import evo.derivation.circe.{EvoDecoder, EvoEncoder}
import evo.derivation.config.Config
import evo.derivation.play.json.{EvoReads, EvoWrites}


final case class Login(value: String) extends AnyVal derives Config, EvoDecoder, EvoEncoder, EvoReads, EvoWrites, EvoEq

enum User derives Config, EvoDecoder, EvoEncoder, EvoReads, EvoWrites, EvoEq:
    case Authorized(login: Login)
    case Anonymous
    case Admin(login: Login, @Rename("access") rights: String)

object User:

    val authorized = User.Authorized(Login("ololo"))

    val admin = User.Admin(Login("ololo"), "kek")

    val authorizedJson = s"""{"Authorized" : {"login": "ololo"}}"""

    val adminJson = s"""{"Admin" : {"login": "ololo", "access": "kek"}}"""

    val anonymousJson = s"""{"Anonymous" : {}}"""
end User
