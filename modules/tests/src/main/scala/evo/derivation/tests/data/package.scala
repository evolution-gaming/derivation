package evo.derivation.tests.data

import play.api.libs.json.*

given [A: Reads]: Reads[Option[A]] with
    def reads(json: JsValue): JsResult[Option[A]] = json match
        case JsNull => JsSuccess(None)
        case other  => other.validate[A].map(Some(_))
