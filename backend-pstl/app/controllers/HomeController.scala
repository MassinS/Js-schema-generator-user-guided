package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import main.MainClass
import io.circe.Json
import io.circe.parser._
import play.api.libs.json._

@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  def generate() = Action(parse.json) { implicit request =>
    (request.body \ "schema").validate[JsValue].flatMap { schemaJson =>
      (request.body \ "count").validate[Int].map { count =>
        val schemaString = schemaJson.toString() // Convertir en string
        val generatedInstances: Json = MainClass.generateInstancesAPI(schemaString, count, oneOfAsAnyOf = true)

        Ok(generatedInstances.noSpaces).as("application/json")
      }
    }.recoverTotal { e =>
      BadRequest("Invalid JSON: " + e)
    }
  }
   
  def renvoieSchemaCanoniser() = Action(parse.json) { implicit request =>
    (request.body \ "schema").validate[JsValue].map { schemaJson =>
      val schemaString = schemaJson.toString() // Convertir en string
      val schemaCanoniser: Json = MainClass.canoniserSchemaAPI(schemaString)

      Ok(schemaCanoniser.noSpaces).as("application/json")
    }.recoverTotal { e =>
      BadRequest("Invalid JSON: " + e)
    }
  }
  
}
