package controllers

import javax.inject._
import play.api.mvc._
import play.api.libs.json._
import services.OpenAIService
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import play.api.Logger

@Singleton
class OpenAIController @Inject()(
  cc: ControllerComponents,
  openAIService: OpenAIService
)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  private val logger = Logger(this.getClass)

  def generateInstances = Action.async(parse.json) { request =>
    // Extraction des deux paramètres de la requête
    val schemaResult = (request.body \ "schema").validate[JsValue]
    val numInstancesResult = (request.body \ "numInstances").validate[Int]

    (schemaResult, numInstancesResult) match {
      case (JsSuccess(schema, _), JsSuccess(num, _)) if num > 0  =>
        openAIService.generateFromSchema(schema, num)
          .map { instances =>
            logger.info(s"Génération de $num instances réussie")
            Ok(instances)
          }
          .recover {
            case e: Exception =>
              logger.error(s"Échec génération : ${e.getMessage}", e)
              InternalServerError(Json.obj("error" -> e.getMessage))
          }

      // Cas où le nombre d'instances est invalide
      case (_, JsSuccess(num, _)) if num <= 0  =>
        val errorMsg = "Le nombre d'instances doit être superieur strictement à 0"
        logger.warn(errorMsg)
        Future.successful(BadRequest(Json.obj("error" -> errorMsg)))

      // Cas d'erreur de validation générale
      case _ =>
        val errorMsg = "Format de requête invalide - attendu : { schema: JSON, numInstances: Int }"
        logger.warn(errorMsg)
        Future.successful(BadRequest(Json.obj("error" -> errorMsg)))
    }
  }
}