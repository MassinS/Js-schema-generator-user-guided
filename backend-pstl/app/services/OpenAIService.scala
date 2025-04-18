package services 

import scalaj.http.Http
import play.api.libs.json._
import play.api.Configuration
import javax.inject.Inject
import scala.concurrent.{Future, ExecutionContext}
import play.api.Logger 

class OpenAIService @Inject()(config: Configuration)(implicit ec: ExecutionContext) {
  private val logger = Logger(this.getClass)
  private val apiKey = config.get[String]("openai.apiKey")
  private val apiUrl = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent"

  def generateFromSchema(schema: JsValue, numInstances: Int): Future[JsValue] = Future {
    val prompt = 
      s"""Générez $numInstances exemples JSON valides pour ce schéma. 
         |Retournez uniquement un tableau JSON sans commentaires.
         |
         |Schéma :
         |${Json.prettyPrint(schema)}""".stripMargin

    val fullUrl = s"$apiUrl?key=$apiKey"

    val response = Http(fullUrl)
      .header("Content-Type", "application/json")
      .postData(
        Json.obj(
          "contents" -> Json.arr(
            Json.obj(
              "parts" -> Json.arr(
                Json.obj("text" -> prompt)
              )
            )
          ),
          "generationConfig" -> Json.obj(
            "temperature" -> 0.3,
            "maxOutputTokens" -> (2000 + numInstances * 100)
          )
        ).toString()
      )
      .timeout(5000, 30000)
      .asString
      

    if(response.isError) {
      throw new Exception(s"API Error: ${response.body}")
    }

    val responseJson = Json.parse(response.body)
   val generatedText = (responseJson \ "candidates")(0) \ "content" \ "parts" \ 0 \ "text"

    generatedText.asOpt[String] match {
    case Some(text) =>
      // Nettoyer le texte (retirer les backticks et markdown)
      val cleanJson = text.replace("```json", "").replace("```", "").trim
      
      Json.parse(cleanJson) // Convertir le texte en JsValue
      
    case None =>
      logger.error(s"Réponse inattendue: ${response.body.take(500)}")
      throw new Exception("Format de réponse Gemini invalide")

    }
  }
}