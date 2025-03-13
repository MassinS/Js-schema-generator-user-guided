package main

import generation.SchemaGenerator.{CANNOT, UNSAT, generate}
import io.circe.Json
import preProcessing.Canonicalization.canonicalizeSchema
import preProcessing.ReferenceExpansion.resolveReferences
import utils.DraftTranslation.translate
import utils.Functions.{extractInstances, readParseJson, readParseJsonApi }

import java.nio.file.Paths


object MainClass {

   private def generateInstances(pathToSchema: String, nbInstances: Int, oneOfAsAnyOf: Boolean): Json = {
    val parsedSchema = readParseJson(pathToSchema,oneOfAsAnyOf)
    val translatedSchema = translate(parsedSchema)
    val resolvedSchema = resolveReferences(translatedSchema, translatedSchema, List())
    val canonicalSchema = canonicalizeSchema(resolvedSchema)
    val instances = generate(canonicalSchema,nbInstances)
    instances
  }

  def generateInstancesAPI(schema : String, nbInstances : Int, oneOfAsAnyOf: Boolean): Json = {
    
    val parsedSchema = readParseJsonApi(schema,oneOfAsAnyOf)
    val translatedSchema = translate(parsedSchema)
    val resolvedSchema = resolveReferences(translatedSchema, translatedSchema, List())
    val canonicalSchema = canonicalizeSchema(resolvedSchema)
    val instances = generate(canonicalSchema,nbInstances)
    instances

  }

  def canoniserSchemaAPI(schema : String): Json = {
    val parsedSchema = readParseJsonApi(schema,oneOfAsAnyOf = true)
    val translatedSchema = translate(parsedSchema)
    val resolvedSchema = resolveReferences(translatedSchema, translatedSchema, List())
    val canonicalSchema = canonicalizeSchema(resolvedSchema)
    canonicalSchema
  }
  
  def main(args: Array[String]): Unit = {

    val pathToSchema = Paths.get(args(0))
    val nbInstances = args(1).toInt


    val start = System.currentTimeMillis
    var instances = generateInstancesAPI(pathToSchema.toString,nbInstances,oneOfAsAnyOf = true)


    val extracted = extractInstances(instances)

    if(extracted.equals(UNSAT) || extracted.equals(CANNOT))
      instances = generateInstancesAPI(pathToSchema.toString,nbInstances,oneOfAsAnyOf = false)


    val total = System.currentTimeMillis - start
    println("execution time : " + total + " ms\n")

    if(extracted.size<nbInstances)
      println("Could not generate "+nbInstances+ " instances. Only generated "+extracted.size)
    extracted.foreach(k => println(k+"\n"))


  }


}
