package utils

import com.fasterxml.jackson.core.JsonProcessingException
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.networknt.schema.{JsonSchema, JsonSchemaException, JsonSchemaFactory, SpecVersion, ValidationMessage}

import java.io.IOException
import java.nio.file.{Files, Path}
import java.util
import java.util.regex.Pattern

object InstanceValidation {

  private val p4 = Pattern.compile("/draft-04/")
  private val p6 = Pattern.compile("/draft-06/")
  private val p7 = Pattern.compile("/draft-07/")
  private val regexXbool = Pattern.compile("(\"exclusiveMinimum\"|\"exclusiveMaximum\"):(false|False|true|True)")


  @throws[IOException]
  def getVersionFlag(pathToFile: String): SpecVersion.VersionFlag = {
    var version = SpecVersion.VersionFlag.V201909
    var s = Files.readString(Path.of(pathToFile))
    s = s.replace(" ", "")
    if (p4.matcher(s).find || regexXbool.matcher(s).find) version = SpecVersion.VersionFlag.V4
    else if (p6.matcher(s).find) version = SpecVersion.VersionFlag.V6
    else if (p7.matcher(s).find) version = SpecVersion.VersionFlag.V7
    version
  }


  @throws[JsonSchemaException]
  @throws[JsonProcessingException]
  def validateStringWitness(schemaString: String, witness: String, version: SpecVersion.VersionFlag): util.Set[ValidationMessage] = {
    val factory: JsonSchemaFactory = JsonSchemaFactory.getInstance(version)
    val mapper: ObjectMapper = new ObjectMapper
    var schema: JsonSchema = null
    schema = factory.getSchema(schemaString)
    var node: JsonNode = null
    node = mapper.readTree(witness)
    val errors: util.Set[ValidationMessage] = schema.validate(node)
    errors
  }

}
