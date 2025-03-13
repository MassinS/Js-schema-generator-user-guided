package utils

import utils.DataTypes._

object Assertions {

  // Non type specific assertions
  final val AllOf = "allOf"
  final val AnyOf = "anyOf"
  final val OneOf = "oneOf"
  final val Not = "not"
  final val Enum = "enum"
  final val Const = "const"
  final val Type = "type"
  final val Ref = "$ref"
  final val Def = "$defs"
  final val Definitions = "definitions"
  final val If = "if"
  final val Then = "then"
  final val Else = "else"

  final val logicalAssertions : scala.collection.immutable.Set[String] = Set(AllOf,AnyOf,OneOf,Not,If)
  final val NonTypeSpecificAssertions : scala.collection.immutable.Set[String] = Set(AllOf,AnyOf,OneOf,Not,If)

  // number/integer assertions
  final val Minimum = "minimum"
  final val Maximum = "maximum"
  final val ExclusiveMinimum = "exclusiveMinimum"
  final val ExclusiveMaximum = "exclusiveMaximum"
  final val MultipleOf = "multipleOf"

  final val NumberSpecificAssertions : scala.collection.immutable.Set[String] = Set(Minimum,Maximum,ExclusiveMinimum,
    ExclusiveMaximum,MultipleOf)

  // string assertions
  final val MinLength = "minLength"
  final val MaxLength = "maxLength"
  final val Pattern = "pattern"
  final val Format = "format"

  final val FormatRegex : scala.collection.immutable.Map[String, String]= Map("date" -> "^[1-9]{4}-[1-9]{2}-[1-9]{2}$",
    "date-time" -> "^[1-9]{4}-[1-9]{2}-[1-9]{2}T[1-9]{2}:[1-9]{2}:[1-9]{2}\\.[1-9]{3}Z$",
    "email" -> "^[a-zA-Z_.]{2,10}@[a-zA-Z0-9-]{2,10}\\.[a-z]{2,3}$",
    "host-name" -> "^[a-zA-Z0-9-]{2,10}\\.[a-z]{2,3}$",
    "hostname" -> "^[a-zA-Z0-9-]{2,10}\\.[a-z]{2,3}$",
    "idn-email" -> "^[a-zA-Z_.]{2,10}@[a-zA-Z0-9-]{2,10}\\.[a-z]{2,3}$",
    "idn-hostname" -> "^[a-zA-Z0-9-]{2,10}\\.[a-z]{2,3}$",
    "ip-address" -> "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$",
    "ipv4" -> "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$",
    "ipv6" -> "^(?:[A-F0-9]{1,4}:){7}[A-F0-9]{1,4}$",
    "iri" -> "^http:\\/\\/[a-zA-Z0-9_\\-]+\\.[a-zA-Z0-9_\\-]+\\.[a-zA-Z0-9_\\-]+$",
    "json-pointer" -> "^/[a-zA-Z0-9_/-]{2,40}$",
    "regex" -> "\\/([^()]*)?\\/([i|g|m]+)?",
    "relative-json-pointer" -> "^\\d{4}/[a-zA-Z0-9_/-]{2,40}$",
    "time" -> "^[1-9]{2}:[1-9]{2}:[1-9]{2}\\.[1-9]{3}Z$",
    "uri" -> "^http:\\/\\/[a-zA-Z0-9_\\-]+\\.[a-zA-Z0-9_\\-]+\\.[a-zA-Z0-9_\\-]+$",
    "uri-reference" -> "^http:\\/\\/[a-zA-Z0-9_\\-]+\\.[a-zA-Z0-9_\\-]+\\.[a-zA-Z0-9_\\-]+$",
    "uuid" -> "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$",
    "path" -> "^(?:[a-z]:)?[\\/\\\\]{0,2}(?:[.\\/\\\\ ](?![.\\/\\\\\\n])|[^<>:\"|?*.\\/\\\\ \\n])+$",//net
    "source-category" -> "^[a-z0-9]+(?:-[a-z0-9]+)*$",//gpt,
    "html-selector" -> "^[a-zA-Z0-9\\s\\[\\]#._:-]+$",//gpt
    "sha1" -> "^[a-fA-F0-9]{40}$",//gpt
    "url" -> "^(https?|ftp)://[^\\s/$.?#].[^\\s]*$",//gpt
    "only-a" -> "^only-a$", // gpt says it doesn't exist
  )

  final val StringSpecificAssertions : scala.collection.immutable.Set[String] = Set(MinLength,MaxLength,Pattern,Format)


  // object assertions
  final val Required = "required"
  final val Properties = "properties"
  final val PatternProperties = "patternProperties"
  final val AdditionalProperties = "additionalProperties"
  final val PropertyNames = "propertyNames"
  final val MinProperties = "minProperties"
  final val MaxProperties = "maxProperties"
  final val DependentRequired = "dependentRequired"
  final val DependentSchemas = "dependentSchemas"

  final val ObjectSpecificAssertions : scala.collection.immutable.Set[String] = Set(Required,Properties,PatternProperties,
    AdditionalProperties,PropertyNames,MinProperties,MaxProperties,DependentRequired,DependentSchemas)

  // array assertions
  final val Contains = "contains"
  final val MinContains = "minContains"
  final val MaxContains = "maxContains"
  final val PrefixItems = "prefixItems"
  final val Items = "items"
  final val MinItems = "minItems"
  final val MaxItems = "maxItems"
  final val UniqueItems = "uniqueItems"

  final val ArraySpecificAssertions : scala.collection.immutable.Set[String] = Set(Contains,MinContains,MaxContains,
    PrefixItems,Items,MinItems,MaxItems,UniqueItems)



  final val TypeAssertionsMap = scala.collection.immutable.Map(
    JSInteger -> NumberSpecificAssertions,
    JSNumber -> NumberSpecificAssertions,
    JSString -> StringSpecificAssertions,
    JSObject -> ObjectSpecificAssertions,
    JSArray -> ArraySpecificAssertions
  )

  final val assertionsMultipleSchemasToCanonicalize : scala.collection.immutable.Set[String] = Set(Properties,PatternProperties,DependentSchemas)
  final val assertionsSchemaToCanonicalize : scala.collection.immutable.Set[String] = Set(AdditionalProperties,Items,Contains,PropertyNames)
  final val assertionsArrayToCanonicalize : scala.collection.immutable.Set[String] = Set(PrefixItems)



}
