package utils

object DataTypes {

  // JSON Schema data types
  final val JSNull = "null"
  final val JSBool = "boolean"
  final val JSInteger = "integer"
  final val JSNumber = "number"
  final val JSString = "string"
  final val JSObject = "object"
  final val JSArray = "array"

  final val JSTypes: scala.collection.immutable.Set[String] = Set(JSNull, JSBool, JSInteger, JSNumber, JSString, JSObject,
    JSArray)


  // names of the data types used in the circe parser
  final val ParserNull = "Null"
  final val ParserBool = "Boolean"
  final val ParserNumber = "Number"
  final val ParserString = "String"
  final val ParserObject = "Object"
  final val ParserArray = "Array"


  final val TypesMap: scala.collection.immutable.Map[String,String] = Map(
    JSNull -> ParserNull,
    JSBool -> ParserBool,
    JSInteger -> ParserNumber,
    JSNumber -> ParserNumber,
    JSString -> ParserString,
    JSObject -> ParserObject,
    JSArray -> ParserArray
  )

}
