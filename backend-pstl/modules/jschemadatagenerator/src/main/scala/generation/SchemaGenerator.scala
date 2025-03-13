package generation

import generation.ArrayGenerator.generateArray
import generation.BasicTypesGenerator.{generateBoolean, generateNull, generateNumber, generateString}
import generation.ObjectGenerator.generateObject
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import utils.Assertions.{AnyOf, Const, Enum, Type}
import utils.DataTypes.{JSArray, JSBool, JSInteger, JSNull, JSNumber, JSObject, JSString, ParserBool, ParserObject}
import utils.Functions.{cannotGenerate, extractInstances}

import scala.util.control.Breaks.{break, breakable}

object SchemaGenerator {

  final val UNSAT = "UNSAT".asJson
  final val DEFAULTVALUE = 123.asJson
  final val CANNOT = "CANNOT GENERATE".asJson

  final val GENNULL = "NullGenerator"
  final val GENBOOL = "BooleanGenerator"
  final val GENNUMBER = "NumberGenerator"
  final val GENSTRING = "StringGenerator"
  final val GENOBJECT = "ObjectGenerator"
  final val GENARRAY = "ArrayGenerator"
  final val GENSCHEMA = "SchemaGenerator"

  final val GENNULLUNSAT = JsonObject.empty.add(GENNULL, UNSAT.asJson).asJson
  final val GENBOOLUNSAT = JsonObject.empty.add(GENBOOL, UNSAT.asJson).asJson
  final val GENNUMBERUNSAT = JsonObject.empty.add(GENNUMBER, UNSAT.asJson).asJson
  final val GENSTRINGUNSAT = JsonObject.empty.add(GENSTRING, UNSAT.asJson).asJson
  final val GENOBJECTUNSAT = JsonObject.empty.add(GENOBJECT, UNSAT.asJson).asJson
  final val GENARRAYUNSAT = JsonObject.empty.add(GENARRAY, UNSAT.asJson).asJson
  final val SCHEMAUNSAT = JsonObject.empty.add(GENSCHEMA, UNSAT.asJson).asJson

  final val GENOBJECTCANNOT = JsonObject.empty.add(GENOBJECT, CANNOT.asJson).asJson
  final val GENARRAYCANNOT = JsonObject.empty.add(GENARRAY, CANNOT.asJson).asJson
  final val SCHEMACANNOT = JsonObject.empty.add(GENSCHEMA, CANNOT.asJson).asJson

  final val GENERATORSUNSAT = List(GENNULLUNSAT, GENBOOLUNSAT, GENNUMBERUNSAT, GENSTRINGUNSAT, GENOBJECTUNSAT, GENARRAYUNSAT, SCHEMAUNSAT)

  final val GENERATORSCANNOT = List(GENOBJECTCANNOT, GENARRAYCANNOT, SCHEMACANNOT)

  final val DEFAULTBASICVALUES = List(DEFAULTVALUE, Json.Null, false.asJson, true.asJson)


  def generate(s: Json, n: Int): Json = {
    var nbIntsances = n

    if (cannotGenerate(s)) return SCHEMACANNOT

    s.name match {
      case ParserBool =>
        if (s.asBoolean.get) {
          var instances = DEFAULTBASICVALUES.take(n)
          if (instances.size < n)
            for (i <- 0 until n - instances.size)
              instances = instances :+ i.asJson

          JsonObject.singleton(GENSCHEMA, JsonObject.singleton("Instances", instances.asJson).asJson).asJson
        }
        else SCHEMAUNSAT

      case ParserObject =>
        val sAsObj = s.asObject.get

        if (sAsObj.contains(Const) && !sAsObj.contains(Type)) {
          var results = JsonObject.singleton("Instances", List(sAsObj.apply(Const).get).asJson)
          if (n > 1) results = results.add("Note", ("Cannot generate " + n + " values because of const").asJson)
          return JsonObject.singleton(GENSCHEMA, results.asJson).asJson
        }

        if (sAsObj.contains(Enum) && !sAsObj.contains(Type)) {
          val enum = sAsObj.apply(Enum).get.asArray.get
          val instances = enum.toList.take(n)
          var results = JsonObject.singleton("Instances", instances.asJson)
          if (n > enum.size)
            results = results.add("Note", ("Only generated " + enum.size +
              " instances. Cannot generate " + n + " values because of enum").asJson)

          return JsonObject.singleton(GENSCHEMA, results.asJson).asJson
        }

        if (sAsObj.contains(Type)) {
          sAsObj.apply(Type).get.asString.get match {
            case JSNull => generateNull(s, nbIntsances)
            case JSBool => generateBoolean(s, nbIntsances)
            case JSNumber | JSInteger => generateNumber(s, nbIntsances)
            case JSString => generateString(s, nbIntsances)
            case JSObject => generateObject(s, nbIntsances)
            case JSArray => generateArray(s, nbIntsances)
          }
        }
        else {
          var instances = List[Json]()

          if (sAsObj.contains(AnyOf)) {
            var booleanUnsat = true
            breakable {
              sAsObj.apply(AnyOf).get.asArray.get.foreach {
                schema =>
                  val values = generate(schema, nbIntsances)
                  val valuesList: List[Json] = extractInstances(values)
                  if (!(valuesList.size == 1 && (valuesList.head.equals(UNSAT) || valuesList.head.equals(CANNOT)))) {
                    instances = instances ++ valuesList
                    nbIntsances -= valuesList.size
                    if (nbIntsances == 0)
                      break
                  }
                  else {
                    booleanUnsat = booleanUnsat && (valuesList.size == 1 && valuesList.head.equals(UNSAT))
                  }
              }
            }
            if (instances.isEmpty)
              if (booleanUnsat) SCHEMAUNSAT
              else SCHEMACANNOT

            else {
              var results = JsonObject.singleton("Instances", instances.asJson)
              if (n > instances.size)
                results = results.add("Note", ("Only generated " + instances.size +
                  " instances. Did not generate " + n + " values").asJson)
              JsonObject.singleton(GENSCHEMA, results.asJson).asJson
            }
          }

          else SCHEMACANNOT

        }
    }
  }

}
