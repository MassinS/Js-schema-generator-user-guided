package generation

import generation.SchemaGenerator.{GENBOOL, GENBOOLUNSAT, GENNULL, GENNULLUNSAT, GENNUMBER, GENNUMBERUNSAT, GENSTRING, GENSTRINGUNSAT, UNSAT}
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import utils.Assertions.{Const, Enum, Maximum, Minimum, MultipleOf, Not, Pattern, Type}
import utils.DataTypes.{JSInteger, ParserBool, ParserString, TypesMap}
import utils.Functions.{getNewPattern, isMultiple, lcm, nearestMultipleToMax, nearestMultipleToMin, patternToAutomaton}
import utils.UNSATPreconditions.{boolUnsat, constUnsat, nullUnsat, numberUnsat, stringUnsat}

import scala.util.control.Breaks.{break, breakable}

object BasicTypesGenerator {

  final val epsilon = 0.0000001d

  def generateNull(s: Json, n: Int): Json = {
    if (nullUnsat(s))
      return GENNULLUNSAT

    var results = JsonObject.singleton("Instances", List(Json.Null).asJson)
    if (n > 1) results = results.add("Note", ("We cannot generate " + n + " values." + " There is only 1 null value").asJson)

    JsonObject.singleton(GENNULL, results.asJson).asJson
  }


  def generateBoolean(s: Json, n: Int): Json = {
    val sAsObj = s.asObject.get

    if (boolUnsat(s)) return GENBOOLUNSAT

    if (sAsObj.contains(Const)) {
      var results = JsonObject.singleton("Instances", List(sAsObj.apply(Const).get).asJson)
      if (n > 1) results = results.add("Note", ("Cannot generate " + n + " values because of const").asJson)
      return JsonObject.singleton(GENBOOL, results.asJson).asJson
    }

    if (sAsObj.contains(Enum)) {
      val filteredEnum = sAsObj.apply(Enum).get.asArray.get.filter(e => e.name.equals(ParserBool))
      val instances = filteredEnum.toList.take(n)
      var results = JsonObject.singleton("Instances", instances.asJson)
      if (n > filteredEnum.size)
        results = results.add("Note", ("Only generated " + filteredEnum.size +
          " instances. Cannot generate " + n + " values because of enum").asJson)
      return JsonObject.singleton(GENBOOL, results.asJson).asJson
    }
    val booleans = List(true, false)
    val instances = booleans.take(n)
    var results = JsonObject.singleton("Instances", instances.asJson)
    if (n > 2)
      results = results.add("Note", ("Only generated 2" +
        " instances. Cannot generate " + n + " because there are only 2 boolean values").asJson)
    JsonObject.singleton(GENBOOL, results.asJson).asJson
  }


  def generateNumber(s: Json, n: Int): Json = {
    var instances: List[Json] = List()
    if (numberUnsat(s))
      return GENNUMBERUNSAT

    val sAsObj = s.asObject.get
    val sType = sAsObj.apply(Type).get.asString.get

    var valMin: Option[Double] = Option(sAsObj.apply(Minimum)) match {
      case Some(Some(m)) => Some(m.asNumber.get.toDouble)
      case _ => None
    }

    var valMax: Option[Double] = Option(sAsObj.apply(Maximum)) match {
      case Some(Some(m)) => Some(m.asNumber.get.toDouble)
      case _ => None
    }

    val valMof: Option[Double] = Option(sAsObj.apply(MultipleOf)) match {
      case Some(Some(m)) => if (sType.equals(JSInteger)) Some(lcm(1.0, m.asNumber.get.toDouble))
      else Some(m.asNumber.get.toDouble)
      case _ => if (sType.equals(JSInteger)) Some(1)
      else None
    }

    val valNotMof: Option[Double] = sAsObj.apply(Not) match {
      case Some(not) => not.asObject.get.apply(MultipleOf) match {
        case Some(m) => Some(m.asNumber.get.toDouble)
        case _ => None
      }
      case _ => None
    }

    val valConst: Option[Json] = sAsObj.apply(Const) match {
      case Some(const) => Some(const)
      case _ => None
    }

    val valEnum: Option[Vector[Json]] = sAsObj.apply(Enum) match {
      case Some(enum) => Some(enum.asArray.get.filter(e => TypesMap(sType).equals(e.name)))
      case _ => None
    }


    if (sAsObj.contains(Const)) {
      instances = instances :+ valConst.get
      var results = JsonObject.singleton("Instances", instances.asJson)

      if (n > 1) results = results.add("Note", ("Cannot generate " + n + " values because of const").asJson)

      return JsonObject.singleton(GENNUMBER, results.asJson).asJson
    }

    if (sAsObj.contains(Enum)) {
      val filteredEnum = valEnum.get.filter(e => !constUnsat(s, e))
      instances = filteredEnum.toList.take(n)
      var results = JsonObject.singleton("Instances", instances.asJson)

      if (n > filteredEnum.size)
        results = results.add("Note", ("Only generated " + filteredEnum.size +
          " instances. Cannot generate " + n + " values because of enum").asJson)

      return JsonObject.singleton(GENNUMBER, results.asJson).asJson
    }

    breakable {
      for (i <- 0 until n) {
        val num: Option[Double] =
          valMof match {

            case Some(mof) => (valMin, valMax) match {

              case (Some(min), Some(max)) =>

                if (min > max) break

                var nearestMof = nearestMultipleToMin(min, mof)
                if (nearestMof > max) break

                valNotMof match {
                  case Some(notMof) => if (!isMultiple(nearestMof, notMof)) Some(nearestMof)
                  else {
                    breakable {
                      while (isMultiple(nearestMof, notMof)) {
                        nearestMof = nearestMof + mof
                        if (nearestMof > max) break
                      }
                    }
                    if (nearestMof > max) None
                    else Some(nearestMof)
                  }
                  case None => Some(nearestMof)
                }

              case (Some(min), None) => var nearestMof = nearestMultipleToMin(min, mof)
                valNotMof match {
                  case Some(notMof) => if (!isMultiple(nearestMof, notMof)) Some(nearestMof)
                  else {
                    while (isMultiple(nearestMof, notMof)) {
                      nearestMof = nearestMof + mof
                    }
                    Some(nearestMof)
                  }
                  case None => Some(nearestMof)
                }

              case (None, Some(max)) => var nearestMof = nearestMultipleToMax(max, mof)
                valNotMof match {
                  case Some(notMof) => if (!isMultiple(nearestMof, notMof)) Some(nearestMof)
                  else {
                    while (isMultiple(nearestMof, notMof)) {
                      nearestMof = nearestMof - mof
                    }
                    Some(nearestMof)
                  }
                  case None => Some(nearestMof)
                }

              case _ => Some(mof)
            }

            case None => (valMin, valMax) match {

              case (Some(min), Some(max)) => var n = min
                valNotMof match {
                  case Some(notMof) => if (!isMultiple(n, notMof)) Some(n)
                  else {
                    breakable {
                      while (isMultiple(n, notMof)) {
                        n = n + epsilon
                        if (n > max) break
                      }
                    }
                    if (n > max) None
                    else Some(n)
                  }
                  case None => Some(n)
                }


              case (Some(min), None) => var n = min
                valNotMof match {
                  case Some(notMof) => if (!isMultiple(n, notMof)) Some(n)
                  else {
                    while (isMultiple(n, notMof)) {
                      n = n + epsilon
                    }
                    Some(n)
                  }
                  case None => Some(n)
                }

              case (None, Some(max)) => var n = max
                valNotMof match {
                  case Some(notMof) => if (!isMultiple(n, notMof)) Some(n)
                  else {
                    while (isMultiple(n, notMof)) {
                      n = n - epsilon
                    }
                    Some(n)
                  }
                  case None => Some(n)
                }

              case _ => var n = 0d
                valNotMof match {
                  case Some(notMof) => if (!isMultiple(n, notMof)) Some(n)
                  else {
                    while (isMultiple(n, notMof)) {
                      n = n + epsilon
                    }
                    Some(n)
                  }
                  case None => Some(n)
                }
            }

          }
        num match {
          case Some(number) =>
            if (number.isWhole) instances = instances :+ number.toInt.asJson
            else instances = instances :+ number.asJson

            (valMin, valMax) match {
              case (Some(_), Some(_)) => valMin = Some(number + epsilon)
              case (Some(_), None) => valMin = Some(number + epsilon)
              case (None, Some(_)) => valMax = Some(number - epsilon)
              case _ => valMin = Some(number + epsilon)

            }



          case _ => if (i == 0) return GENNUMBERUNSAT
            else break
        }
      }
    }

    var results = JsonObject.singleton("Instances", instances.asJson)
    if (n > instances.size)
      results = results.add("Note", ("Only generated " + instances.size + " values. Cannot generate " + n).asJson)

    JsonObject.singleton(GENNUMBER, results.asJson).asJson


  }


  def generateString(s: Json, n: Int): Json = {
    var instances: List[Json] = List()

    if (stringUnsat(s))
      return GENSTRINGUNSAT

    val sAsObj = s.asObject.get

    val pattern: Option[String] = sAsObj.apply(Pattern) match {
      case Some(p) => Some(getNewPattern(p.asString.get))
      case _ => Some(".*")
    }
    val valConst: Option[Json] = sAsObj.apply(Const) match {
      case Some(const) => Some(const)
      case _ => None
    }

    val valEnum: Option[Vector[Json]] = sAsObj.apply(Enum) match {
      case Some(enum) => Some(enum.asArray.get.filter(e => e.name.equals(ParserString)))
      case _ => None
    }

    valConst match {
      case Some(const) =>
        instances = instances :+ const
        var results = JsonObject.singleton("Instances", instances.asJson)

        if (n > 1) results = results.add("Note", ("Cannot generate " + n + " values because of const").asJson)

        return JsonObject.singleton(GENSTRING, results.asJson).asJson

      case _ =>
    }

    valEnum match {
      case Some(enum) =>
        val filteredEnum = enum.filter(e => !constUnsat(s, e))
        if (filteredEnum.isEmpty) return GENSTRINGUNSAT
        else {
          instances = filteredEnum.toList.take(n)
          var results = JsonObject.singleton("Instances", instances.asJson)
          if (n > filteredEnum.size)
            results = results.add("Note", ("Only generated " + filteredEnum.size +
              " instances. Cannot generate " + n + " values because of enum").asJson)

          return JsonObject.singleton(GENSTRING, results.asJson).asJson
        }
      case _ =>
    }

    var initialAutomaton = patternToAutomaton(pattern.get)
    breakable {
      for (i <- 0 until n) {
        val strValue = initialAutomaton.getShortestExample(true)
        if (strValue != null) {
          instances = instances :+ strValue.asJson
          initialAutomaton = initialAutomaton.intersection(patternToAutomaton(strValue).complement())
        }
        else {
          if (i == 0) return JsonObject.empty.add(GENSTRING, (UNSAT + "_BRICS").asJson).asJson
          else break
        }
      }
    }
    var results = JsonObject.singleton("Instances", instances.asJson)
    if (n > instances.size)
      results = results.add("Note", ("Only generated " + instances.size + " values. Cannot generate " + n).asJson)

    JsonObject.singleton(GENSTRING, results.asJson).asJson
  }

}
