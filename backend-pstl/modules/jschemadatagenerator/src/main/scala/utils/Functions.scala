package utils

import dk.brics.automaton.{Automaton, RegExp}
import generation.SchemaGenerator.{CANNOT, UNSAT}
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject, ParsingFailure}
import utils.Assertions.{AllOf, AnyOf, Maximum, Minimum, MultipleOf, Not, Pattern, Type}
import utils.DataTypes.{JSInteger, JSNumber, JSString, ParserBool, ParserObject, ParserString}

import scala.io.Source

object Functions {

  final val epsilon = 0.0000001d


  def readParseJson(pathToFile: String, oneOfAsAnyOf: Boolean): Json = {
    val sourceFile = Source.fromFile(pathToFile)
    var stringFile = try sourceFile.mkString finally sourceFile.close()
    if (oneOfAsAnyOf)
      stringFile = stringFile.replaceAll("oneOf", "anyOf")

    val parseJson: Either[ParsingFailure, Json] = parse(stringFile)
    val parsedJson = parseJson match {
      case Left(parsingError) =>
        throw new IllegalArgumentException(s"Invalid JSON File: ${parsingError.message}")
      case Right(json) => json.name match {
        case "Object" | "Boolean" => json
        case _ => throw new Exception("This is an invalid JSON Schema")
      }

    }
    parsedJson
  }


def readParseJsonApi(schema: String, oneOfAsAnyOf: Boolean): Json = {

    var stringFile =  schema
    if (oneOfAsAnyOf)
      stringFile = stringFile.replaceAll("oneOf", "anyOf")

    val parseJson: Either[ParsingFailure, Json] = parse(stringFile)
    val parsedJson = parseJson match {
      case Left(parsingError) =>
        throw new IllegalArgumentException(s"Invalid JSON File: ${parsingError.message}")
      case Right(json) => json.name match {
        case "Object" | "Boolean" => json
        case _ => throw new Exception("This is an invalid JSON Schema")
      }

    }
    parsedJson
  }


  


  def gcd(x: Double, y: Double): Double = {
    var a = x;
    var b = y
    while (b > 0.001) {
      val temp = b
      b = a % b
      a = temp
    }
    a
  }


  def lcm(x: Double, y: Double): Double = {
    (x * y) / gcd(x, y)
  }

  def hasMultipleInInterval(mof: Double, min: Double, max: Double): Boolean = {
    val a = math.ceil(min / mof) * mof
    val b = math.floor(max / mof) * mof
    a <= b + epsilon
  }

  def nearestMultipleToMin(min: Double, mof: Double): Double = {
    val quotient = Math.ceil(min / mof)
    val nearestMultiple = quotient * mof

    nearestMultiple
  }

  def nearestMultipleToMax(max: Double, mof: Double): Double = {
    val quotient = Math.floor(max / mof)
    val nearestMultiple = quotient * mof

    nearestMultiple
  }


  def isMultiple(mof: Double, notMof: Double): Boolean = {
    if (notMof == 0.0) {
      false
    } else {
      val result = mof / notMof
      result.isWhole
    }
  }


  def numbersRange(s1: Json, s2: Json): (Option[Double], Option[Double]) = {
    val s1Obj = s1.asObject.get
    val s2Obj = s2.asObject.get
    val min1: Option[Double] = if (s1Obj.contains(Minimum)) Some(s1Obj.apply(Minimum).get.asNumber.get.toDouble)
    else None
    val max1: Option[Double] = if (s1Obj.contains(Maximum)) Some(s1Obj.apply(Maximum).get.asNumber.get.toDouble)
    else None

    val min2: Option[Double] = if (s2Obj.contains(Minimum)) Some(s2Obj.apply(Minimum).get.asNumber.get.toDouble)
    else None
    val max2: Option[Double] = if (s2Obj.contains(Maximum)) Some(s2Obj.apply(Maximum).get.asNumber.get.toDouble)
    else None

    val min = (min1, min2) match {
      case (Some(mn1), Some(mn2)) => Some(scala.math.max(mn1, mn2))
      case (Some(mn1), None) => Some(mn1)
      case (None, Some(mn2)) => Some(mn2)
      case (None, None) => None
    }

    val max = (max1, max2) match {
      case (Some(mx1), Some(mx2)) => Some(scala.math.min(mx1, mx2))
      case (Some(mx1), None) => Some(mx1)
      case (None, Some(mx2)) => Some(mx2)
      case (None, None) => None
    }

    (min, max)
  }


  def prefixItemsOrItems(prefixItems: Option[Option[Json]], items: Option[Option[Json]], index: Int): Option[Json] = {
    prefixItems match {
      case Some(Some(pItems)) => if (index < pItems.asArray.get.size) Some(pItems.asArray.get(index))
      else items match {
        case Some(Some(it)) => Some(it)
        case _ => None
      }

      case _ => items match {
        case Some(Some(it)) => Some(it)
        case _ => None
      }
    }
  }


  def complement(s: Json): Json = {
    val obj = s.asObject.get
    val t = obj.apply(Type).get.asString.get

    t match {
      case JSNumber | JSInteger =>
        val list = scala.collection.mutable.Set[Json]()
        val mof: Option[Json] = obj.apply(MultipleOf)
        val min: Option[Json] = obj.apply(Minimum)
        val max: Option[Json] = obj.apply(Maximum)
        val not: Option[Json] = obj.apply(Not)

        mof match {
          case Some(mf) => list += JsonObject.fromIterable(Iterable((Type, t.asJson), (Not, JsonObject.singleton(MultipleOf, mf.asJson).asJson))).asJson
          case None =>
        }
        min match {
          case Some(mn) => list += JsonObject.fromIterable(Iterable((Type, t.asJson),
            (Maximum, (mn.asNumber.get.toDouble - epsilon).asJson))).asJson
          case None =>
        }

        max match {
          case Some(mx) => list += JsonObject.fromIterable(Iterable((Type, t.asJson),
            (Minimum, (mx.asNumber.get.toDouble + epsilon).asJson))).asJson
          case None =>
        }

        not match {
          case Some(n) => n.asObject.get.apply(MultipleOf) match {
            case Some(m) => list += JsonObject.fromIterable(Iterable((Type, t.asJson), (MultipleOf, m.asJson))).asJson
            case _ =>
          }
          case _ =>
        }

        if (list.size == 1)
          return list.last

        JsonObject.singleton(AnyOf, list.asJson).asJson


      case JSString =>
        val patt = Option(obj.apply(Pattern))
        patt match {
          case Some(Some(p)) => obj.remove(Pattern).add(Pattern, ("NOT(" + p.asString.get + ")").asJson).asJson
          case _ => JsonObject.singleton(Not, s).asJson
        }


      case _ => JsonObject.singleton(Not, s).asJson

    }

  }


  def getMatchingPatterns(k: String, patternPropsSchema: JsonObject): List[String] = {
    val matchingPatterns = patternPropsSchema.keys.filter { p =>
      val automaton = patternToAutomaton(p)
      automaton.run(k)

    }
    matchingPatterns.toList
  }


  def getNewPattern(p: String): String = {
    var newP = (p.startsWith("^"), p.endsWith("$")) match {
      case (true, true) => p.drop(1).dropRight(1)
      case (true, false) => p.drop(1)
      case (false, true) => p.dropRight(1)
      case _ => p
    }

    if (newP.contains("#"))
      newP = newP.replace("#", "")

    //    if(newP.contains("\\d"))
    //      newP = newP.replace("\\d","[0-9]")
    //
    //    if(newP.contains("\\w"))
    //      newP = newP.replace("\\w","[a-zA-Z0-9_]")

    // correct this case
    //    if (newP.contains("@"))
    //      newP = newP.replace("@", "\\"+"\\@")

    newP
  }


  def patternToAutomaton(p: String): Automaton = {

    if (p.startsWith("NOT(") && p.endsWith(")")) {
      val p1 = p.drop(4).dropRight(1)
      patternToAutomaton(p1).complement()
    }
    else {
      if (!p.contains("AND") && !p.contains("OORR")) {
        val p1 = getNewPattern(p)
        new RegExp(p1).toAutomaton
      }
      else {
        if (p.contains("AND")) {
          val patternsList = p.split("\\bAND\\b").map(_.trim).toList
          val automatonList = patternsList.map(k => patternToAutomaton(k))

          var finalAutomaton = new Automaton().complement()
          automatonList.foreach(a => finalAutomaton = finalAutomaton.intersection(a))
          finalAutomaton
        }
        else {
          val patternsList = p.split("\\bOORR\\b").map(_.trim).toList
          val automatonList = patternsList.map(k => patternToAutomaton(k))
          var finalAutomaton = new Automaton()
          automatonList.foreach(a => finalAutomaton = finalAutomaton.union(a))
          finalAutomaton
        }
      }
    }
  }


  def canMerge(s1: Json, s2: Json): Json = {
    (s1.name, s2.name) match {
      case (ParserBool, ParserBool) =>
        if (!s1.asBoolean.get || !s2.asBoolean.get) return UNSAT

      case (ParserBool, ParserObject) =>
        if (!s1.asBoolean.get) return UNSAT

      case (ParserObject, ParserBool) =>
        if (!s2.asBoolean.get) return UNSAT

      case (ParserObject, ParserObject) =>
        if (cannotGenerate(s1) || cannotGenerate(s2)) return CANNOT

//        if (canonicalizeSchema(JsonObject.singleton(AllOf, List(s1, s2).asJson).asJson).equals(false.asJson))
//          return UNSAT

        val s1Obj = s1.asObject.get
        val s2Obj = s2.asObject.get

        if (s1Obj.contains(Type) && s2Obj.contains(Type) && !s1Obj.apply(Type).get.equals(s2Obj.apply(Type).get))
          return UNSAT

    }

    true.asJson
  }


  def cannotGenerate(s: Json): Boolean = {
    s.name match {
      case ParserObject =>
        val sAsObj = s.asObject.get
        if (sAsObj.contains(AllOf)) return true
        if (sAsObj.contains(Not)) {
          if (sAsObj.contains(Type)) {
            if (!(sAsObj.apply(Type).get.asString.get.equals(JSNumber) || sAsObj.apply(Type).get.asString.get.equals(JSInteger)))
              return true
          }
        }
        if (sAsObj.contains(AnyOf)) {
          val anyOfArray = sAsObj.apply(AnyOf).get.asArray.get
          val cannotMap = anyOfArray.map(i => cannotGenerate(i))
          if (cannotMap.reduce((x, y) => x && y)) return true
        }

      case _ => return false
    }
    false
  }


  def extractInstances(generatorsResults: Json): List[Json] = {
    generatorsResults.name match {
      case ParserObject =>
        var instances = List[Json]()
        val res = generatorsResults.asObject.get
        res.keys.foreach {
          generatorRes =>
            res.apply(generatorRes).get.name match {
              case ParserObject =>
                val generatorResValue = res.apply(generatorRes).get.asObject.get
                if (generatorResValue.contains("Instances"))
                  instances = instances ++ generatorResValue.apply("Instances").get.asArray.get.toList

              case _ => instances = instances :+ res.apply(generatorRes).get
            }
        }

        if (instances.exists(i => !i.equals(CANNOT) && !i.equals(UNSAT)))
          instances.filter(i => !i.equals(CANNOT) && !i.equals(UNSAT))
        else {
          if (instances.contains(CANNOT)) List(CANNOT)
          else List(UNSAT)
        }

      case ParserString => List()
    }
  }


  def cartesianProduct[A](in: List[List[(A, Json)]]): List[List[(A, Json)]] = {
    @scala.annotation.tailrec
    def loop(acc: List[List[(A, Json)]], rest: List[List[(A, Json)]]): List[List[(A, Json)]] = {
      rest match {
        case Nil =>
          acc
        case seq :: remainingSeqs =>
          val next = for {
            i <- seq
            a <- acc
          } yield i +: a
          loop(next, remainingSeqs)
      }
    }

    loop(List(Nil), in.reverse)
  }


  def cartesianProductList[T](lists: List[List[T]]): List[List[T]] = lists match {
    case Nil => List(List.empty)
    case headList :: tailLists =>
      for {
        x <- headList
        rest <- cartesianProductList(tailLists)
      } yield x :: rest
  }

}
