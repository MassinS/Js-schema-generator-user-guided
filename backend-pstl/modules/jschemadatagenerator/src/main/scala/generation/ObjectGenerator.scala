package generation

import dk.brics.automaton.Automaton
import generation.SchemaGenerator.{CANNOT, GENOBJECT, GENOBJECTCANNOT, GENOBJECTUNSAT, UNSAT, generate}
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import preProcessing.Canonicalization.canonicalizeSchema
import utils.Assertions.{AllOf, Const, DependentRequired, DependentSchemas, Enum, MaxProperties, MinProperties, Pattern, PatternProperties, PropertyNames, Required}
import utils.DataTypes.ParserObject
import utils.Functions.{cannotGenerate, cartesianProduct, extractInstances, getMatchingPatterns, patternToAutomaton}
import utils.UNSATPreconditions.{constUnsat, objectUnsat}

import scala.collection.mutable
import scala.math.ceil
import scala.util.control.Breaks.{break, breakable}

object ObjectGenerator {


  def getInstances(keysValues: mutable.HashMap[String, List[Json]], n: Int): List[Json] = {
    var J = JsonObject.empty
    val keysWithMultipleValues = keysValues.filter { case (_, v) => v.size > 1 }
    val keyWithOneValue = keysValues.keys.toSet.diff(keysWithMultipleValues.keys.toSet)
    keyWithOneValue.foreach(k => J = J.add(k, keysValues(k).head.asJson))

    val l: List[List[(String, Json)]] = cartesianProduct(keysWithMultipleValues.map { case (k, v) => v.map(i => (k, i)) }.toList)

    var instances: List[Json] = List()
    breakable {
      l.foreach { subL =>
        subL.foreach {
          pair => J = J.add(pair._1, pair._2)
        }
        instances = instances :+ J.asJson
        if (instances.size == n)
          break
      }
    }

    instances
  }


  def findProductAndCombos(keysValues: mutable.HashMap[String, List[Json]], reqProps: Set[String], nonReqProps: Set[String],
                           initialNonReqSize: Int, maxPro: Int, previousCombos: List[List[String]]): (Int, List[List[String]]) = {

    val Preq = if (reqProps.isEmpty) 1
    else keysValues.filter { case (k, _) => reqProps.contains(k) }.map { case (_, v) => v.size }.product

    var nonReqComboList: List[List[String]] = List()

    if (previousCombos.nonEmpty) {
      nonReqComboList = previousCombos.filter(l => l.size <= (maxPro - reqProps.size - 1))
    }


    else {
      if (nonReqProps.nonEmpty)
        breakable {
          for (i <- initialNonReqSize - 1 to (maxPro - reqProps.size - 1)) {
            nonReqProps.toList.combinations(i).foreach(combo => nonReqComboList = nonReqComboList :+ combo)
            if (i >= nonReqComboList.size)
              break
          }
        }
    }

    val nonReqProduct = if (nonReqComboList.isEmpty) List(1)
    else
      nonReqComboList.map { combo =>
        if (combo.isEmpty) 1
        else combo.map(k => keysValues(k).size).product
      }

    val sumNonReqProduct = nonReqProduct.sum

    (Preq * sumNonReqProduct, nonReqComboList)
  }


  def generateObject(s: Json, n: Int): Json = {

    var nbDistinctValues = n

    if (objectUnsat(s)) return GENOBJECTUNSAT

    val sAsObj = s.asObject.get

    var J = JsonObject.empty


    var keysValues: mutable.HashMap[String, List[Json]] = scala.collection.mutable.HashMap()


    val valConst: Option[Json] = sAsObj.apply(Const) match {
      case Some(const) => Some(const)
      case _ => None
    }

    val valEnum: Option[Vector[Json]] = sAsObj.apply(Enum) match {
      case Some(enum) => Some(enum.asArray.get.filter(e => e.name.equals(ParserObject)))
      case _ => None
    }

    val valMinPro: Int = sAsObj.apply(MinProperties) match {
      case Some(min) => min.asNumber.get.toInt.get
      case _ => 0
    }

    val valMaxPro: Int = sAsObj.apply(MaxProperties) match {
      case Some(max) => max.asNumber.get.toInt.get
      case _ => Int.MaxValue
    }

    val valReq: Option[Vector[String]] = sAsObj.apply(Required) match {
      case Some(req) => Some(req.asArray.get.map(k => k.asString.get))
      case _ => None
    }

    val valPropNames: Option[Json] = sAsObj.apply(PropertyNames) match {
      case Some(propNames) => Some(propNames)
      case _ => None
    }

    val valPattProps: Option[Json] = sAsObj.apply(PatternProperties) match {
      case Some(pattProps) => Some(pattProps)
      case _ => None
    }

    val valDepReq: Option[Json] = sAsObj.apply(DependentRequired) match {
      case Some(depReq) => Some(depReq)
      case _ => None
    }

    val valDepSchemas: Option[Json] = sAsObj.apply(DependentSchemas) match {
      case Some(depSchemas) => Some(depSchemas)
      case _ => None
    }

    if (sAsObj.contains(Const)) {
      var results = JsonObject.singleton("Instances", List(sAsObj.apply(Const).get).asJson)
      if (n > 1) results = results.add("Note", ("Cannot generate " + n + " values because of const").asJson)
      return JsonObject.singleton(GENOBJECT, results.asJson).asJson
    }

    if (sAsObj.contains(Enum)) {
      val filteredEnum = valEnum.get.filter(e => !constUnsat(s, e))
      val instances = filteredEnum.toList.take(n)
      var results = JsonObject.singleton("Instances", instances.asJson)

      if (n > filteredEnum.size)
        results = results.add("Note", ("Only generated " + filteredEnum.size +
          " instances. Cannot generate " + n + " values because of enum").asJson)

      return JsonObject.singleton(GENOBJECT, results.asJson).asJson
    }

    val valNewReq: Option[Vector[String]] = (valReq, valDepReq) match {
      case (Some(req), Some(depReq)) => var newReq: Set[String] = req.toSet
        val depReqAsObj = depReq.asObject.get
        val depReqKeys = depReqAsObj.keys.toSet
        val intersectKeys = req.intersect(depReqKeys.toVector)
        intersectKeys.foreach(k => newReq = newReq ++ depReqAsObj.apply(k).get.asArray.get.map(i => i.asString.get).toSet)
        Some(newReq.toVector)
      case (Some(req), None) => Some(req)
      case _ => None
    }


    val cannotPatterns: Set[String] = valPattProps match {
      case Some(pattProps) => pattProps.asObject.get.filter { case (_, v) => cannotGenerate(v) }.keys.toSet
      case _ => Set()
    }

    val okPatterns: Set[String] = valPattProps match {
      case Some(pattProps) => pattProps.asObject.get.keys.toSet.diff(cannotPatterns)
      case _ => Set()
    }


    valNewReq match {
      case Some(req) =>
        req.foreach { k =>
          valPattProps match {
            case Some(pattProps) =>
              val patternPropsSchema = pattProps.asObject.get
              val matchingPatterns = getMatchingPatterns(k, patternPropsSchema)

              if (matchingPatterns.toSet.intersect(cannotPatterns).nonEmpty) return GENOBJECTCANNOT

              val schemasToSatisfy = matchingPatterns.map(p => patternPropsSchema.apply(p).get)
              val canonicalizedSchema = canonicalizeSchema(JsonObject.empty.add(AllOf, schemasToSatisfy.asJson).asJson)


              val value = generate(canonicalizedSchema, nbDistinctValues)
              val valuesList = extractInstances(value)

              if (valuesList.size == 1 && valuesList.head.equals(UNSAT)) return GENOBJECTUNSAT
              else if (valuesList.size == 1 && valuesList.head.equals(CANNOT)) return GENOBJECTCANNOT

              else {
                nbDistinctValues = ceil(nbDistinctValues.toFloat / valuesList.size.toFloat).toInt
                keysValues = keysValues.addOne(k, valuesList)
              }

            case _ =>
              val value = generate(true.asJson, nbDistinctValues)
              val valuesList = extractInstances(value)
              nbDistinctValues = ceil(nbDistinctValues.toFloat / valuesList.size.toFloat).toInt
              keysValues = keysValues.addOne(k, valuesList)
          }
        }

      case _ =>
    }

    var initialAcceptedAutomaton = new Automaton().complement()

    valPropNames match {
      case Some(propNames) => propNames.name match {
        case ParserObject => val propNamesPattern = propNames.asObject.get.apply(Pattern).get.asString.get
          val propNamesAutomaton = patternToAutomaton(propNamesPattern)
          initialAcceptedAutomaton = initialAcceptedAutomaton.intersection(propNamesAutomaton)
        case _ =>
      }
      case _ =>
    }

    keysValues.keys.foreach(k => initialAcceptedAutomaton = initialAcceptedAutomaton.intersection(patternToAutomaton(k).complement()))

    cannotPatterns.foreach(k => initialAcceptedAutomaton = initialAcceptedAutomaton.intersection(patternToAutomaton(k).complement()))

    if (keysValues.size < valMinPro) {
      var nbMissingProps = valMinPro - keysValues.size


      valPattProps match {
        case Some(pattProps) => val patterns = okPatterns.filter(k => !keysValues.keys.toList.contains(k))

          val patternPropsSchema = pattProps.asObject.get

          breakable {
            patterns.foreach { p =>
              val pAutomaton = patternToAutomaton(p)
              var acceptedAutomaton = initialAcceptedAutomaton.intersection(pAutomaton)
              breakable {
                for (_ <- 0 until nbMissingProps) {
                  val newK = acceptedAutomaton.getShortestExample(true)
                  if (newK == null)
                    break
                  else {
                    val excludeNewK = patternToAutomaton(newK).complement()
                    acceptedAutomaton = acceptedAutomaton.intersection(excludeNewK)
                    initialAcceptedAutomaton = initialAcceptedAutomaton.intersection(excludeNewK)


                    val matchingPatterns = getMatchingPatterns(newK, patternPropsSchema)
                    val schemasToSatisfy = matchingPatterns.map(p => patternPropsSchema.apply(p).get)
                    val canonicalizedSchema = canonicalizeSchema(JsonObject.empty.add(AllOf, schemasToSatisfy.asJson).asJson)

                    val value = generate(canonicalizedSchema, nbDistinctValues)
                    val valuesList = extractInstances(value)


                    if (!(valuesList.size == 1 && (valuesList.head.equals(UNSAT) || valuesList.head.equals(CANNOT)))) {
                      nbDistinctValues = ceil(nbDistinctValues.toFloat / valuesList.size.toFloat).toInt
                      keysValues = keysValues.addOne(newK, valuesList)
                      nbMissingProps -= 1
                    }

                  }
                }
              }
              if (nbMissingProps == 0)
                break
            }
          }
        case _ =>
      }


      if (nbMissingProps > 0) {
        breakable {
          for (_ <- 0 until nbMissingProps) {
            val newK = initialAcceptedAutomaton.getShortestExample(true)
            if (newK == null)
              throw new Exception("We can't generate new keys")
            else {
              initialAcceptedAutomaton = initialAcceptedAutomaton.intersection(patternToAutomaton(newK).complement())
              val value = generate(true.asJson, nbDistinctValues)
              val valuesList = extractInstances(value)
              nbDistinctValues = ceil(nbDistinctValues.toFloat / valuesList.size.toFloat).toInt
              keysValues = keysValues.addOne(newK, valuesList)
            }
          }
        }
      }
    }


    val reqProps: Set[String] = valNewReq match {
      case Some(req) => req.toSet
      case _ => Set()
    }


    var nbMissingInstances = n - keysValues.values.map(v => v.size).product


    if (nbDistinctValues <= 1) {
      val instances = getInstances(keysValues, n)

      val results = JsonObject.singleton("Instances", instances.asJson)
      JsonObject.singleton(GENOBJECT, results.asJson).asJson

    }

    else {

      if (keysValues.size == valMaxPro && reqProps.equals(keysValues.keys.toSet)) {
        val instances = getInstances(keysValues, n)
        var results = JsonObject.singleton("Instances", instances.asJson)
        results = results.add("Note", ("Only generated " + instances.size +
          " instances. Cannot generate " + n + " values because the number of values is limited (maxProp + Req)").asJson)
        JsonObject.singleton(GENOBJECT, results.asJson).asJson

      }

      else {

        var newSetOfInstances: List[Json] = List()
        val previousInstances = getInstances(keysValues, n)
        previousInstances.foreach(i => newSetOfInstances = newSetOfInstances :+ i)

        var previousCombos: List[List[String]] = List()
        var nonReqProps: Set[String] = keysValues.keys.toSet.diff(reqProps)


        val initialNonReqSize = nonReqProps.size

        val patterns = okPatterns.filter(k => !keysValues.keys.toList.contains(k)) + ".*"

        val patternPropsSchema = valPattProps match {
          case Some(pattProps) => pattProps.asObject.get
          case _ => JsonObject.empty
        }

        breakable {
          patterns.foreach { p =>
            val pAutomaton = patternToAutomaton(p)
            var acceptedAutomaton = initialAcceptedAutomaton.intersection(pAutomaton)
            breakable {
              while (true) {
                val newK = acceptedAutomaton.getShortestExample(true)
                if (newK == null || nbMissingInstances == 0)
                  break
                else {
                  val excludeNewK = patternToAutomaton(newK).complement()
                  acceptedAutomaton = acceptedAutomaton.intersection(excludeNewK)
                  initialAcceptedAutomaton = initialAcceptedAutomaton.intersection(excludeNewK)

                  val matchingPatterns = getMatchingPatterns(newK, patternPropsSchema)
                  val schemasToSatisfy = matchingPatterns.map(p => patternPropsSchema.apply(p).get)
                  val canonicalizedSchema = canonicalizeSchema(JsonObject.empty.add(AllOf, schemasToSatisfy.asJson).asJson)

                  val productAndCombos = findProductAndCombos(keysValues, reqProps, nonReqProps, initialNonReqSize, valMaxPro, previousCombos)

                  val reqNonReqProduct = productAndCombos._1
                  previousCombos = productAndCombos._2

                  val nbValuesToGenerate = ceil(nbMissingInstances.toFloat / reqNonReqProduct.toFloat).toInt

                  val value = generate(canonicalizedSchema, nbValuesToGenerate)
                  val valuesList = extractInstances(value)

                  if (valuesList.size == 1 && valuesList.head.equals(UNSAT)) break

                  val nbOfNewInstances = reqNonReqProduct * valuesList.size


                  if (previousCombos.isEmpty) previousCombos = List(List(newK))
                  else {
                    previousCombos.foreach { combo =>
                      val newCombo = combo :+ newK
                      previousCombos = previousCombos :+ newCombo
                    }
                    previousCombos = previousCombos :+ List(newK)
                  }
                  keysValues = keysValues.addOne(newK, valuesList)
                  nonReqProps = nonReqProps + newK

                  previousCombos.filter(combo => combo.contains(newK) && combo.size >= initialNonReqSize).foreach {
                    combo =>
                      var newKeysValues: mutable.HashMap[String, List[Json]] = scala.collection.mutable.HashMap()
                      reqProps.foreach(k => newKeysValues = newKeysValues.addOne(k, keysValues(k)))
                      combo.foreach(k => newKeysValues = newKeysValues.addOne(k, keysValues(k)))
                      val newInstances = getInstances(newKeysValues, nbMissingInstances)

                      nbMissingInstances = nbMissingInstances - newInstances.size

                      newInstances.foreach(i => newSetOfInstances = newSetOfInstances :+ i)
                      if (nbMissingInstances == 0)
                        break
                  }

                }

              }
            }
            if (nbMissingInstances == 0) break
          }
        }


        val results = JsonObject.singleton("Instances", newSetOfInstances.asJson)
        JsonObject.singleton(GENOBJECT, results.asJson).asJson
      }


    }

  }

}
