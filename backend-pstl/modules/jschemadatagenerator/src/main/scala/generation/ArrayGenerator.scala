package generation

import generation.SchemaGenerator.{CANNOT, GENARRAY, GENARRAYCANNOT, GENARRAYUNSAT, UNSAT, generate}
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import preProcessing.Canonicalization.canonicalizeSchema
import utils.Assertions.{AllOf, Const, Contains, Enum, Items, MaxContains, MaxItems, MinContains, MinItems, Not, PrefixItems, Type, UniqueItems}
import utils.DataTypes.TypesMap
import utils.Functions.{canMerge, cartesianProductList, extractInstances}
import utils.UNSATPreconditions.{arrayUnsat, constUnsat}

import scala.collection.mutable
import scala.math.ceil
import scala.util.control.Breaks.{break, breakable}

object ArrayGenerator {


  def generateArray(s: Json, n: Int): Json = {

    if (arrayUnsat(s)) return GENARRAYUNSAT

    val sAsObj = s.asObject.get
    val sType = sAsObj.apply(Type).get.asString.get

    val valConst: Option[Json] = sAsObj.apply(Const) match {
      case Some(const) => Some(const)
      case _ => None
    }

    val valEnum: Option[Vector[Json]] = sAsObj.apply(Enum) match {
      case Some(enum) => Some(enum.asArray.get.filter(e => TypesMap(sType).equals(e.name)))
      case _ => None
    }

    if (sAsObj.contains(Const)) {
      var results = JsonObject.singleton("Instances", List(sAsObj.apply(Const).get).asJson)
      if (n > 1) results = results.add("Note", ("Cannot generate " + n + " values because of const").asJson)
      return JsonObject.singleton(GENARRAY, results.asJson).asJson
    }

    if (sAsObj.contains(Enum)) {
      val filteredEnum = valEnum.get.filter(e => !constUnsat(s, e))
      val instances = filteredEnum.toList.take(n)
      var results = JsonObject.singleton("Instances", instances.asJson)

      if (n > filteredEnum.size)
        results = results.add("Note", ("Only generated " + filteredEnum.size +
          " instances. Cannot generate " + n + " values because of enum").asJson)

      return JsonObject.singleton(GENARRAY, results.asJson).asJson
    }


    var J: List[Json] = List[Json]()
    var schemas: List[List[Json]] = List[List[Json]]()


    val valMinItems: Int = sAsObj.apply(MinItems) match {
      case Some(min) => min.asNumber.get.toInt.get
      case _ => 0
    }

    val valMaxItems: Int = sAsObj.apply(MaxItems) match {
      case Some(max) => max.asNumber.get.toInt.get
      case _ => Int.MaxValue
    }

    val valPrefixItems: Option[Vector[Json]] = sAsObj.apply(PrefixItems) match {
      case Some(prefixItems) => Some(prefixItems.asArray.get)
      case _ => None
    }

    val valItems: Json = sAsObj.apply(Items) match {
      case Some(items) => items
      case _ => true.asJson
    }

    var valMinContains: Int = sAsObj.apply(MinContains) match {
      case Some(min) => min.asNumber.get.toInt.get
      case _ => 0
    }

    val valMaxContains: Int = sAsObj.apply(MaxContains) match {
      case Some(max) => max.asNumber.get.toInt.get
      case _ => Int.MaxValue
    }

    val valContains: Option[Json] = sAsObj.apply(Contains) match {
      case Some(contains) => if (valMinContains == 0) valMinContains = 1
        Some(contains)
      case _ => None
    }

    val valUniqueItems: Boolean = sAsObj.apply(UniqueItems) match {
      case Some(uniqueItems) => uniqueItems.asBoolean.get
      case _ => false
    }


    var nbUnsat = 0
    var nbCannot = 0


    for (i <- 0 until valMinItems) {
      var schemasToSatisfy: List[Json] = List[Json]()

      (valPrefixItems, valItems, valContains) match {

        case (Some(prefixItems), items, Some(contains)) =>
          if (i + 1 <= prefixItems.size) {
            val merge = canMerge(prefixItems(i), contains)
            if (merge.equals(true.asJson)) {
              schemasToSatisfy = schemasToSatisfy :+ contains :+ prefixItems(i)
            }
            else {
              if (merge.equals(UNSAT)) nbUnsat = nbUnsat + 1
              if (merge.equals(CANNOT)) nbCannot = nbCannot + 1
              schemasToSatisfy = schemasToSatisfy :+ prefixItems(i)
            }
          }
          else {
            val merge = canMerge(items, contains)
            if (merge.equals(true.asJson)) {
              schemasToSatisfy = schemasToSatisfy :+ contains :+ items
            }
            else {
              if (merge.equals(UNSAT)) nbUnsat = nbUnsat + 1
              if (merge.equals(CANNOT)) nbCannot = nbCannot + 1
              schemasToSatisfy = schemasToSatisfy :+ items
            }
          }


        case (Some(prefixItems), items, None) =>
          if (i + 1 <= prefixItems.size) schemasToSatisfy = schemasToSatisfy :+ prefixItems(i)
          else schemasToSatisfy = schemasToSatisfy :+ items


        case (None, items, Some(contains)) =>
          val merge = canMerge(items, contains)
          if (merge.equals(true.asJson)) {
            schemasToSatisfy = schemasToSatisfy :+ contains :+ items
          }
          else {
            if (merge.equals(UNSAT)) nbUnsat = nbUnsat + 1
            if (merge.equals(CANNOT)) nbCannot = nbCannot + 1
            schemasToSatisfy = schemasToSatisfy :+ items
          }


        case (None, items, None) => schemasToSatisfy = schemasToSatisfy :+ items
      }
      schemas = schemas :+ schemasToSatisfy
    }


    val newMax = valMaxItems - schemas.size

    val subPrefixItems: Vector[Json] = valPrefixItems match {
      case Some(prefixItems) => prefixItems.drop(schemas.size)
      case _ => Vector[Json]()
    }

    val maxAttempts = if (newMax < (subPrefixItems.size + valMinContains)) newMax else subPrefixItems.size + valMinContains


    if (valMinContains > 0) {

      val contains = valContains.get

      for (i <- 0 until maxAttempts) {
        var schemasToSatisfy = List[Json]()

        if (i < subPrefixItems.size) {
          val merge = canMerge(contains, subPrefixItems(i))
          if (merge.equals(true.asJson)) {
            schemasToSatisfy = schemasToSatisfy :+ contains :+ subPrefixItems(i)

          }
          else {
            if (merge.equals(UNSAT)) nbUnsat = nbUnsat + 1
            if (merge.equals(CANNOT)) nbCannot = nbCannot + 1
            schemasToSatisfy = schemasToSatisfy :+ subPrefixItems(i)
          }
        }
        else {
          val merge = canMerge(contains, valItems)
          if (merge.equals(true.asJson)) {
            schemasToSatisfy = schemasToSatisfy :+ contains :+ valItems
          }
          else {
            if (merge.equals(UNSAT)) nbUnsat = nbUnsat + 1
            if (merge.equals(CANNOT)) nbCannot = nbCannot + 1
            schemasToSatisfy = schemasToSatisfy :+ valItems
          }
        }
        schemas = schemas :+ schemasToSatisfy
      }
    }



    var minC = valMinContains
    var maxC = valMaxContains

    var nbValuesToGenerate = if (valUniqueItems) 1 else n


    var needToMerge = if (maxC > schemas.size) false else true
    var min = valMinItems

    val resultsMap: mutable.HashMap[Int, List[Json]] = scala.collection.mutable.HashMap()

    var subSetsToValues: mutable.HashMap[List[Int], List[Json]] = scala.collection.mutable.HashMap()

    var idx = 0

    breakable {
      schemas.foreach { schemasToSatisfy =>
        var schemasList = schemasToSatisfy

        if (maxC == 0) needToMerge = false

        if (valMinContains > 0)
          if (minC <= 0 && min <= 0) break


        val canonicalizedSchema = {

          if (needToMerge || minC > 0)
            if (schemasList.size > 1) canonicalizeSchema(JsonObject.singleton(AllOf, schemasList.asJson).asJson)
            else schemasList.last

          else {
            schemasList = if (schemasList.size > 1) schemasList.tail
            else schemasList

            if (valMinContains > 0 && maxC <= 0) {
              val notContains = JsonObject.singleton(Not, valContains.get).asJson
              canonicalizeSchema(JsonObject.singleton(AllOf, (schemasList :+ notContains).asJson).asJson)
            }
            else {
              schemasList.last
            }

          }
        }

        val value = generate(canonicalizedSchema, nbValuesToGenerate)
        var valuesList = extractInstances(value)

        if (valuesList.size == 1 && valuesList.head.equals(UNSAT)) {
          if (schemasList.size == 1) return GENARRAYUNSAT
          else {
            nbUnsat = nbUnsat + 1
            val newValue = generate(schemasList.last, nbValuesToGenerate)
            var newValuesList = extractInstances(newValue)

            if (newValuesList.size == 1 && newValuesList.head.equals(UNSAT)) return GENARRAYUNSAT
            if (newValuesList.size == 1 && newValuesList.head.equals(CANNOT)) return GENARRAYCANNOT

            newValuesList = newValuesList.filter(i => !i.equals(UNSAT) && !i.equals(CANNOT))

            resultsMap += (idx -> newValuesList)


            val newSubSets: mutable.HashMap[List[Int], List[Json]] = subSetsToValues.map { case (idxs, vals) => (idxs :+ idx, (vals ++ newValuesList).distinct) }
            subSetsToValues = subSetsToValues.addAll(newSubSets.toList)

            subSetsToValues += (List(idx) -> newValuesList)

            if (valUniqueItems) {
              nbValuesToGenerate += 1

              if (newValuesList.size < nbValuesToGenerate) {
                val HallsThSatisfied = subSetsToValues.map { case (subset, distinctValues) => distinctValues.size >= subset.size }
                  .reduce((x, y) => x && y)
                if (HallsThSatisfied) println("Values sufficient to build a matching")
                else {
                  throw new Exception("Need to do a rearrangement")
                }
              }
            }

            else {
              nbValuesToGenerate = ceil(nbValuesToGenerate.toFloat / newValuesList.size.toFloat).toInt
            }


            min = min - 1
          }
        }

        else if (valuesList.size == 1 && valuesList.head.equals(CANNOT)) {
          if (schemasList.size == 1) return GENARRAYCANNOT
          else {
            nbCannot = nbCannot + 1
            val newValue = generate(schemasList.last, nbValuesToGenerate)
            var newValuesList = extractInstances(newValue)

            if (newValuesList.size == 1 && newValuesList.head.equals(UNSAT)) return GENARRAYUNSAT
            if (newValuesList.size == 1 && newValuesList.head.equals(CANNOT)) return GENARRAYCANNOT

            newValuesList = newValuesList.filter(i => !i.equals(UNSAT) && !i.equals(CANNOT))

            resultsMap += (idx -> newValuesList)

            val newSubSets: mutable.HashMap[List[Int], List[Json]] = subSetsToValues.map { case (idxs, vals) => (idxs :+ idx, (vals ++ newValuesList).distinct) }
            subSetsToValues = subSetsToValues.addAll(newSubSets.toList)
            subSetsToValues += (List(idx) -> newValuesList)

            if (valUniqueItems) {
              nbValuesToGenerate += 1

              if (newValuesList.size < nbValuesToGenerate) {
                val HallsThSatisfied = subSetsToValues.map { case (subset, distinctValues) => distinctValues.size >= subset.size }
                  .reduce((x, y) => x && y)
                if (HallsThSatisfied) println("Values sufficient to build a matching")
                else {
                  throw new Exception("Need to do a rearrangement")
                }
              }
            }

            else {
              nbValuesToGenerate = ceil(nbValuesToGenerate.toFloat / newValuesList.size.toFloat).toInt
            }

            min = min - 1
          }
        }

        else {

          valuesList = valuesList.filter(i => !i.equals(UNSAT) && !i.equals(CANNOT))
          resultsMap += (idx -> valuesList)

          val newSubSets: mutable.HashMap[List[Int], List[Json]] = subSetsToValues.map { case (idxs, vals) => (idxs :+ idx, (vals ++ valuesList).distinct) }
          subSetsToValues = subSetsToValues.addAll(newSubSets.toList)
          subSetsToValues += (List(idx) -> valuesList)

          if (valUniqueItems) {
            nbValuesToGenerate += 1

            if (valuesList.size < nbValuesToGenerate) {
              val HallsThSatisfied = subSetsToValues.map { case (subset, distinctValues) => distinctValues.size >= subset.size }
                .reduce((x, y) => x && y)
              if (HallsThSatisfied) println("Values sufficient to build a matching")
            }
          }

          else {
            nbValuesToGenerate = ceil(nbValuesToGenerate.toFloat / valuesList.size.toFloat).toInt
          }

          min = min - 1
          if (schemasList.size > 1) {
            minC = minC - 1
            maxC = maxC - 1
          }
        }

        idx += 1

      }
    }

    if (valMinContains > 0)
      if (minC > 0) {
        if (nbUnsat > 0) return GENARRAYUNSAT
        if (nbCannot > 0) return GENARRAYCANNOT
      }

    if (!valUniqueItems) {
      var instances = cartesianProductList(resultsMap.values.toList).take(n)
      val currentArraysSize = instances.head.size

      var results = JsonObject.empty

      if (instances.size == n) {
        results = JsonObject.singleton("Instances", instances.map(i => i.asJson).asJson)
        JsonObject.singleton(GENARRAY, results.asJson).asJson
      }
      else {
        if (currentArraysSize == valMaxItems) {
          results = JsonObject.singleton("Instances", instances.map(i => i.asJson).asJson)
          results = results.add("Note", ("Only generated " + instances.size +
            " instances. Cannot generate " + n + " values because the number of values is limited (maxItems)").asJson)
          JsonObject.singleton(GENARRAY, results.asJson).asJson
        }

        else {
          val pItems: List[Json] = sAsObj.apply(PrefixItems) match {
            case Some(prefixItems) => prefixItems.asArray.get.toList
            case _ => List()
          }
          breakable {
            for (idx <- currentArraysSize until valMaxItems) {
              if (instances.size == n) break

              val schemaToUse = if (idx < pItems.size) pItems(idx) else valItems
              var merged = false

              val canonicalS =
                if (valMinContains > 0) {
                  merged = true
                  if (maxC <= 0) {
                    val notContains = JsonObject.singleton(Not, valContains.get).asJson
                    canonicalizeSchema(JsonObject.singleton(AllOf, List(notContains, schemaToUse).asJson).asJson)
                  }
                  else {
                    canonicalizeSchema(JsonObject.singleton(AllOf, List(schemaToUse, valContains.get).asJson).asJson)
                  }
                }
                else schemaToUse

              val value = generate(canonicalS, nbValuesToGenerate)
              var valuesList = extractInstances(value)

              if (valuesList.size == 1 && (valuesList.head.equals(UNSAT) || valuesList.head.equals(CANNOT))) {
                if (!merged) break
                else {
                  val newValue = generate(schemaToUse, nbValuesToGenerate)
                  var newValuesList = extractInstances(newValue)

                  if (newValuesList.size == 1 && (newValuesList.head.equals(UNSAT) || newValuesList.head.equals(CANNOT)))
                    break
                  else {
                    newValuesList = newValuesList.filter(i => !i.equals(UNSAT) && !i.equals(CANNOT))
                    nbValuesToGenerate = ceil(nbValuesToGenerate.toFloat / newValuesList.size.toFloat).toInt
                    var newInstances: List[List[Json]] = List()
                    newValuesList.foreach {
                      i =>
                        instances.filter(vec => vec.size == idx).foreach {
                          arr => newInstances = newInstances :+ (arr :+ i)
                        }
                    }
                    val nbMissingInstances = n - instances.size
                    newInstances = newInstances.take(nbMissingInstances)
                    newInstances.foreach { newI => instances = instances :+ newI }

                  }
                }

              }
              else {
                valuesList = valuesList.filter(i => !i.equals(UNSAT) && !i.equals(CANNOT))
                nbValuesToGenerate = ceil(nbValuesToGenerate.toFloat / valuesList.size.toFloat).toInt
                if (merged) maxC -= 1
                var newInstances: List[List[Json]] = List()
                valuesList.foreach {
                  i =>
                    instances.filter(vec => vec.size == idx).foreach {
                      arr => newInstances = newInstances :+ (arr :+ i)
                    }
                }
                val nbMissingInstances = n - instances.size
                newInstances = newInstances.take(nbMissingInstances)
                newInstances.foreach { newI => instances = instances :+ newI }
              }

            }
          }

          results = JsonObject.singleton("Instances", instances.map(i => i.asJson).asJson)

          if (instances.size < n)
            results = results.add("Note", ("Only generated " + instances.size +
              " instances. Cannot generate " + n + " values because the number of values is limited (maxItems)").asJson)
          JsonObject.singleton(GENARRAY, results.asJson).asJson
        }

      }


    }
    else {
      var sortedResultsMap = resultsMap.toList
      sortedResultsMap = sortedResultsMap.sortBy(_._2.size)(Ordering[Int])

      val newResultsMap: mutable.HashMap[Int, Json] = scala.collection.mutable.HashMap()
      var counted: List[Json] = List()

      for (i <- sortedResultsMap.indices) {
        val idxValuesPair = sortedResultsMap(i)
        val idx = idxValuesPair._1
        val values = idxValuesPair._2

        val filteredValues = values.filter(k => !counted.contains(k))
        if (filteredValues.isEmpty) return GENARRAYCANNOT
        else {
          val value = filteredValues.head
          newResultsMap += (idx -> value)
          counted = counted :+ value
        }
      }
      for (i <- 0 until newResultsMap.size) {
        J = J :+ newResultsMap(i)
      }

      val results = JsonObject.singleton("Instances", List(J.asJson).asJson)
      JsonObject.singleton(GENARRAY, results.asJson).asJson

    }

  }

}
