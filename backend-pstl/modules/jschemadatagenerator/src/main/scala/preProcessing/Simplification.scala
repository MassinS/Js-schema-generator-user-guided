package preProcessing

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import preProcessing.Canonicalization.canonicalizeSchema
import utils.Assertions.{AllOf, AnyOf, Const, Contains, DependentRequired, DependentSchemas, Enum, Items, MaxContains, MaxItems, MaxProperties, Maximum, MinContains, MinItems, MinProperties, Minimum, MultipleOf, Not, Pattern, PatternProperties, PrefixItems, PropertyNames, Required, Type, UniqueItems}
import utils.DataTypes._
import utils.Functions.{complement, lcm, numbersRange, prefixItemsOrItems}

import scala.collection.mutable.ListBuffer


object Simplification {

  def notSimplification(s: Json): Json = {

    s.name match {
      case ParserBool => (!s.asBoolean.get).asJson

      case ParserObject =>
        val notSchema = s.asObject.get

        if (notSchema.equals(JsonObject.empty))
          return false.asJson

        if (notSchema.contains(Type) && notSchema.apply(Type).get.name.equals(ParserString)) {

          val t = notSchema.apply(Type).get.asString.get
          var otherTypes = JSTypes - t
          // Remove integer type if we're negating the type number
          //          if(t.equals(JSNumber))
          //            otherTypes -= JSInteger

          val canonicalizeOtherTypes = canonicalizeSchema(JsonObject.singleton(Type, otherTypes.asJson).asJson)

          if (t.equals(JSNull) || t.equals(JSBool) || notSchema.size == 1)
            canonicalizeOtherTypes
          else

            anyOfSimplification(List(complement(notSchema.asJson), canonicalizeOtherTypes).distinct.asJson)
        }


        else if (notSchema.keys.size == 1) {
          val key = notSchema.keys.last

          key match {

            case AllOf =>
              val simplifiedNots = notSchema.apply(AllOf).get.asArray.get.map(x => notSimplification(x)).asJson
              anyOfSimplification(simplifiedNots)

            case AnyOf =>
              val simplifiedNots = notSchema.apply(AnyOf).get.asArray.get.map(x => notSimplification(x)).asJson
              allOfSimplification(simplifiedNots)

            case Not => notSchema.apply(Not).get

            case _ => JsonObject.singleton(Not, notSchema.asJson).asJson
          }
        }

        else JsonObject.singleton(Not, notSchema.asJson).asJson

    }
  }


  def allOfSimplification(s: Json): Json = {
    val allOfArray = s.asArray.get.distinct

    if (allOfArray.contains(false.asJson))
      return false.asJson

    val allOfSchema = allOfArray.filter(x => !x.equals(true.asJson))

    if (allOfSchema.isEmpty)
      return true.asJson

    if (allOfSchema.size == 1)
      allOfSchema.last


    else if (allOfSchema.size > 2) {
      val head = allOfSchema.head
      val tail = allOfSchema.tail.asJson

      allOfSimplification(List(head, allOfSimplification(tail)).asJson)
    }

    else {
      val s1 = allOfSchema.head
      val s2 = allOfSchema.last

      val s1AsObj = s1.asObject.get
      val s2AsObj = s2.asObject.get



      if (s1AsObj.contains(Type) && s2.asObject.get.contains(Type)) {
        if (!s1AsObj.apply(Type).get.asString.get.equals(s2.asObject.get.apply(Type).get.asString.get))
          false.asJson

        else {
          val s1Type = s1AsObj.apply(Type).get.asString.get
          s1Type match {
            case JSNull => s1AsObj.deepMerge(s2AsObj).asJson

            case JSBool => s1AsObj.deepMerge(s2AsObj).asJson

            case JSNumber | JSInteger =>
              val range = numbersRange(s1, s2)
              val min = range._1
              val max = range._2

              val s1Obj = s1AsObj
              val s2Obj = s2AsObj

              val mof: Option[Double] = if (s1Obj.contains(MultipleOf)) {
                if (s2Obj.contains(MultipleOf))
                  Some(lcm(s1Obj.apply(MultipleOf).get.asNumber.get.toDouble, s2Obj.apply(MultipleOf).get.asNumber.get.toDouble))
                else
                  Some(s1Obj.apply(MultipleOf).get.asNumber.get.toDouble)
              }
              else {
                if (s2Obj.contains(MultipleOf))
                  Some(s2Obj.apply(MultipleOf).get.asNumber.get.toDouble)
                else None
              }

              val notMof1: Option[Double] = if (s1Obj.contains(Not))
                if (s1Obj.apply(Not).get.asObject.get.contains(MultipleOf))
                  Some(s1Obj.apply(Not).get.asObject.get.apply(MultipleOf).get.asNumber.get.toDouble)
                else None
              else None

              val notMof2: Option[Double] = if (s2Obj.contains(Not))
                if (s2Obj.apply(Not).get.asObject.get.contains(MultipleOf))
                  Some(s2Obj.apply(Not).get.asObject.get.apply(MultipleOf).get.asNumber.get.toDouble)
                else None
              else None

              val notMof: Option[Double] = (notMof1, notMof2) match {
                case (Some(n1), Some(n2)) => Some(lcm(n1, n2))
                case (Some(n1), None) => Some(n1)
                case (None, Some(n2)) => Some(n2)
                case _ => None
              }

              var obj = JsonObject.fromIterable(Iterable((Type, s1Type.asJson)))
              min match {
                case Some(mn) => obj = obj.add(Minimum, mn.asJson)
                case None =>
              }
              max match {
                case Some(mx) => obj = obj.add(Maximum, mx.asJson)
                case None =>
              }

              mof match {
                case Some(m) => obj = obj.add(MultipleOf, m.asJson)
                case None =>
              }
              notMof match {
                case Some(n) => obj = obj.add(Not, JsonObject.singleton(MultipleOf, n.asJson).asJson)
                case None =>
              }

              constEnumSimplification(s1Obj, s2Obj, obj, s1Type)


            case JSArray =>
              val s1Obj = s1AsObj
              val s2Obj = s2AsObj

              val s1MinItems = Option(s1Obj.apply(MinItems))
              val s2MinItems = Option(s2Obj.apply(MinItems))

              val newMinItems: Option[Double] = (s1MinItems, s2MinItems) match {
                case (Some(Some(min1)), Some(Some(min2))) => Some(math.max(min1.asNumber.get.toDouble, min2.asNumber.get.toDouble))
                case (Some(Some(min1)), Some(None)) => Some(min1.asNumber.get.toDouble)
                case (Some(None), Some(Some(min2))) => Some(min2.asNumber.get.toDouble)
                case _ => None
              }

              val s1MaxItems = Option(s1Obj.apply(MaxItems))
              val s2MaxItems = Option(s2Obj.apply(MaxItems))

              val newMaxItems: Option[Double] = (s1MaxItems, s2MaxItems) match {
                case (Some(Some(max1)), Some(Some(max2))) => Some(math.min(max1.asNumber.get.toDouble, max2.asNumber.get.toDouble))
                case (Some(Some(max1)), Some(None)) => Some(max1.asNumber.get.toDouble)
                case (Some(None), Some(Some(max2))) => Some(max2.asNumber.get.toDouble)
                case _ => None
              }

              val s1PrefixItems = Option(s1Obj.apply(PrefixItems))
              val s2PrefixItems = Option(s2Obj.apply(PrefixItems))

              val maxPrefixItems = (s1PrefixItems, s2PrefixItems) match {
                case (Some(Some(s1P)), Some(Some(s2P))) => math.max(s1P.asArray.get.size, s2P.asArray.get.size)
                case (Some(Some(s1P)), Some(None)) => s1P.asArray.get.size
                case (Some(None), Some(Some(s2P))) => s2P.asArray.get.size
                case _ => 0
              }

              val s1Items = Option(s1Obj.apply(Items))
              val s2Items = Option(s2Obj.apply(Items))

              val prefixItemsList: ListBuffer[Json] = ListBuffer()
              for (i <- 0 until maxPrefixItems) {
                val sp1 = prefixItemsOrItems(s1PrefixItems, s1Items, i)
                val sp2 = prefixItemsOrItems(s2PrefixItems, s2Items, i)
                (sp1, sp2) match {
                  case (Some(p1), Some(p2)) => prefixItemsList += allOfSimplification(List(p1, p2).asJson)
                  case (Some(p1), None) => prefixItemsList += p1
                  case (None, Some(p2)) => prefixItemsList += p2
                  case _ =>
                }
              }

              val newItems: Option[Json] = (s1Items, s2Items) match {
                case (Some(Some(it1)), Some(Some(it2))) => Some(allOfSimplification(List(it1, it2).asJson))
                case (Some(Some(it1)), Some(None)) => Some(it1)
                case (Some(None), Some(Some(it2))) => Some(it2)
                case _ => None
              }

              val s1UniqueItems = Option(s1Obj.apply(UniqueItems))
              val s2UniqueItems = Option(s2Obj.apply(UniqueItems))
              val newUniqueItems: Option[Boolean] = (s1UniqueItems, s2UniqueItems) match {
                case (Some(Some(u1)), Some(Some(u2))) => Some(u1.asBoolean.get || u2.asBoolean.get)
                case (Some(Some(u1)), Some(None)) => Some(u1.asBoolean.get)
                case (Some(None), Some(Some(u2))) => Some(u2.asBoolean.get)
                case _ => None
              }


              val s1MinContains = Option(s1Obj.apply(MinContains))
              val s2MinContains = Option(s2Obj.apply(MinContains))

              val newMinContains: Option[Double] = (s1MinContains, s2MinContains) match {
                case (Some(Some(min1)), Some(Some(min2))) => Some(math.max(min1.asNumber.get.toDouble, min2.asNumber.get.toDouble))
                case (Some(Some(min1)), Some(None)) => Some(min1.asNumber.get.toDouble)
                case (Some(None), Some(Some(min2))) => Some(min2.asNumber.get.toDouble)
                case _ => None
              }

              val s1MaxContains = Option(s1Obj.apply(MaxContains))
              val s2MaxContains = Option(s2Obj.apply(MaxContains))

              val newMaxContains: Option[Double] = (s1MaxContains, s2MaxContains) match {
                case (Some(Some(max1)), Some(Some(max2))) => Some(math.min(max1.asNumber.get.toDouble, max2.asNumber.get.toDouble))
                case (Some(Some(max1)), Some(None)) => Some(max1.asNumber.get.toDouble)
                case (Some(None), Some(Some(max2))) => Some(max2.asNumber.get.toDouble)
                case _ => None
              }

              val s1Contains = Option(s1Obj.apply(Contains))
              val s2Contains = Option(s2Obj.apply(Contains))
              val newContains: Option[Json] = (s1Contains, s2Contains) match {
                case (Some(Some(sc1)), Some(Some(sc2))) => Some(allOfSimplification(List(sc1, sc2).asJson))
                case (Some(Some(sc1)), Some(None)) => Some(sc1)
                case (Some(None), Some(Some(sc2))) => Some(sc2)
                case _ => None
              }


              var obj = JsonObject.singleton(Type, JSArray.asJson)
              newMinItems match {
                case Some(min) => obj = obj.add(MinItems, min.asJson)
                case None =>
              }
              newMaxItems match {
                case Some(max) => obj = obj.add(MaxItems, max.asJson)
                case None =>
              }

              if (prefixItemsList.nonEmpty)
                obj = obj.add(PrefixItems, prefixItemsList.asJson)

              newItems match {
                case Some(it) => obj = obj.add(Items, it)
                case None =>
              }
              newMinContains match {
                case Some(min) => obj = obj.add(MinContains, min.asJson)
                case None =>
              }
              newMaxContains match {
                case Some(max) => obj = obj.add(MaxContains, max.asJson)
                case None =>
              }
              newContains match {
                case Some(c) => obj = obj.add(Contains, c)
                case None =>
              }
              newUniqueItems match {
                case Some(unique) => obj = obj.add(UniqueItems, unique.asJson)
                case None =>
              }

              constEnumSimplification(s1Obj, s2Obj, obj, s1Type)

            case JSString =>
              val s1Obj = s1AsObj
              val s2Obj = s2AsObj
              val pattern1 = Option(s1Obj.apply(Pattern))
              val pattern2 = Option(s2Obj.apply(Pattern))

              val newPattern: Option[String] = (pattern1, pattern2) match {
                case (Some(Some(p1)), Some(Some(p2))) => Some(p1.asString.get + " AND " + p2.asString.get)
                case (Some(Some(p1)), Some(None)) => Some(p1.asString.get)
                case (Some(None), Some(Some(p2))) => Some(p2.asString.get)
                case _ => None
              }
              var obj = JsonObject.singleton(Type, JSString.asJson)

              newPattern match {
                case Some(p) => obj = obj.add(Pattern, p.asJson)
                case None =>
              }
              constEnumSimplification(s1Obj, s2Obj, obj, s1Type)


            case JSObject =>
              val s1Obj = s1AsObj
              val s2Obj = s2AsObj

              val req1 = Option(s1Obj.apply(Required))
              val req2 = Option(s2Obj.apply(Required))

              val newReq: Option[Json] = (req1, req2) match {
                case (Some(Some(r1)), Some(Some(r2))) => Some(r1.asArray.get.toSet.union(r2.asArray.get.toSet).asJson)
                case (Some(Some(r1)), Some(None)) => Some(r1)
                case (Some(None), Some(Some(r2))) => Some(r2)
                case _ => None
              }

              val s1MinP = Option(s1Obj.apply(MinProperties))
              val s2MinP = Option(s2Obj.apply(MinProperties))

              val newMinP: Option[Double] = (s1MinP, s2MinP) match {
                case (Some(Some(min1)), Some(Some(min2))) => Some(math.max(min1.asNumber.get.toDouble, min2.asNumber.get.toDouble))
                case (Some(Some(min1)), Some(None)) => Some(min1.asNumber.get.toDouble)
                case (Some(None), Some(Some(min2))) => Some(min2.asNumber.get.toDouble)
                case _ => None
              }

              val s1MaxP = Option(s1Obj.apply(MaxProperties))
              val s2MaxP = Option(s2Obj.apply(MaxProperties))

              val newMaxP: Option[Double] = (s1MaxP, s2MaxP) match {
                case (Some(Some(max1)), Some(Some(max2))) => Some(math.min(max1.asNumber.get.toDouble, max2.asNumber.get.toDouble))
                case (Some(Some(max1)), Some(None)) => Some(max1.asNumber.get.toDouble)
                case (Some(None), Some(Some(max2))) => Some(max2.asNumber.get.toDouble)
                case _ => None
              }

              val s1PattProps = Option(s1Obj.apply(PatternProperties))
              val s2PattProps = Option(s2Obj.apply(PatternProperties))

              val newPattProps: Option[Json] = (s1PattProps, s2PattProps) match {
                case (Some(Some(p1)), Some(Some(p2))) =>
                  val keys1 = p1.asObject.get.keys.toSet
                  val keys2 = p2.asObject.get.keys.toSet
                  val keysIntersect = keys1.intersect(keys2)
                  var pattPropsObj = JsonObject.empty
                  keysIntersect.foreach {
                    k =>
                      pattPropsObj = pattPropsObj.add(k, allOfSimplification(List(p1.asObject.get.apply(k),
                        p2.asObject.get.apply(k)).asJson))
                  }
                  keys1.foreach {
                    k => if (!keysIntersect.contains(k)) pattPropsObj = pattPropsObj.add(k, p1.asObject.get.apply(k).get)
                  }
                  keys2.foreach {
                    k => if (!keysIntersect.contains(k)) pattPropsObj = pattPropsObj.add(k, p2.asObject.get.apply(k).get)
                  }

                  Some(pattPropsObj.asJson)


                case (Some(Some(p1)), Some(None)) => Some(p1)
                case (Some(None), Some(Some(p2))) => Some(p2)
                case _ => None
              }

              val pNames1 = Option(s1Obj.apply(PropertyNames))
              val pNames2 = Option(s2Obj.apply(PropertyNames))
              val newPNames = (pNames1, pNames2) match {
                case (Some(Some(p1)), Some(Some(p2))) => Some(allOfSimplification(List(p1, p2).asJson))
                case (Some(Some(p1)), Some(None)) => Some(p1)
                case (Some(None), Some(Some(p2))) => Some(p2)
                case _ => None
              }

              val depReq1 = Option(s1Obj.apply(DependentRequired))
              val depReq2 = Option(s2Obj.apply(DependentRequired))

              val newDepReq: Option[Json] = (depReq1, depReq2) match {
                case (Some(Some(r1)), Some(Some(r2))) => Some(r1.asArray.get.toSet.union(r2.asArray.get.toSet).asJson)
                case (Some(Some(r1)), Some(None)) => Some(r1)
                case (Some(None), Some(Some(r2))) => Some(r2)
                case _ => None
              }

              val depSchemas1 = Option(s1Obj.apply(DependentSchemas))
              val depSchemas2 = Option(s2Obj.apply(DependentSchemas))

              val newDepSchemas: Option[Json] = (depSchemas1, depSchemas2) match {
                case (Some(Some(r1)), Some(Some(r2))) => Some(r1.asObject.get.deepMerge(r2.asObject.get).asJson)
                case (Some(Some(r1)), Some(None)) => Some(r1)
                case (Some(None), Some(Some(r2))) => Some(r2)
                case _ => None
              }


              var obj = JsonObject.singleton(Type, s1Type.asJson)
              newReq match {
                case Some(r) => obj = obj.add(Required, r)
                case _ =>
              }
              newMinP match {
                case Some(min) => obj = obj.add(MinProperties, min.asJson)
                case _ =>
              }
              newMaxP match {
                case Some(max) => obj = obj.add(MaxProperties, max.asJson)
                case _ =>
              }
              newPattProps match {
                case Some(pp) => obj = obj.add(PatternProperties, pp)
                case _ =>
              }
              newPNames match {
                case Some(np) => obj = obj.add(PropertyNames, np)
                case _ =>
              }
              newDepReq match {
                case Some(r) => obj = obj.add(DependentRequired, r)
                case _ =>
              }
              newDepSchemas match {
                case Some(r) => obj = obj.add(DependentSchemas, r)
                case _ =>
              }

              constEnumSimplification(s1Obj, s2Obj, obj, s1Type)

          }

        }

      }


      else if (s1AsObj.contains(AnyOf) || s2AsObj.contains(AnyOf)) {
        val pair = if (s1AsObj.contains(AnyOf)) (s2, s1)
                   else (s1, s2)
        val anyOfSchema = pair._2
        val otherSchema = pair._1
        val anyOfList = anyOfSchema.asObject.get.apply(AnyOf).get.asArray.get.toList
        val canonicalizedAnyOfList = anyOfList.map(x =>
          allOfSimplification(List(otherSchema, x).asJson))
        anyOfSimplification(canonicalizedAnyOfList.asJson)
      }


      else if ((s1AsObj.contains(Enum) && s1AsObj.size == 1) || (s2AsObj.contains(Enum) && s2AsObj.size == 1)) {
        val pair = if (s1AsObj.contains(Enum) && s1AsObj.size == 1) (s1AsObj, s2AsObj)
        else (s2AsObj, s1AsObj)
        val singletonEnum = pair._1
        val singletonEnumAsArray = singletonEnum.apply(Enum).get.asArray.get.toSet
        val otherSchema = pair._2
        val obj = if (otherSchema.contains(Enum)) {
          val otherSchemaEnum = otherSchema.apply(Enum).get.asArray.get.toSet
          val intersect = singletonEnumAsArray.intersect(otherSchemaEnum)
          if (intersect.isEmpty) false.asJson
          else otherSchema.remove(Enum).add(Enum, intersect.asJson).asJson
        }
        else
          otherSchema.add(Enum, singletonEnumAsArray.asJson).asJson
        obj
      }

      else if ((s1AsObj.contains(Const) && s1AsObj.size == 1) || (s2AsObj.contains(Const) && s2AsObj.size == 1)) {
        val pair = if (s1AsObj.contains(Const) && s1AsObj.size == 1) (s1AsObj, s2AsObj)
                   else (s2AsObj, s1AsObj)
        val singletonConst = pair._1
        val otherSchema = pair._2
        val obj = if (otherSchema.contains(Const)) {
          if (!singletonConst.apply(Const).get.equals(otherSchema.apply(Const).get)) false.asJson
          else otherSchema.asJson
        }
        else
          otherSchema.add(Const, singletonConst.apply(Const).get).asJson

        obj
      }

      else JsonObject.singleton(AllOf, allOfSchema.asJson).asJson

    }

  }


  def anyOfSimplification(s: Json): Json = {

    val canonicalizedSchema = s.asArray.get.distinct

    val anyOfSchema = canonicalizedSchema
    if (anyOfSchema.size == 1)
      return anyOfSchema.last

    if (anyOfSchema.contains(true.asJson))
      return true.asJson

    if (anyOfSchema.contains(false.asJson)) {
      val filteredArray = anyOfSchema.filter(x => !x.eq(false.asJson))
      return anyOfSimplification(filteredArray.asJson)
    }

    if (anyOfSchema.size == 2) {
      val s1 = anyOfSchema.head.asObject.get
      val s2 = anyOfSchema.last.asObject.get

      if (s1.size == 1 && s2.size == 1 && s1.contains(AnyOf) && s2.contains(AnyOf)) {
        val flatten: List[Json] = s1.apply(AnyOf).get.asArray.get.toList ++ s2.apply(AnyOf).get.asArray.get.toList
        return JsonObject.singleton(AnyOf, flatten.distinct.asJson).asJson
      }

      if (s1.size == 1 && s2.size == 1 && (s1.contains(AnyOf) || s2.contains(AnyOf))) {
        val disjunctSingleton = if (s1.contains(AnyOf)) (s1, s2) else (s2, s1)
        val concat = disjunctSingleton._1.apply(AnyOf).get.asArray.get.toList.appended(disjunctSingleton._2.asJson)
        return JsonObject.singleton(AnyOf, concat.distinct.asJson).asJson

      }
    }

    JsonObject.singleton(AnyOf, anyOfSchema.asJson).asJson


  }


  def oneOfSimplification(s: Json): Json = {
    val oneOfArray = s.asArray.get.toList

    if (oneOfArray.size == 1) {
      if (s.asArray.get.size > oneOfArray.size) {
        return false.asJson
      }
      oneOfArray.last
    }
    else {
      val anyOfArray = oneOfArray.zipWithIndex.map { case (x, i) =>
        allOfSimplification(oneOfArray.zipWithIndex.map { case (y, j) => if (i == j) x.asJson
        else notSimplification(y)
        }.asJson)
      }
      anyOfSimplification(anyOfArray.asJson)
    }
  }


  def constEnumSimplification(s1: JsonObject, s2: JsonObject, s: JsonObject, s1Type: String): Json = {

    var merged: Json = s.asJson
    val s1Const = Option(s1.apply(Const))
    val s2Const = Option(s2.apply(Const))

    val newConst: Option[Json] = (s1Const, s2Const) match {
      case (Some(Some(c1)), Some(Some(c2))) => if (!c1.equals(c2)) return false.asJson
      else Some(c1)
      case (Some(Some(c1)), Some(None)) => Some(c1)
      case (Some(None), Some(Some(c2))) => Some(c2)
      case _ => None
    }
    newConst match {
      case Some(c) =>
        if (!TypesMap(s1Type).equals(newConst.get.name)) return false.asJson
        else merged = merged.asObject.get.add(Const, c).asJson
      case None =>
    }

    val s1Enum = Option(s1.apply(Enum))
    val s2Enum = Option(s2.apply(Enum))

    val newEnum: Option[Vector[Json]] = (s1Enum, s2Enum) match {
      case (Some(Some(e1)), Some(Some(e2))) =>
        val intersect = e1.asArray.get.toSet & e2.asArray.get.toSet

        if (intersect.isEmpty) return false.asJson
        else Some(intersect.toVector)

      case (Some(Some(e1)), Some(None)) =>
        Some(e1.asArray.get)
      case (Some(None), Some(Some(e2))) =>
        Some(e2.asArray.get)
      case _ => None
    }

    newEnum match {
      case Some(e) => newConst match {
        case Some(c) => if (!e.contains(c)) return false.asJson
        case None =>
      }
        val enumTypes = e.map(k => TypesMap(s1Type).equals(k.name)).reduce((x, y) => x || y)
        if (!enumTypes) return false.asJson

        merged = merged.asObject.get.add(Enum, e.asJson).asJson

      case None =>
    }

    merged
  }
}
