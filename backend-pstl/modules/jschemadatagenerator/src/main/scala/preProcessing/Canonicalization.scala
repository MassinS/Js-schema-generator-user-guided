package preProcessing

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import utils.Assertions._
import utils.DataTypes._
import utils.Functions.epsilon

object Canonicalization {


  def missingTypeCanonicalization(s: Json): Json = {
    s.asObject.get.add(Type, JSTypes.asJson).asJson
  }

  def irrelevantKeywords(s: Json): Json = {
    val sAsObj = s.asObject.get
    val sType = sAsObj.apply(Type).get.asString.get
    var typeSpecificAssertions: Set[String] = Set()

    if (sType.equals(JSNull) || sType.equals(JSBool))
      typeSpecificAssertions = Set(Type, Enum, Const, AllOf, AnyOf, OneOf, Not)
    else
      typeSpecificAssertions = TypeAssertionsMap(sType) ++ Set(Type, Enum, Const, AllOf, AnyOf, OneOf, Not)


    val newS = sAsObj.filterKeys(k => typeSpecificAssertions.contains(k))
    newS.asJson
  }


  def multipleTypesCanonicalization(s: Json): Json = {
    val sAsObj = s.asObject.get
    val types = sAsObj.apply(Type).get.asArray.get.toList
    val sWithoutType = sAsObj.remove(Type)

    val anyOfArray = types.map(t => sWithoutType.add(Type, t))

    val canonicalizedAnyOfArray = anyOfArray.map(x => singleTypeCanonicalization(x.asJson)).asJson

    Simplification.anyOfSimplification(canonicalizedAnyOfArray)
  }


  def singleTypeCanonicalization(s: Json): Json = {

    val s1 = irrelevantKeywords(s)
    var sAsObj = s1.asObject.get

    val sType = sAsObj.apply(Type).get.asString.get

    if (sAsObj.contains(Const))
      if (!TypesMap(sType).equals(sAsObj.apply(Const).get.name))
        return false.asJson

    if (sAsObj.contains(Enum)) {
      val e = sAsObj.apply(Enum).get.asArray.get
      if (e.isEmpty) throw new Exception("Invalid JSON Schema: enum is empty")
      val existsSameType = e.map(k => TypesMap(sType).equals(k.name)).reduce((x, y) => x || y)
      if (!existsSameType) return false.asJson
    }


    sAsObj = Json.fromFields(sAsObj.toList.map {
      case (k, v) =>
        if (assertionsMultipleSchemasToCanonicalize.contains(k)) {
          var newV = v
          newV = newV.asObject.get.mapValues(x => canonicalizeSchema(x)).asJson
          (k, newV)
        }
        else if (assertionsSchemaToCanonicalize.contains(k)) {
          if (k.equals(PropertyNames) && v.name.equals(ParserObject)) (k, canonicalizeSchema(v.asObject.get.add(Type, JSString.asJson).asJson))
          else (k, canonicalizeSchema(v))
        }
        else if (assertionsArrayToCanonicalize.contains(k)) {
          var newV = v
          newV = newV.asArray.get.map(x => canonicalizeSchema(x)).asJson
          (k, newV)
        }
        else (k, v)
    }).asObject.get


    sType match {

      case JSNumber | JSInteger =>
        val min = Option(sAsObj.apply(Minimum))
        val xMin = Option(sAsObj.apply(ExclusiveMinimum))
        val newMin: Option[Double] = (min, xMin) match {
          case (Some(Some(mi)), Some(Some(xMi))) =>
            if (xMi.asNumber.get.toDouble >= mi.asNumber.get.toDouble) {
              Some(xMi.asNumber.get.toDouble + epsilon)
            }
            else Some(mi.asNumber.get.toDouble)

          case (Some(None), Some(Some(xMi))) => Some(xMi.asNumber.get.toDouble + epsilon)
          case (Some(Some(mi)), Some(None)) => Some(mi.asNumber.get.toDouble)
          case _ => None
        }
        val max = Option(sAsObj.apply(Maximum))
        val xMax = Option(sAsObj.apply(ExclusiveMaximum))
        val newMax: Option[Double] = (max, xMax) match {
          case (Some(Some(ma)), Some(Some(xMa))) =>
            if (xMa.asNumber.get.toDouble <= ma.asNumber.get.toDouble) {
              Some(xMa.asNumber.get.toDouble - epsilon)
            }
            else Some(ma.asNumber.get.toDouble)

          case (Some(None), Some(Some(xMa))) => Some(xMa.asNumber.get.toDouble - epsilon)
          case (Some(Some(ma)), Some(None)) => Some(ma.asNumber.get.toDouble)
          case _ => None
        }

        var newS = sAsObj.remove(Minimum).remove(ExclusiveMinimum).remove(Maximum).remove(ExclusiveMaximum)
        newMin match {
          case Some(m) => newS = newS.add(Minimum, m.asJson)
          case _ =>
        }
        newMax match {
          case Some(m) => newS = newS.add(Maximum, m.asJson)
          case _ =>
        }
        newS.asJson


      case JSString =>
        val minL = Option(sAsObj.apply(MinLength))
        val maxL = Option(sAsObj.apply(MaxLength))
        val intervalPattern: Option[String] = (minL, maxL) match {
          case (Some(Some(min)), Some(Some(max))) => Some(".{" + min.asNumber.get.toLong.get + "," + max.asNumber.get.toLong.get + "}")
          case (Some(Some(min)), Some(None)) => Some(".{" + min.asNumber.get.toLong.get + ",}")
          case (Some(None), Some(Some(max))) => Some(".{" + 0 + "," + max.asNumber.get.toLong.get + "}")
          case _ => None
        }

        val pattern = Option(sAsObj.apply(Pattern))
        val format = Option(sAsObj.apply(Format))

        var l = List[String]()
        pattern match {
          case Some(Some(p)) => l = l :+ p.asString.get
          case _ =>
        }
        format match {
          case Some(Some(f)) => if (FormatRegex.keys.toList.contains(f.asString.get)) l = l :+ FormatRegex(f.asString.get)
            else l = l :+ ".*"
          case _ =>
        }

        intervalPattern match {
          case Some(i) =>
            l = l :+ i
          case _ =>
        }

        val newPattern: Option[String] =
          if (l.size > 1) Some(l.mkString(" AND "))
          else if (l.size == 1) Some(l.last)
          else None

        var newS = sAsObj.remove(Pattern).remove(MinLength).remove(MaxLength).remove(Format)
        newPattern match {
          case Some(newP) => newS = newS.add(Pattern, newP.asJson)
          case _ =>
        }
        newS.asJson


      case JSObject =>

        var patternProps = if (sAsObj.contains(Properties)) sAsObj.apply(Properties).get.asObject.get
        else JsonObject.empty

        if (sAsObj.contains(PatternProperties))
          sAsObj.apply(PatternProperties).get.asObject.get.toMap.foreach {
            case (k, v) => patternProps = patternProps.add(k, v)
          }

        if (sAsObj.contains(AdditionalProperties)) {
          val pattPropsKeys = patternProps.keys
          val newKey =
            if (pattPropsKeys.nonEmpty) "NOT(" + pattPropsKeys.mkString(" OORR ") + ")"
            else ".*"
          patternProps = patternProps.add(newKey, sAsObj.apply(AdditionalProperties).get)
        }

        var newS = sAsObj.remove(PatternProperties).remove(Properties).remove(AdditionalProperties)

        if (patternProps.size > 0)
          newS = newS.add(PatternProperties, patternProps.asJson)

        newS.asJson


      case _ => sAsObj.asJson
    }
  }


  def multipleConnectivesCanonicalization(s: Json): Json = {
    val sAsObj = s.asObject.get
    val keys = sAsObj.keys.toSet
    var connectives = NonTypeSpecificAssertions & keys

    if (keys.contains(DependentSchemas))
      connectives = connectives + DependentSchemas

    val nonConnectives = keys -- connectives

    val connectivesAsObjects = connectives.map(c => JsonObject.singleton(c, sAsObj.apply(c).get))

    val connectivesCanonicalization = connectivesAsObjects.toList.map { schemaWithConnective =>
      val connectiveName = schemaWithConnective.keys.last
      var connectiveSchema = schemaWithConnective.apply(connectiveName).get

      if (keys.contains(Type) && (connectiveName.equals(AnyOf) || connectiveName.equals(AllOf) || connectiveName.equals(OneOf)))
        connectiveSchema = connectiveSchema.asArray.get.map {
          subS =>
            var newSubS = subS
            if (newSubS.name.equals(ParserObject)) {
              if (!newSubS.asObject.get.contains(Type)) {
                newSubS = newSubS.asObject.get.add(Type, sAsObj.apply(Type).get).asJson
              }
            }
            newSubS

        }.asJson

      if (logicalAssertions.contains(connectiveName) || connectiveName.equals(DependentSchemas)) {
        connectiveName match {
          case OneOf => val canonicalizedSchema = connectiveSchema.asArray.get.map(x => canonicalizeSchema(x)).toList
            Simplification.oneOfSimplification(canonicalizedSchema.asJson)

          case Not => val canonicalizedSchema = canonicalizeSchema(connectiveSchema)
            Simplification.notSimplification(canonicalizedSchema)

          case AllOf =>
            // patch for containment allOf[S,Not:S]
            //            if(connectiveSchema.asArray.get.size==2){
            //              val s1 = connectiveSchema.asArray.get.head
            //              val s2 = connectiveSchema.asArray.get.last
            //              (s1.asObject,s2.asObject) match {
            //                case (Some(s1Obj),Some(s2Obj)) => if(s1Obj.contains(Not) || s2Obj.contains(Not)) {
            //                  val pair = if(s1Obj.contains(Not)) (s1Obj,s2Obj)
            //                  else (s2Obj,s1Obj)
            //                  val not = pair._1
            //                  val other = pair._2
            //                  if(not.apply(Not).get.equals(other.asJson)) return false.asJson
            //                }
            //                case _ =>
            //              }
            //            }
            val canonicalizedSchema = connectiveSchema.asArray.get.map(x => canonicalizeSchema(x)).toList
            Simplification.allOfSimplification(canonicalizedSchema.asJson)

          case AnyOf => val canonicalizedSchema = connectiveSchema.asArray.get.map(x => canonicalizeSchema(x)).toList
            Simplification.anyOfSimplification(canonicalizedSchema.asJson)



          case If => val canonicalIfSchema: Json =
            if (connectiveSchema.name.equals(ParserObject)) {
              if (!connectiveSchema.asObject.get.contains(Type)) {
                if (keys.contains(Type))
                  canonicalizeSchema(connectiveSchema.asObject.get.add(Type, sAsObj.apply(Type).get).asJson)
                else canonicalizeSchema(connectiveSchema)
              }
              else canonicalizeSchema(connectiveSchema)
            }
            else canonicalizeSchema(connectiveSchema)

            val thenSchema: Json = if (keys.contains(Then)) sAsObj.apply(Then).get
                                   else true.asJson

            val canonicalThenSchema: Json =
              if (thenSchema.name.equals(ParserObject)) {
                if (!thenSchema.asObject.get.contains(Type) && sAsObj.contains(Type)) canonicalizeSchema(thenSchema.asObject.get.add(Type, sAsObj.apply(Type).get).asJson)
                else thenSchema
              }
              else thenSchema

            val elseSchema: Json = if (keys.contains(Else)) sAsObj.apply(Else).get
            else true.asJson

            val canonicalElseSchema: Json =
              if (elseSchema.name.equals(ParserObject)) {
                if (!elseSchema.asObject.get.contains(Type) && sAsObj.contains(Type)) canonicalizeSchema(elseSchema.asObject.get.add(Type, sAsObj.apply(Type).get).asJson)
                else elseSchema
              }
              else elseSchema

            val positive = Simplification.allOfSimplification(List(canonicalIfSchema, canonicalThenSchema).asJson)

            val canonicalNotIf = Simplification.notSimplification(canonicalIfSchema)

            val negative = Simplification.allOfSimplification(List(canonicalNotIf, canonicalElseSchema).asJson)

            Simplification.anyOfSimplification(List(positive, negative).asJson)


          case DependentSchemas =>
            val depSchemas = connectiveSchema.asObject.get
            val depSchemasTranslated = depSchemas.toMap.map { case (k, v) =>
              val reqK = JsonObject.singleton(Type, JSObject.asJson).add(Required, List(k).asJson).asJson
              val notReqK = Simplification.notSimplification(reqK)
              val newV = if (v.name.equals(ParserObject))
                if (!v.asObject.get.keys.toList.contains(Type)) v.asObject.get.add(Type, JSObject.asJson).asJson
                else v
              else v
              val canonicalNewV = canonicalizeSchema(newV)

              Simplification.anyOfSimplification(List(notReqK, canonicalNewV).asJson)
            }
            Simplification.allOfSimplification(depSchemasTranslated.asJson)

        }
      }
      else
        schemaWithConnective.asJson
    }


    if ((connectivesAsObjects.size == 1) && keys.size == 1) {
      return connectivesCanonicalization.last
    }



    val nonConnectivesMap = nonConnectives.map(k => (k, sAsObj.apply(k).get))
    val nonConnectivesObject = JsonObject.fromIterable(nonConnectivesMap).asJson
    val canonicalizedNonConnectiveSchema = canonicalizeSchema(nonConnectivesObject)


    val allOfArray = (connectivesCanonicalization.asJson.asArray.get :+ canonicalizedNonConnectiveSchema).asJson
    val simplifiedAllOfArray = Simplification.allOfSimplification(allOfArray)
    simplifiedAllOfArray
  }


  def canonicalizeSchema(s: Json): Json = {

    val newS = s
    newS.name match {
      case ParserBool => newS

      case ParserObject =>

        val newSAsObj = newS.asObject.get

        if (newSAsObj.equals(JsonObject.empty))
          return true.asJson


        val connectives = NonTypeSpecificAssertions & newSAsObj.keys.toSet
        if (connectives.nonEmpty || newSAsObj.contains(DependentSchemas))
          return multipleConnectivesCanonicalization(newS)


        val typeNewS: Option[Json] = newSAsObj.apply(Type)

        typeNewS match {
          case Some(t) => t.name match {
            case ParserString => singleTypeCanonicalization(newS)
            case ParserArray => multipleTypesCanonicalization(newS)
          }
          case None => if (newSAsObj.size == 1 && (newSAsObj.keys.last.equals(Enum) || newSAsObj.keys.last.equals(Const))) newS
          else multipleTypesCanonicalization(missingTypeCanonicalization(newS))
        }

      case _ => throw new Exception(s"This is not a valid Json Schema: $newS")
    }
  }

}
