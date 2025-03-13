package utils

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import utils.Assertions.{Contains, Def, Definitions, DependentRequired, DependentSchemas, Else, ExclusiveMaximum, ExclusiveMinimum, If, Items, MaxContains, MaxLength, Maximum, MinContains, Minimum, PatternProperties, PrefixItems, Properties, Then}
import utils.DataTypes.{ParserArray, ParserBool, ParserObject}

object DraftTranslation {

  def modifyExclusive(assertion1: String, assertion2: String, s: JsonObject): JsonObject = {
    var obj = s
    obj.apply(assertion1).get.name match {
      case ParserBool => Option(obj.apply(assertion2)) match {
        case Some(Some(m)) => if(obj.apply(assertion1).get.asBoolean.get) obj = obj.remove(assertion1).add(assertion1,m)
        else obj = obj.remove(assertion1)
        case _ => obj = obj.remove(assertion1)
      }
      case _ =>
    }

    obj
  }
  def translate(s: Json): Json = {

    s.name match {
      case ParserObject =>
        var sAsObj = s.asObject.get
        if(sAsObj.contains(ExclusiveMinimum))
          sAsObj = modifyExclusive(ExclusiveMinimum,Minimum,sAsObj)

        if(sAsObj.contains(ExclusiveMaximum))
          sAsObj = modifyExclusive(ExclusiveMaximum,Maximum,sAsObj)

        if(sAsObj.contains(Items)) {
          sAsObj.apply(Items).get.name match {
            case ParserArray => val itemsTuple = sAsObj.apply(Items).get
              sAsObj = sAsObj.remove(Items).add(PrefixItems,itemsTuple)
              if(sAsObj.contains("additionalItems")) {
                val addItems = sAsObj.apply("additionalItems").get
                sAsObj = sAsObj.remove("additionalItems").add(Items,addItems)
              }
            case _ =>
          }
        }

        if(sAsObj.contains("dependencies")) {
          val schemaDependencies = sAsObj.apply("dependencies").get
          var dependentRequired = JsonObject.empty
          var dependentSchemas = JsonObject.empty

          schemaDependencies.name match {
            case ParserObject => schemaDependencies.asObject.get.toMap.foreach {
              case (k, v) => v.name match {
                case ParserObject => dependentSchemas = dependentSchemas.add(k, v)
                case ParserArray => dependentRequired = dependentRequired.add(k, v)
                case _ =>
              }
            }
            case _ =>
          }

          sAsObj = sAsObj.remove("dependencies")
          if(dependentRequired.size>0) sAsObj = sAsObj.add(DependentRequired,dependentRequired.asJson)
          if(dependentSchemas.size>0) {
            sAsObj = sAsObj.add(DependentSchemas,dependentSchemas.asJson)
          }

        }


        if(!sAsObj.contains(If)) {
          if(sAsObj.contains(Then)) sAsObj = sAsObj.remove(Then)
          if(sAsObj.contains(Else)) sAsObj = sAsObj.remove(Else)
        }

        if (!sAsObj.contains(Contains)) {
          if (sAsObj.contains(MinContains)) sAsObj = sAsObj.remove(MinContains)
          if (sAsObj.contains(MaxContains)) sAsObj = sAsObj.remove(MaxContains)
        }

        sAsObj = JsonObject.fromMap(sAsObj.toMap.map{
          case (k,v) => if(v.name.equals(ParserObject)) {
            if(k.equals(Properties) || k.equals(PatternProperties) || k.equals(Def) || k.equals(Definitions)) {
              (k,v.asObject.get.mapValues(x => translate(x)).asJson)
            }
            else
              (k,translate(v))
          }
          else if(v.name.equals(ParserArray)) (k,v.asArray.get.map(i => translate(i)).asJson)
          else (k,v)
        })
        sAsObj.asJson

      case _ => s
    }

  }
}
