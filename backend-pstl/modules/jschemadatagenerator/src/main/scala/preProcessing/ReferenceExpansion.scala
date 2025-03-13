package preProcessing

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import utils.Assertions.{AllOf, Enum, Ref}
import utils.DataTypes.{ParserArray, ParserObject, ParserString}

object ReferenceExpansion {


  def getRefSchema(uri: String, s: Json): Json = {

    var newS = s
    val refList = uri.drop(1).split("/").toList.filter(k => k.nonEmpty)
    for (i <- refList.indices) {
      newS.name match {
        case ParserObject =>
          val frag = refList(i) //if(refList(i).equals("items")) PrefixItems else
          newS.asObject.get.apply(frag) match {
            case Some(referencedS) => newS = referencedS
            case _ => throw new Exception("Reference not found")
          }
        case ParserArray => val index = refList(i)
          index.toIntOption match {
            case Some(idx) => if (idx > newS.asArray.get.size - 1) throw new Exception("Reference not found")
            else newS = newS.asArray.get.apply(idx)
            case _ => throw new Exception("Reference not found")
          }
        case _ => return newS
      }
    }

    newS
  }


  def resolveReferences(originalS: Json, s: Json, context: List[String]): Json = {
    var c: List[String] = context
    var newS = s
    newS.name match {
      case ParserObject =>
        val sAsObj = newS.asObject.get

        if (sAsObj.contains(Ref) && sAsObj.apply(Ref).get.name.equals(ParserString)) {
          val uri = sAsObj.apply(Ref).get.asString.get
          if (context.contains(uri)) newS = false.asJson
          else if (sAsObj.size == 1) {
            c = c :+ uri
            newS = resolveReferences(originalS, getRefSchema(uri, originalS), c)
          }
          else {
            c = c :+ uri
            val sAsObjWithoutRef = sAsObj.remove(Ref)
            newS = resolveReferences(originalS, JsonObject.singleton(AllOf, List(getRefSchema(uri, originalS), sAsObjWithoutRef.asJson).asJson).asJson, c)
          }
        }
        else {
          newS = sAsObj.toMap.map {
            case (k, v) => if (k.equals(Enum)) (k, v) else (k, resolveReferences(originalS, v, c))
          }.asJson
        }
        newS

      case ParserArray => newS = newS.asArray.get.map(i => resolveReferences(originalS, i, c)).asJson
        newS


      case _ => newS
    }
  }

}
