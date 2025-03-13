package utils

import io.circe.Json
import io.circe.syntax.EncoderOps
import utils.Assertions.{AnyOf, Const, Contains, Enum, Items, MaxContains, MaxItems, MaxProperties, Maximum, MinContains, MinItems, MinProperties, Minimum, MultipleOf, Not, Pattern, PatternProperties, PrefixItems, PropertyNames, Required, Type}
import utils.DataTypes.{JSArray, JSBool, JSInteger, JSNull, JSNumber, JSObject, JSString, ParserArray, ParserBool, ParserObject, ParserString, TypesMap}
import utils.Functions.{getNewPattern, hasMultipleInInterval, isMultiple, lcm, patternToAutomaton}

object UNSATPreconditions {


  def nullUnsat(s: Json): Boolean = {
    if (s.asObject.get.contains(Enum) && !s.asObject.get.apply(Enum).get.asArray.get.contains(Json.Null)) return true
    false
  }


  def boolUnsat(s: Json): Boolean = {
    val sAsObj = s.asObject.get

    val const: Option[Json] = sAsObj.apply(Const) match {
      case Some(c) => Some(c)
      case _ => None
    }

    val enum: Option[Vector[Json]] = sAsObj.apply(Enum) match {
      case Some(e) => Some(e.asArray.get)
      case _ => None
    }

    const match {
      case Some(c) => enum match {
        case Some(e) =>
          if (!e.exists(i => i.name.equals(ParserBool))) return true
          if (!e.contains(c)) return true
        case _ =>
      }
      case _ => enum match {
        case Some(e) =>
          if (!e.exists(i => i.name.equals(ParserBool))) return true
        case _ =>
      }
    }

    false
  }


  def numberUnsat(s: Json): Boolean = {

    val sAsObj = s.asObject.get
    val sType = sAsObj.apply(Type).get.asString.get

    val valMin: Option[Double] = Option(sAsObj.apply(Minimum)) match {
      case Some(Some(m)) => Some(m.asNumber.get.toDouble)
      case _ => None
    }

    val valMax: Option[Double] = Option(sAsObj.apply(Maximum)) match {
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

    valMof match {
      case Some(mof) => if (mof == 0) return true
      case _ =>
    }
    (valMin, valMax) match {
      case (Some(min), Some(max)) => if (min > max) return true
      case _ =>
    }
    (valMof, valMin, valMax) match {
      case (Some(mof), Some(min), Some(max)) => if (!hasMultipleInInterval(mof,
        min, max)) return true
      case _ =>
    }
    (valMof, valNotMof) match {
      case (Some(mof), Some(notMof)) => if (isMultiple(mof, notMof)) return true
      case _ =>
    }

    valConst match {
      case Some(const) =>
        if (constUnsat(s, const)) return true

        valEnum match {
          case Some(enum) => if (!enum.contains(const)) return true
          case _ =>
        }

      case _ => valEnum match {
        case Some(enum) => if (enum.isEmpty) return true
          if (enum.map(e => constUnsat(s, e)).reduce((x, y) => x && y)) return true
        case _ =>
      }
    }

    false
  }


  def stringUnsat(s: Json): Boolean = {

    val sAsObj = s.asObject.get


    sAsObj.apply(Pattern) match {
      case Some(p) => val automaton = patternToAutomaton(getNewPattern(p.asString.get))
        if (automaton.isEmpty) return true
      case _ =>
    }

    val valConst: Option[Json] = sAsObj.apply(Const) match {
      case Some(c) => Some(c)
      case _ => None
    }

    val valEnum: Option[Vector[Json]] = sAsObj.apply(Enum) match {
      case Some(enum) => Some(enum.asArray.get.filter(e => e.name.equals(ParserString)))
      case _ => None
    }

    valConst match {
      case Some(const) => if (constUnsat(s, const)) return true
        valEnum match {
          case Some(enum) => if (!enum.contains(const)) return true
          case _ =>
        }
      case _ => valEnum match {
        case Some(enum) => if (enum.isEmpty) return true
          if (enum.map {
              e => constUnsat(s, e)
            }.
            reduce((x, y) => x && y)) return true
        case _ =>
      }
    }

    false
  }


  def objectUnsat(s: Json): Boolean = {

    val sAsObj = s.asObject.get

    val valConst: Option[Json] = sAsObj.apply(Const) match {
      case Some(const) => Some(const)
      case _ => None
    }

    val valEnum: Option[Vector[Json]] = sAsObj.apply(Enum) match {
      case Some(enum) => Some(enum.asArray.get.filter(e => e.name.equals(ParserObject)))
      case _ => None
    }

    val valMinPro: Option[Int] = sAsObj.apply(MinProperties) match {
      case Some(min) => Some(min.asNumber.get.toInt.get)
      case _ => None
    }

    val valMaxPro: Option[Int] = sAsObj.apply(MaxProperties) match {
      case Some(max) => Some(max.asNumber.get.toInt.get)
      case _ => None
    }

    val valReq: Option[Vector[String]] = sAsObj.apply(Required) match {
      case Some(req) => Some(req.asArray.get.map(k => k.asString.get))
      case _ => None
    }

    val valPropNames: Option[Json] = sAsObj.apply(PropertyNames) match {
      case Some(propNames) => Some(propNames)
      case _ => None
    }


    (valMinPro, valMaxPro) match {
      case (Some(min), Some(max)) => if (min > max) return true
      case _ =>
    }


    (valMaxPro, valReq) match {
      case (Some(max), Some(req)) => if (req.size > max) return true
      case _ =>
    }

    (valMinPro, valPropNames) match {
      case (Some(min), Some(propNames)) => if (propNames.name.equals(ParserBool) && !propNames.asBoolean.get && min > 0)
        return true
      case _ =>
    }

    (valReq, valPropNames) match {
      case (Some(req), Some(propNames)) =>
        val propNamesPattern = if (propNames.name.equals(ParserObject) && propNames.asObject.get.contains(Pattern))
          propNames.asObject.get.apply(Pattern).get.asString.get
          else ".*"
        val automaton = patternToAutomaton(propNamesPattern)
        val existsNoMatchingReq = req.map(k => automaton.run(k)).reduce((x, y) => x && y)
        if (!existsNoMatchingReq) return true

      case _ =>
    }

    valConst match {
      case Some(const) => if (constUnsat(s, const)) return true
        valEnum match {
          case Some(enum) => if (!enum.contains(const)) return true
          case _ =>
        }
      case _ => valEnum match {
        case Some(enum) => if (enum.isEmpty) return true
          if (enum.map(e => constUnsat(s, e)).reduce((x, y) => x && y)) return true
        case _ =>
      }
    }

    false
  }


  def arrayUnsat(s: Json): Boolean = {
    val sAsObj = s.asObject.get

    val valMinItems: Option[Int] = sAsObj.apply(MinItems) match {
      case Some(min) => Some(min.asNumber.get.toInt.get)
      case _ => None
    }

    val valMaxItems: Option[Int] = sAsObj.apply(MaxItems) match {
      case Some(max) => Some(max.asNumber.get.toInt.get)
      case _ => None
    }

    val valPrefixItems: Option[Vector[Json]] = sAsObj.apply(PrefixItems) match {
      case Some(prefixItems) => Some(prefixItems.asArray.get)
      case _ => None
    }

    val valItems: Option[Json] = sAsObj.apply(Items) match {
      case Some(items) => Some(items)
      case _ => None
    }

    val valContains: Option[Json] = sAsObj.apply(Contains) match {
      case Some(contains) => Some(contains)
      case _ => None
    }

    val valMinContains: Option[Int] = sAsObj.apply(MinContains) match {
      case Some(minContains) => Some(minContains.asNumber.get.toInt.get)
      case _ => None
    }

    val valMaxContains: Option[Int] = sAsObj.apply(MaxContains) match {
      case Some(maxContains) => Some(maxContains.asNumber.get.toInt.get)
      case _ => None
    }

    val valConst: Option[Json] = sAsObj.apply(Const) match {
      case Some(const) => Some(const)
      case _ => None
    }

    val valEnum: Option[Vector[Json]] = sAsObj.apply(Enum) match {
      case Some(enum) => Some(enum.asArray.get.filter(e => e.name.equals(ParserArray)))
      case _ => None
    }

    (valMinItems, valMaxItems) match {
      case (Some(min), Some(max)) => if (min > max) return true
      case _ =>
    }

    (valContains, valMinContains, valMaxContains) match {
      case (Some(_), Some(min), Some(max)) => if (min > max) return true
      case _ =>
    }

    (valContains, valMinContains, valMaxItems) match {
      case (Some(_), Some(min), Some(max)) => if (min > max) return true
      case _ =>
    }

    (valPrefixItems, valItems, valMinItems) match {
      case (Some(prefixItems), Some(items), Some(min)) => if (prefixItems.size < min && items.equals(false.asJson)) return true
      case _ =>
    }

    (valContains, valMaxItems) match {
      case (Some(_), Some(max)) => if (max == 0) return true
      case _ =>
    }

    (valPrefixItems, valMinItems) match {
      case (Some(prefixItems), Some(min)) => if (prefixItems.size >= min) {
        val subPrefixItems = prefixItems.take(min)
        if (subPrefixItems.contains(false.asJson)) return true
      }
      case _ =>
    }

    (valPrefixItems, valContains, valMinContains) match {
      case (Some(prefixItems), Some(_), Some(min)) => val subPrefixItems = prefixItems.take(min)
        if (subPrefixItems.contains(false.asJson)) return true
      case _ =>
    }

    valConst match {
      case Some(const) => if (constUnsat(s, const)) return true
        valEnum match {
          case Some(enum) => if (!enum.contains(const)) return true
          case _ =>
        }
      case _ => valEnum match {
        case Some(enum) => if (enum.isEmpty) return true
          if (enum.map(e => constUnsat(s, e)).reduce((x, y) => x && y)) return true
        case _ =>
      }
    }

    false
  }


  def constUnsat(s: Json, valConst: Json): Boolean = {
    if (s.name.equals(ParserBool)) {
      if (s.asBoolean.get) return false
      else return true
    }
    val sAsObj = s.asObject.get

    if (sAsObj.contains(Const) && !sAsObj.apply(Const).get.equals(valConst)) return true

    if (sAsObj.contains(Enum) && !sAsObj.apply(Enum).get.asArray.get.contains(valConst)) return true

    if (sAsObj.contains(Type)) {

      val sType = sAsObj.apply(Type).get.asString.get
      val constType = valConst.name

      if (!TypesMap(sType).equals(constType)) true
      else {
        sType match {
          case JSNull => false
          case JSBool => false
          case JSNumber | JSInteger =>

            val valMin: Option[Double] = Option(sAsObj.apply(Minimum)) match {
              case Some(Some(m)) => Some(m.asNumber.get.toDouble)
              case _ => None
            }

            val valMax: Option[Double] = Option(sAsObj.apply(Maximum)) match {
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

            val c = valConst.asNumber.get.toDouble
            valMin match {
              case Some(min) => if (c < min) return true
              case _ =>
            }
            valMax match {
              case Some(max) => if (c > max) return true
              case _ =>
            }
            valMof match {
              case Some(mof) => if (!isMultiple(c, mof)) return true
              case _ =>
            }

            valNotMof match {
              case Some(notMof) => if (isMultiple(c, notMof)) return true
              case _ =>
            }
            false

          case JSString =>

            val pattern: Option[String] = sAsObj.apply(Pattern) match {
              case Some(p) => Some(getNewPattern(p.asString.get))
              case _ => None
            }

            pattern match {
              case Some(p) => val c = valConst.asString.get
                val automata = patternToAutomaton(p)
                return !automata.run(c)
              case _ =>
            }
            false

          case JSObject =>
            val valMinPro: Int = sAsObj.apply(MinProperties) match {
              case Some(min) => min.asNumber.get.toInt.get
              case _ => 0
            }

            val valMaxPro: Option[Int] = sAsObj.apply(MaxProperties) match {
              case Some(max) => Some(max.asNumber.get.toInt.get)
              case _ => None
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

            val constAsObj = valConst.asObject.get
            val constKeys = constAsObj.keys.toList

            if (constAsObj.size < valMinPro) return true


            valMaxPro match {
              case Some(max) => if (constAsObj.size > max) return true
              case _ =>
            }

            valReq match {
              case Some(req) => if (!constAsObj.keys.toList.containsSlice(req)) return true
              case _ =>
            }

            valPropNames match {
              case Some(propNames) => val propNamesPattern = if (propNames.name.equals(ParserObject) && propNames.asObject.get.contains(Pattern))
                propNames.asObject.get.apply(Pattern).get.asString.get
              else ".*"
                val automaton = patternToAutomaton(propNamesPattern)
                val constKeys = constAsObj.keys
                val existsNoMatching = constKeys.map(k => automaton.run(k)).reduce((x, y) => x && y)
                if (!existsNoMatching) return true

              case _ =>
            }


            if (valConst.asObject.get.size > 1) {
              valPattProps match {
                case Some(pattProps) => val pattPropsAsObj = pattProps.asObject.get
                  val pattPropsKeys = pattPropsAsObj.keys.toList
                  val matchingKeysPatterns = constKeys.map(k => (k, pattPropsKeys.filter(p => patternToAutomaton(p).run(k))))

                  val existsNoMatching = matchingKeysPatterns.map { case (k, patterns) =>
                    if (patterns.nonEmpty) {
                      patterns.map { p =>
                        val schemaPattern = pattPropsAsObj.apply(p).get
                        val keyValue = constAsObj.apply(k).get
                        constUnsat(schemaPattern, keyValue)
                      }.reduce((x, y) => x || y)
                    } else false
                  }.reduce((x, y) => x || y)

                  if (existsNoMatching) return true
                case _ =>
              }
            }
            false

          case JSArray =>
            val valMinItems: Int = sAsObj.apply(MinItems) match {
              case Some(min) => min.asNumber.get.toInt.get
              case _ => 0
            }

            val valMaxItems: Option[Int] = sAsObj.apply(MaxItems) match {
              case Some(max) => Some(max.asNumber.get.toInt.get)
              case _ => None
            }

            val valPrefixItems: Option[Vector[Json]] = sAsObj.apply(PrefixItems) match {
              case Some(prefixItems) => Some(prefixItems.asArray.get)
              case _ => None
            }

            val valItems: Option[Json] = sAsObj.apply(Items) match {
              case Some(items) => Some(items)
              case _ => None
            }

            val valContains: Option[Json] = sAsObj.apply(Contains) match {
              case Some(contains) => Some(contains)
              case _ => None
            }

            val valMinContains: Option[Int] = sAsObj.apply(MinContains) match {
              case Some(minContains) => Some(minContains.asNumber.get.toInt.get)
              case _ => None
            }

            val valMaxContains: Option[Int] = sAsObj.apply(MaxContains) match {
              case Some(maxContains) => Some(maxContains.asNumber.get.toInt.get)
              case _ => None
            }

            val constAsArray = valConst.asArray.get
            val constSize = constAsArray.size

            if (constAsArray.size < valMinItems) return true

            valMaxItems match {
              case Some(max) => if (constAsArray.size > max) return true
              case _ =>
            }

            valPrefixItems match {
              case Some(prefixItems) => val minSize = if (prefixItems.size < constSize) prefixItems.size else constSize
                for (i <- 0 until minSize) {
                  if (constUnsat(prefixItems(i), constAsArray(i))) return true
                }
                // Verify const items against items if size of const is bigger than prefixItems
                if (minSize < constSize) {
                  valItems match {
                    case Some(items) => val constAddItems = constAsArray.takeRight(constSize - minSize)
                      // use a normal for loop instead of map reduce: if one is unsat break immediately
                      //                      if(constAddItems.map(i => constUnsat(items,i)).reduce((x,y) => x||y)) return true
                      for (i <- 0 until constSize - minSize) {
                        if (constUnsat(items, constAddItems(i))) return true
                      }
                    case _ =>
                  }
                }

              case _ => valItems match {
                case Some(items) => for (i <- 0 until constSize) {
                  if (constUnsat(items, constAsArray(i))) return true
                }
                case _ =>
              }
            }

            valContains match {
              case Some(contains) => val matchingContains = constAsArray.map(i => constUnsat(contains, i))
                val nbSat = matchingContains.count(_ == false)
                if (nbSat == 0) return true
                valMinContains match {
                  case Some(min) => if (nbSat < min) return true
                  case _ =>
                }
                valMaxContains match {
                  case Some(max) => if (nbSat > max) return true
                  case _ =>
                }
              case _ =>
            }

            false
        }
      }
    }

    else if (sAsObj.contains(AnyOf)) {
      val anyOfArray = sAsObj.apply(AnyOf).get.asArray.get
      for (subSchema <- anyOfArray) {
        if (!constUnsat(subSchema, valConst)) return false
      }
      true
    }

    else
      false
  }

}
