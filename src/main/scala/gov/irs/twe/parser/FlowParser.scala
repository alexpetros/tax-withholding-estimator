package gov.irs.twe.parser

import gov.irs.factgraph.FactDictionary
import gov.irs.twe.exceptions.InvalidFormConfig
import scala.collection.mutable
import scala.xml.Elem

enum FlowNodeType {
  case FG_ALERT
  case FG_APPLY
  case FG_COLLECTION
  case FG_DETAIL
  case FG_SET
  case FG_WITHHOLDING_ADJUSTMENTS
  case HTML
  case MODAL
  case PAGE
  case SECTION
}
object FlowNodeType:
  def fromLabel(label: String): FlowNodeType = label match
    // Named elements with custom parsing logic
    case "fg-alert"                   => FlowNodeType.FG_ALERT
    case "fg-apply"                   => FlowNodeType.FG_APPLY
    case "fg-collection"              => FlowNodeType.FG_COLLECTION
    case "fg-detail"                  => FlowNodeType.FG_DETAIL
    case "fg-set"                     => FlowNodeType.FG_SET
    case "fg-withholding-adjustments" => FlowNodeType.FG_WITHHOLDING_ADJUSTMENTS
    case "modal-dialog"               => FlowNodeType.MODAL
    case "page"                       => FlowNodeType.PAGE
    case "section"                    => FlowNodeType.SECTION
    // Treat all other elements as HTML
    case _ => FlowNodeType.HTML

case class FlowParser(
    factDictionary: FactDictionary,
) {
  var translationContext: List[String] = List.empty[String]
  val translationMap = mutable.LinkedHashMap.empty[String, Any]

  def parseChildElements(
      parent: Elem,
      excludedLabels: Seq[String] = Seq.empty[String],
      level: Int = 0,
      tagCounts: mutable.Map[Int, mutable.Map[String, Int]] = mutable.Map(),
  ): Seq[FlowNode] = {
    val childElements = (parent \ "_").filter(c => !excludedLabels.contains(c.label))

    if (childElements.isEmpty) {
      throw InvalidFormConfig(s"Encountered an empty element for which there is no parser configured: $parent")
    }
    childElements.collect { case element: Elem =>
      // These elements are effectively ignored from a translations pov and thus level is not incremented
      val ignoredElements = List("div", "details", "summary")
      val updatedLevel = if (ignoredElements.contains(element.label)) level else level + 1
      parseElement(element, updatedLevel, tagCounts)
    }
  }

  private def parseElement(
      element: Elem,
      level: Int = 0,
      tagCounts: mutable.Map[Int, mutable.Map[String, Int]],
  ): FlowNode =
    FlowNodeType.fromLabel(element.label) match {
      case FlowNodeType.FG_ALERT                   => FgAlert.fromXml(element, this, level, tagCounts)
      case FlowNodeType.FG_APPLY                   => FgApply.fromXml(element, this, level)
      case FlowNodeType.FG_COLLECTION              => FgCollection.fromXml(element, this, level)
      case FlowNodeType.FG_DETAIL                  => FgDetail.fromXml(element, this, level, tagCounts)
      case FlowNodeType.FG_SET                     => FgSet.fromXml(element, this, level)
      case FlowNodeType.FG_WITHHOLDING_ADJUSTMENTS => FgWithholdingAdjustments.fromXml(element, this, level)
      case FlowNodeType.HTML                       => Html.fromXml(element, this, level, tagCounts)
      case FlowNodeType.MODAL                      => Modal.fromXml(element, this, level)
      case FlowNodeType.SECTION                    => Section.fromXml(element, this, level)
      case FlowNodeType.PAGE                       =>
        throw InvalidFormConfig(
          s"Encountered 'page' element outside of flow config root. Pages are only supported as top-level flow config elements.",
        )
    }
}

extension (translationMap: mutable.LinkedHashMap[String, Any]) {
  def getMap(keys: List[String]): mutable.LinkedHashMap[String, Any] = {
    val output = keys.foldLeft(Option(translationMap: Any)) {
      case (Some(m: mutable.LinkedHashMap[String, Any] @unchecked), key) => m.get(key)
      case _ => throw new IllegalArgumentException("invalid key path to translation map")
    }
    output.get match
      case m: mutable.LinkedHashMap[String, Any] @unchecked => m
      case _                                                =>
        throw new IllegalArgumentException(
          s"expected value to be of type mutable.LinkedHashMap[String, Any], but was ${output.get.getClass.getName}",
        )
  }
}
