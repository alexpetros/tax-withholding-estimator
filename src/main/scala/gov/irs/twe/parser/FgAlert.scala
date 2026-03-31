package gov.irs.twe.parser

import gov.irs.twe.parser.{ Condition, ConditionOperator }
import gov.irs.twe.parser.{ FgAlert, FlowNode, FlowNodeParser, FlowParser }
import gov.irs.twe.TweTemplateEngine
import org.thymeleaf.context.Context
import scala.collection.mutable
import scala.xml.Elem

case class FgAlert(
    condition: Option[Condition],
    alertType: String,
    headingKey: String,
    children: Seq[FlowNode],
) extends FlowNode {
  override def html(templateEngine: TweTemplateEngine): String = {
    val context = new Context()
    context.setVariable("condition", condition.map(_.path).orNull)
    context.setVariable("operator", condition.map(_.operator.toString).orNull)
    context.setVariable("alertType", alertType)
    val heading = templateEngine.messageResolver.resolveMessage(headingKey)
    context.setVariable("heading", heading)
    val childrenHtml = children.html(templateEngine)
    context.setVariable("children", childrenHtml)

    templateEngine.process("nodes/fg-alert", context)
  }
}

object FgAlert extends FlowNodeParserWithCounts {
  override def fromXml(
      fgAlertElement: Elem,
      flowParser: FlowParser,
      level: Int,
      tagCounts: mutable.Map[Int, mutable.Map[String, Int]],
  ): FgAlert = {
    val alertType = fgAlertElement \@ "alert-type"

    val heading = (fgAlertElement \ "heading").head.child.mkString.trim

    val conditionPath = fgAlertElement \@ "condition"
    val conditionOperator = fgAlertElement \@ "operator"
    val condition = Option.when(conditionPath.nonEmpty && conditionOperator.nonEmpty)(
      Condition(conditionPath, ConditionOperator.fromAttribute(conditionOperator)),
    )

    val alertCount = getAndUpdateTagCounts(tagCounts, fgAlertElement, level)
    val mapToBeUpdated = flowParser.translationMap.getMap(flowParser.translationContext)
    mapToBeUpdated += alertCount -> mutable.LinkedHashMap.empty[String, Any]

    val currentMapLevel = mapToBeUpdated.getMap(List(alertCount))
    currentMapLevel += "heading" -> heading

    flowParser.translationContext = flowParser.translationContext :+ alertCount
    val headingKey = flowParser.translationContext.mkString(".") + ".heading"
    val children = flowParser.parseChildElements(fgAlertElement, List("heading"), level)
    flowParser.translationContext = flowParser.translationContext.dropRight(1)

    FgAlert(condition, alertType, headingKey, children)
  }
}
