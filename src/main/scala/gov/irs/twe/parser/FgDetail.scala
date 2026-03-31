package gov.irs.twe.parser

import gov.irs.factgraph.FactDictionary
import gov.irs.twe.parser.{ Condition, FgDetail }
import gov.irs.twe.TweTemplateEngine
import org.thymeleaf.context.Context
import org.thymeleaf.TemplateEngine
import scala.collection.mutable
import scala.xml.Elem

case class FgDetail(
    summaryKey: String,
    children: Seq[FlowNode],
    useChevron: Boolean,
    detailsClass: Option[String],
    headingTag: String,
    open: Boolean,
    condition: Option[Condition],
) extends FlowNode {
  override def html(templateEngine: TweTemplateEngine): String = {
    val context = new Context()
    val summary = templateEngine.messageResolver.resolveMessage(summaryKey)
    context.setVariable("summary", summary)
    val childrenHtml = children.html(templateEngine)
    context.setVariable("childrenHtml", childrenHtml)
    context.setVariable("useChevron", java.lang.Boolean.valueOf(useChevron))
    context.setVariable("detailsClass", detailsClass.orNull)
    context.setVariable("headingTag", headingTag)
    context.setVariable("open", java.lang.Boolean.valueOf(open))
    context.setVariable("condition", condition.map(_.path).orNull)
    context.setVariable("operator", condition.map(c => c.operator.toString).orNull)

    templateEngine.process("nodes/fg-detail", context)
  }
}

object FgDetail extends FlowNodeParserWithCounts {
  private val VALID_HEADING_TAGS = Set("h2", "h3", "h4", "h5", "h6")

  override def fromXml(
      fgDetailElement: Elem,
      flowNodeParser: FlowParser,
      level: Int = 0,
      // This default value should only be used for tests, worth refactoring the tests that rely on this
      tagCounts: mutable.Map[Int, mutable.Map[String, Int]] = mutable.Map.empty,
  ): FgDetail = {
    val summary = (fgDetailElement \ "summary").headOption match {
      case Some(summaryNode) => summaryNode.child.map(_.toString).mkString.trim
      case None              => ""
    }
    val useChevron = (fgDetailElement \@ "icon") == "chevron"
    val classAttribute = (fgDetailElement \@ "class").trim
    val detailsClass = if (classAttribute.nonEmpty) Some(classAttribute) else None
    val rawHeadingTag = (fgDetailElement \@ "heading-tag").trim.toLowerCase
    val headingTag = if (VALID_HEADING_TAGS.contains(rawHeadingTag)) rawHeadingTag else "h4"
    val open = (fgDetailElement \@ "open").trim.equalsIgnoreCase("true")
    val condition = Condition.getCondition(fgDetailElement, flowNodeParser.factDictionary)

    val fgDetailWithCount = getAndUpdateTagCounts(tagCounts, fgDetailElement, level)
    val mapToBeUpdated = flowNodeParser.translationMap.getMap(flowNodeParser.translationContext)
    mapToBeUpdated += fgDetailWithCount -> mutable.LinkedHashMap.empty[String, Any]

    val currentMapLevel = mapToBeUpdated.getMap(List(fgDetailWithCount))
    if (summary.nonEmpty) {
      currentMapLevel += "summary" -> summary
    }

    flowNodeParser.translationContext = flowNodeParser.translationContext :+ fgDetailWithCount
    val summaryKey = flowNodeParser.translationContext.mkString(".") + ".summary"
    val childrenHtml = flowNodeParser.parseChildElements(fgDetailElement, List("summary"), level)
    flowNodeParser.translationContext = flowNodeParser.translationContext.dropRight(1)

    FgDetail(summary, childrenHtml, useChevron, detailsClass, headingTag, open, condition)
  }
}
