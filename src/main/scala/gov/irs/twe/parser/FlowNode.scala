package gov.irs.twe.parser

import gov.irs.twe.TweTemplateEngine
import scala.collection.mutable
import scala.xml.Elem

trait FlowNode {
  def html(templateEngine: TweTemplateEngine): String
}

extension (flowNodes: Seq[FlowNode]) {
  def html(templateEngine: TweTemplateEngine): String = flowNodes.map(node => node.html(templateEngine)).mkString("")
}

trait FlowNodeParser {
  def fromXml(element: Elem, flowParser: FlowParser, level: Int): FlowNode
}

trait FlowNodeParserWithCounts {
  def fromXml(
      element: Elem,
      flowParser: FlowParser,
      level: Int,
      tagCounts: mutable.Map[Int, mutable.Map[String, Int]],
  ): FlowNode

  def getAndUpdateTagCounts(
      tagCounts: mutable.Map[Int, mutable.Map[String, Int]],
      htmlElement: Elem,
      level: Int,
  ): String = {
    val currentLevelMap = tagCounts.getOrElse(level, mutable.Map.empty[String, Int])
    val updatedCount = currentLevelMap.getOrElse(htmlElement.label, -1) + 1
    currentLevelMap.update(htmlElement.label, updatedCount)
    tagCounts.update(level, currentLevelMap)
    s"${htmlElement.label}-$updatedCount"
  }
}
