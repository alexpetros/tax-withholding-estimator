package gov.irs.twe.parser

import gov.irs.twe.TweTemplateEngine
import scala.collection.mutable
import scala.xml.Elem

abstract class Html extends FlowNode

case class HtmlLeafNode(htmlElement: Elem, openTag: String, closeTag: String, translationKey: String) extends Html {
  override def html(templateEngine: TweTemplateEngine): String = {
    val content = templateEngine.messageResolver.resolveMessage(translationKey)
    s"$openTag$content$closeTag"
  }
}

case class HtmlWithChildren(openTag: String, closeTag: String, children: Seq[FlowNode]) extends Html {
  override def html(templateEngine: TweTemplateEngine): String = {
    // parse children
    val childrenHtml = children.html(templateEngine)

    // return children inside of current element tag as html
    s"$openTag$childrenHtml$closeTag"
  }
}

object Html extends FlowNodeParserWithCounts {
  override def fromXml(
      htmlElement: Elem,
      flowParser: FlowParser,
      level: Int,
      tagCounts: mutable.Map[Int, mutable.Map[String, Int]],
  ): Html = {
    val openTag = getOpenTag(htmlElement)
    val closeTag = getClosingTag(htmlElement)
    if (isLeafNode(htmlElement)) {
      val updatedCount = getAndUpdateTagCounts(tagCounts, htmlElement, level)
      val parentContext = flowParser.translationContext
      val translationKey = s"${parentContext.mkString(".")}.$updatedCount"
      val mapToBeUpdated = flowParser.translationMap.getMap(parentContext)
      mapToBeUpdated += updatedCount -> htmlElement.head.child.mkString.strip

      HtmlLeafNode(htmlElement, openTag, closeTag, translationKey)
    } else {
      var children: Seq[FlowNode] = Seq.empty
      var lvl = level
      if (htmlElement.label == "ul" | htmlElement.label == "ol") {
        val updatedCount = getAndUpdateTagCounts(tagCounts, htmlElement, level)
        val parentContext = flowParser.translationContext
        val mapToBeUpdated = flowParser.translationMap.getMap(parentContext)
        mapToBeUpdated += updatedCount -> mutable.LinkedHashMap.empty[String, Any]

        flowParser.translationContext = flowParser.translationContext :+ updatedCount
        children = flowParser.parseChildElements(htmlElement, level = lvl)
        flowParser.translationContext = flowParser.translationContext.dropRight(1)
      } else {
        children = flowParser.parseChildElements(htmlElement, level = lvl, tagCounts = tagCounts)
      }
      HtmlWithChildren(openTag, closeTag, children)
    }
  }

  private def getOpenTag(htmlElem: Elem): String = {
    val tag = htmlElem.label
    val attrs = htmlElem.attributes.asAttrMap
      .map { case (k, v) => s"""$k="$v"""" }
      .mkString(" ")
    val openTag = if (attrs.isEmpty) s"<$tag>" else s"<$tag $attrs>"
    openTag
  }

  private def getClosingTag(htmlElem: Elem): String = {
    val tag = htmlElem.label
    s"</$tag>"
  }

  private val LEAF_NODES = Set("p", "li", "caption", "th", "td", "h1", "h2", "h3", "h4", "h5", "h6")
  private def isLeafNode(element: xml.Elem) = {
    LEAF_NODES.contains(element.label)
  }
}
