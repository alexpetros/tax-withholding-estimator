package gov.irs.twe.parser

import gov.irs.twe.exceptions.InvalidFormConfig
import gov.irs.twe.parser.Utils.optionString
import gov.irs.twe.TweTemplateEngine
import scala.collection.mutable
import scala.util.matching.Regex
import scala.xml.Elem

case class Page(
    titleKey: String,
    route: String,
    exclude: Boolean,
    children: Seq[FlowNode],
) extends FlowNode {
  val href: String = "/app/tax-withholding-estimator" + route + (if (route == "/") "" else "/")

  override def html(templateEngine: TweTemplateEngine): String = {
    val pageContent = children.html(templateEngine)
    // Coerce all fg-show nodes into open, empty tags because HTML doesn't allow custom, self-closing tags
    val regex = new Regex("""<fg-show ([^>]*)>""", "attributes")
    val pageHtml = regex.replaceAllIn(
      pageContent,
      m => s"<fg-show \\${m group "attributes"}></fg-show>",
    )

    pageHtml
  }
}

object Page extends FlowNodeParser {
  override def fromXml(page: Elem, flowParser: FlowParser, level: Int = 0): Page = {
    val route =
      optionString(page \@ "route").getOrElse(throw InvalidFormConfig("<page> is missing a route attribute"))
    val title =
      optionString(page \@ "title").getOrElse(throw InvalidFormConfig("<page> is missing a title attribute"))
    val exclude = (page \@ "exclude-from-stepper").toBooleanOption.getOrElse(false)

    flowParser.translationContext = List(route)
    flowParser.translationMap += route -> mutable.LinkedHashMap.empty[String, Any]
    val mapToBeUpdated = flowParser.translationMap.getMap(List(route))
    mapToBeUpdated += "title" -> title

    val titleKey = flowParser.translationContext.mkString(".") + ".title"
    val children = flowParser.parseChildElements(page)
    Page(titleKey, route, exclude, children)
  }
}
