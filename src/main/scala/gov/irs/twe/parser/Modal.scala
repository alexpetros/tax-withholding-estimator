package gov.irs.twe.parser

import gov.irs.twe.exceptions.InvalidFormConfig
import gov.irs.twe.TweTemplateEngine
import org.thymeleaf.context.Context
import scala.collection.mutable
import scala.xml.Elem

case class Modal(
    id: String,
    modalHeadingKey: String,
    modalElements: Seq[FlowNode],
) extends FlowNode {
  override def html(templateEngine: TweTemplateEngine): String = {
    val context = new Context()
    context.setVariable("modalId", this.id)
    val modalHeading = templateEngine.messageResolver.resolveMessage(modalHeadingKey)
    context.setVariable("modalHeading", modalHeading)
    val modalContent = modalElements.html(templateEngine)
    context.setVariable("modalContent", modalContent)

    templateEngine.process("nodes/modal-dialog", context)
  }
}

object Modal extends FlowNodeParser {
  override def fromXml(modalElement: Elem, flowParser: FlowParser, level: Int): Modal = {
    val id = modalElement \@ "id"
    if (id == null) {
      throw InvalidFormConfig(s"Modal is missing an id")
    }
    val modalHeadingNode = (modalElement \ "modal-heading").head
    if (modalHeadingNode.isEmpty) {
      throw InvalidFormConfig(s"Modal $id is missing a heading")
    }
    val modalContentNode = (modalElement \ "modal-content").collect { case e: xml.Elem => e }.head
    if (modalContentNode.isEmpty) {
      throw InvalidFormConfig(s"Modal $id is missing content")
    }

    val parentContext = flowParser.translationContext
    val currentLevelMap = flowParser.translationMap.getMap(parentContext)
    currentLevelMap += id -> mutable.LinkedHashMap.empty[String, Any]

    val modalIdLevelMap = currentLevelMap.getMap(List(id))
    val modalHeading = modalHeadingNode.child.mkString
    modalIdLevelMap += "heading" -> modalHeading

    val updatedTranslationContext = flowParser.translationContext :+ id
    flowParser.translationContext = updatedTranslationContext
    val modalElements = flowParser.parseChildElements(modalContentNode, level = level)
    flowParser.translationContext = flowParser.translationContext.dropRight(1)
    Modal(id, s"${updatedTranslationContext.mkString(".")}.heading", modalElements)
  }
}
