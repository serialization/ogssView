package qq.graph

import de.ust.skill.common.scala.api;

/** the thing a node in the graph represents */
abstract class AbstractNode() {
  /** how it should display */
  def getUiElement(graph: Graph): swing.UIElement
  /** outgoing edges */
  def getOutEdge(file: qq.editor.File): Iterator[AbstractEdge]
}

case class SkillObjectNode(val skillObject: api.SkillObject)
    extends AbstractNode() {
  
  override def getUiElement(graph: Graph) = {
    /* TODO context menu */
    new qq.util.PlainButton(
        new swing.Action(graph.page.file.idOfObj(skillObject)){
          override def apply() = {
            for (i <- getOutEdge(graph.page.file)) graph.addEdge(i)
          }
        }) {
      
    }
  }
  override def getOutEdge(file: qq.editor.File) = {
    val τ = file.s(skillObject.getTypeName)
    for (f <- τ.allFields
        if file.typeSettings(τ).fields(f).visibilityIn(skillObject).showAsNode)
      yield new SkillFieldEdge(skillObject, f)
  }
  override def hashCode = skillObject.hashCode()
  override def equals(that: Any) = that match {
    case that: SkillObjectNode => that.skillObject == skillObject
    case _ => false
  }
}

case class ValueNode[T](val skillObject: api.SkillObject, val field: api.FieldDeclaration[T])
    extends AbstractNode() {
  
  override def getUiElement(graph: Graph) = {
    /* TODO null strings? hex, what ever? edit? at least update?! */
    new swing.Label(skillObject.get(field).toString())
  }
  override def getOutEdge(file: qq.editor.File) = Iterator()
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: ValueNode[T] => that.skillObject == skillObject && that.field == field
    case _ => false
  }
}
case class NullNode[T](val skillObject: api.SkillObject, val field: api.FieldDeclaration[T])
extends AbstractNode() {
  override def getUiElement(graph: Graph) = new swing.Label("⊥")
  override def getOutEdge(file: qq.editor.File) = Iterator()
 
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: ValueNode[T] => that.skillObject == skillObject && that.field == field
    case _ => false
  }
}