package qq.graph

import de.ust.skill.common.scala.api;
import scala.collection.mutable.Buffer;
import scala.collection.mutable.HashSet;

/** the thing a node in the graph represents */
abstract class AbstractNode() {
  /** how it should display */
  def getUiElement(graph: Graph): swing.UIElement
  /** outgoing edges */
  def getOutEdge(file: qq.editor.File): Iterator[AbstractEdge]
}

case class SkillObjectNode(val skillObject: api.SkillObject)
    extends AbstractNode() {
 
  private def theNode = this
  override def getUiElement(graph: Graph) = {
    /* TODO context menu */
    new qq.util.PlainButton(
        new swing.Action(graph.viewer.page.file.idOfObj(skillObject)){
          override def apply() = {
            graph.viewer.expandCollapse(theNode)
          }
        }) {
      
    }
  }
  override def getOutEdge(file: qq.editor.File) = {
    val τ = file.s(skillObject.getTypeName)
    for (f <- τ.allFields
        if file.fieldSettings(f).visibilityIn(skillObject).showAsNode)
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
case class ListNode[E, C[E] <: Buffer[E]](val skillObject: api.SkillObject, val field: api.FieldDeclaration[C[E]])
    extends AbstractNode() {
  
  override def getUiElement(graph: Graph) = {
    /* TODO update?! */
    new swing.Label(s"[${skillObject.get(field).size}]")
  }
  override def getOutEdge(file: qq.editor.File) = Iterator() // TODO
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: ListNode[E, C] => that.skillObject == skillObject && that.field == field
    case _ => false
  }
}
case class SetNode[E, C[E] <: HashSet[E]](val skillObject: api.SkillObject, val field: api.FieldDeclaration[C[E]])
    extends AbstractNode() {
  
  override def getUiElement(graph: Graph) = {
    /* TODO update?! */
    new swing.Label(s"{${skillObject.get(field).size}}")
  }
  override def getOutEdge(file: qq.editor.File) = Iterator() // TODO
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: SetNode[E, C] => that.skillObject == skillObject && that.field == field
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