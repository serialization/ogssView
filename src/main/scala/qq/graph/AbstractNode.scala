package qq.graph

import de.ust.skill.common.scala.api;

/** the thing a node in the graph represents */
abstract class AbstractNode() {

}

case class SkillObjectNode(val skillObject: api.SkillObject)
    extends AbstractNode() {
  
  override def hashCode = skillObject.hashCode()
  override def equals(that: Any) = that match {
    case that: SkillObjectNode => that.skillObject == skillObject
    case _ => false
  }
}

case class NullNode()
extends AbstractNode() {
  /** all null nodes are different */
  override def equals(that: Any) = this == that
}