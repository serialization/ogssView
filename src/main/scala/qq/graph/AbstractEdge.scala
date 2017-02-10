package qq.graph

import de.ust.skill.common.scala.api
import de.ust.skill.common.scala.internal.fieldTypes._

/** the thing a link in the graph represents */
abstract class AbstractEdge {
  def getTo: AbstractNode
  def getFrom: AbstractNode
}

case class SimpleFieldEdge[T](
    val from: api.SkillObject,
    val field: api.FieldDeclaration[T])
    extends AbstractEdge {
  
  val to = from.get(field)
  
  override def hashCode = 17 + 31 * java.util.Objects.hash(from, field) + to.hashCode()
  override def equals(that: Object) = that match {
    case that: SimpleFieldEdge[T] =>
      from == that.from && field == that.field && to == that.to
    case _ => false
  }
  
  override def getFrom = new SkillObjectNode(from)
  override def getTo = field.t.asInstanceOf[FieldType[_]] match {
    case u: UserType[t] => if (to != null) new SkillObjectNode(to.asInstanceOf[t]) else new NullNode
    case a: AnnotationType => if (to != null) new SkillObjectNode(to.asInstanceOf[api.SkillObject]) else new NullNode
    
    
  }
  
 }
  

