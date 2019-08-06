package qq.graph

import ogss.common.scala.api
import ogss.common.scala.internal
import ogss.common.scala.internal.fieldTypes._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import qq.util.Vector
import qq.editor.File
import qq.util.FlattenedMap

/** the thing a link in the graph represents */
abstract class AbstractEdge {
  /** start node in the abstract sense (before it has a position…) */
  def getTo: AbstractNode
  /** end node */
  def getFrom: AbstractNode
  /** label for the edge */
  def textLabel(file: File): String
  /** prefereed direction of the edge in `file` */
  def idealDirection(file: File): Vector
  /** decoration at the to-end of the edge */
  def toDecoration: EdgeDecoration = SmallArrowDecoration
}

/** edge representing a skill field */
case class SkillFieldEdge[T](
    val from: internal.Obj,
    val field: api.FieldAccess[T])
    extends AbstractEdge {
  
  val to = field.get(from)
  
  override def hashCode = 17 + 31 * java.util.Objects.hash(from, field) + (if(to != null) to.hashCode() else 0)
  override def equals(that: Any) = that match {
    case that: SkillFieldEdge[T] =>
      from == that.from && field == that.field && to == that.to
    case _ => false
  }
  
  override def getFrom = new SkillObjectNode(from)
  override def getTo = field.t.asInstanceOf[internal.FieldType[_]] match {
    case u: internal.Pool[t] => if (to != null) new SkillObjectNode(to.asInstanceOf[t]) else new NullNode(from, field)
    case a: internal.AnyRefType => if (to != null) new SkillObjectNode(to.asInstanceOf[internal.Obj]) else new NullNode(from, field)
    case c: ArrayType[e] => new ListNode(from, field.asInstanceOf[api.FieldAccess[Buffer[e]]])
    case c: ListType[e] => new ListNode(from, field.asInstanceOf[api.FieldAccess[Buffer[e]]])
    case c: SetType[e] => new SetNode(from, field.asInstanceOf[api.FieldAccess[HashSet[e]]])
    case c: MapType[k,v] => new MapNode(from, field.asInstanceOf[api.FieldAccess[HashMap[k,v]]])
    case I8| I16| I32| I64| V64| F32| F64| _:internal.StringPool| Bool =>
      new ValueNode(from, field)
  }
  override def textLabel(file: File) = field.name
  
  override def idealDirection(file: File) = file.fieldPreferences(field).prefEdgeDirection()
  
 }
  
/** edge from a list node to the list member */
case class ListMemberEdge[E, C[E] <: Buffer[E]](
    val from: internal.Obj,
    val field: api.FieldAccess[C[E]],
    val index: Int)
    extends AbstractEdge {
  
  override def getFrom = new ListNode(from, field)
  override def getTo = field.t.asInstanceOf[SingleArgumentType[_,_]].base match {
    case u: internal.Pool[t] => if (field.get(from)(index) != null) new SkillObjectNode(field.get(from)(index).asInstanceOf[t]) else new ListNullNode(from, field, index)
    case a: internal.AnyRefType => if (field.get(from)(index) != null) new SkillObjectNode(field.get(from)(index).asInstanceOf[internal.Obj]) else new ListNullNode(from, field, index)
    case _: ArrayType[_] | _: ListType[_] | _:SetType[_] | _: MapType[_,_] =>
      throw new Exception("Nested container.")
    case I8| I16| I32| I64| V64| F32| F64| _:internal.StringPool| Bool =>
      new ListValueNode(from, field, index)
  }
    
    
  override def textLabel(file: File) = index.toString()
  
  // TODO do collection members share the ideal direction?
  override def idealDirection(file: File) = file.fieldPreferences(field).prefEdgeDirection()
}

/** edge from a set node to the set member */
case class SetMemberEdge[E, C[E] <: HashSet[E]](
    val from: internal.Obj,
    val field: api.FieldAccess[C[E]],
    val index: E)
    extends AbstractEdge {
  
  override def getFrom = new SetNode(from, field)
  override def getTo = field.t.asInstanceOf[SingleArgumentType[_,_]].base match {
    case u: internal.Pool[t] => if (index != null) new SkillObjectNode(index.asInstanceOf[internal.Obj]) else new SetNullNode(from, field)
    case a: internal.AnyRefType => if (index != null) new SkillObjectNode(index.asInstanceOf[internal.Obj]) else new SetNullNode(from, field)
    case _: ArrayType[_] | _: ListType[_] | _:SetType[_] | _: MapType[_,_] =>
      throw new Exception("Nested container.")
    case I8| I16| I32| I64| V64| F32| F64| _:internal.StringPool| Bool =>
      new SetValueNode(from, field, index)
  }
  override def textLabel(file: File) = ""
  
  // TODO do collection members share the ideal direction?
  override def idealDirection(file: File) = file.fieldPreferences(field).prefEdgeDirection()
}

/** edge from a map node to the map member */
case class MapMemberEdge[K, V, C[K,V] <: HashMap[K,V]](
    val from: internal.Obj,
    val field: api.FieldAccess[C[K,V]],
    val index: Seq[Any])
    extends AbstractEdge {
  
  private def fieldType = field.t.asInstanceOf[MapType[K,V]]
  private def to = FlattenedMap.get(field.get(from), fieldType, index) 
  private def valuebase = FlattenedMap.typeList(fieldType).last

  override def getFrom = new MapNode(from, field)
  override def getTo = valuebase match {
    case u: internal.Pool[t] => if (to != null) new SkillObjectNode(to.asInstanceOf[internal.Obj]) else new MapNullNode(from, field, index)
    case a: internal.AnyRefType => if (to != null) new SkillObjectNode(to.asInstanceOf[internal.Obj]) else new MapNullNode(from, field, index)
    case _: ArrayType[_] | _: ListType[_] | _:SetType[_] | _: MapType[_,_] =>
      throw new Exception("Nested container.")
    case I8| I16| I32| I64| V64| F32| F64| _:internal.StringPool| Bool =>
      new MapValueNode(from, field, index)
  }
  override def textLabel(file: File) = index.map {
    case o: internal.Obj => file.idOfObj(o)
    case x => x
         }.mkString(", ")
  
  // TODO do collection members share the ideal direction?
  override def idealDirection(file: File) = file.fieldPreferences(field).prefEdgeDirection()
}