package qq.graph

import de.ust.skill.common.scala.api
import de.ust.skill.common.scala.internal.fieldTypes._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import qq.util.Vector
import qq.editor.File
import qq.util.FlattenedMap

/** the thing a link in the graph represents */
abstract class AbstractEdge {
  def getTo: AbstractNode
  def getFrom: AbstractNode
  def textLabel: String
  def idealDirection(file: File): Vector
}

case class SkillFieldEdge[T](
    val from: api.SkillObject,
    val field: api.FieldDeclaration[T])
    extends AbstractEdge {
  
  val to = from.get(field)
  
  override def hashCode = 17 + 31 * java.util.Objects.hash(from, field) + (if(to != null) to.hashCode() else 0)
  override def equals(that: Any) = that match {
    case that: SkillFieldEdge[T] =>
      from == that.from && field == that.field && to == that.to
    case _ => false
  }
  
  override def getFrom = new SkillObjectNode(from)
  override def getTo = field.t.asInstanceOf[FieldType[_]] match {
    case u: UserType[t] => if (to != null) new SkillObjectNode(to.asInstanceOf[t]) else new NullNode(from, field)
    case a: AnnotationType => if (to != null) new SkillObjectNode(to.asInstanceOf[api.SkillObject]) else new NullNode(from, field)
    case c: ConstantLengthArray[e] => new ListNode(from, field.asInstanceOf[api.FieldDeclaration[Buffer[e]]])
    case c: VariableLengthArray[e] => new ListNode(from, field.asInstanceOf[api.FieldDeclaration[Buffer[e]]])
    case c: ListType[e] => new ListNode(from, field.asInstanceOf[api.FieldDeclaration[Buffer[e]]])
    case c: SetType[e] => new SetNode(from, field.asInstanceOf[api.FieldDeclaration[HashSet[e]]])
    case c: MapType[k,v] => new MapNode(from, field.asInstanceOf[api.FieldDeclaration[HashMap[k,v]]])
    case I8| I16| I32| I64| V64| F32| F64| _:StringType| BoolType =>
      new ValueNode(from, field)
    case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) =>
      throw new Exception("Constant values are not path of the graph, this edge should have never been created.")
  }
  override def textLabel = field.name
  
  override def idealDirection(file: File) = file.fieldSettings(field).prefEdgeDirection()
  
 }
  

case class ListMemberEdge[E, C[E] <: Buffer[E]](
    val from: api.SkillObject,
    val field: api.FieldDeclaration[C[E]],
    val index: Int)
    extends AbstractEdge {
  
  override def getFrom = new ListNode(from, field)
  override def getTo = field.t.asInstanceOf[SingleBaseTypeContainer[_,_]].groundType match {
    case u: UserType[t] => if (from.get(field)(index) != null) new SkillObjectNode(from.get(field)(index).asInstanceOf[t]) else new ListNullNode(from, field, index)
    case a: AnnotationType => if (from.get(field)(index) != null) new SkillObjectNode(from.get(field)(index).asInstanceOf[api.SkillObject]) else new ListNullNode(from, field, index)
    case _: ConstantLengthArray[_] | _: VariableLengthArray[_] | _: ListType[_] | _:SetType[_] | _: MapType[_,_] =>
      throw new Exception("Nested container.")
    case I8| I16| I32| I64| V64| F32| F64| _:StringType| BoolType =>
      new ListValueNode(from, field, index)
    case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) =>
      throw new Exception("Constant values are not path of the graph, this edge should have never been created.")
  }
    
    
  override def textLabel = index.toString()
  
  // TODO do collection members share the ideal direction?
  override def idealDirection(file: File) = file.fieldSettings(field).prefEdgeDirection()
}

case class SetMemberEdge[E, C[E] <: HashSet[E]](
    val from: api.SkillObject,
    val field: api.FieldDeclaration[C[E]],
    val index: E)
    extends AbstractEdge {
  
  override def getFrom = new SetNode(from, field)
  override def getTo = field.t.asInstanceOf[SingleBaseTypeContainer[_,_]].groundType match {
    case u: UserType[t] => if (index != null) new SkillObjectNode(index.asInstanceOf[api.SkillObject]) else new SetNullNode(from, field)
    case a: AnnotationType => if (index != null) new SkillObjectNode(index.asInstanceOf[api.SkillObject]) else new SetNullNode(from, field)
    case _: ConstantLengthArray[_] | _: VariableLengthArray[_] | _: ListType[_] | _:SetType[_] | _: MapType[_,_] =>
      throw new Exception("Nested container.")
    case I8| I16| I32| I64| V64| F32| F64| _:StringType| BoolType =>
      new SetValueNode(from, field, index)
    case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) =>
      throw new Exception("Constant values are not path of the graph, this edge should have never been created.")
  }
  override def textLabel = ""
  
  // TODO do collection members share the ideal direction?
  override def idealDirection(file: File) = file.fieldSettings(field).prefEdgeDirection()
}

case class MapMemberEdge[K, V, C[K,V] <: HashMap[K,V]](
    val from: api.SkillObject,
    val field: api.FieldDeclaration[C[K,V]],
    val index: Seq[Any])
    extends AbstractEdge {
  
  private def fieldType = field.t.asInstanceOf[MapType[K,V]]
  private def to = FlattenedMap.get(from.get(field), fieldType, index) 
  private def valueGroundType = FlattenedMap.typeList(fieldType).last

  override def getFrom = new MapNode(from, field)
  override def getTo = valueGroundType match {
    case u: UserType[t] => if (to != null) new SkillObjectNode(to.asInstanceOf[api.SkillObject]) else new MapNullNode(from, field, index)
    case a: AnnotationType => if (to != null) new SkillObjectNode(to.asInstanceOf[api.SkillObject]) else new MapNullNode(from, field, index)
    case _: ConstantLengthArray[_] | _: VariableLengthArray[_] | _: ListType[_] | _:SetType[_] | _: MapType[_,_] =>
      throw new Exception("Nested container.")
    case I8| I16| I32| I64| V64| F32| F64| _:StringType| BoolType =>
      new MapValueNode(from, field, index)
    case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) =>
      throw new Exception("Constant values are not path of the graph, this edge should have never been created.")
  }
  override def textLabel = index.mkString(", ")
  
  // TODO do collection members share the ideal direction?
  override def idealDirection(file: File) = file.fieldSettings(field).prefEdgeDirection()
}