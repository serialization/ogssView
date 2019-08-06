package qq.graph

import ogss.common.scala.api;
import ogss.common.scala.internal;
import ogss.common.scala.internal.fieldTypes._;
import scala.collection.mutable.Buffer;
import scala.collection.mutable.HashSet;
import scala.collection.mutable.HashMap;
import qq.util.FlattenedMap;

/** the thing a node in the graph represents */
abstract class AbstractNode() {
  /** how it should display */
  def getUiElement(graph: Graph): swing.UIElement
  /** outgoing edges */
  def getOutEdge(file: qq.editor.File): Iterator[AbstractEdge]
  /** name in graph */
  def name(graph: Graph): String
}

/** A node representing a skill object */
case class SkillObjectNode(val skillObject: internal.Obj)
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = UIElements.skillObject(graph, this, skillObject)
  override def getOutEdge(file: qq.editor.File) = {
    val τ = file.s.pool(skillObject)
    for (
      f ← τ.allFields if !file.fieldPreferences(f).isDeleted && file.fieldPreferences(f).visibilityIn(skillObject).showAsNode
    ) yield new SkillFieldEdge(skillObject, f)
  }
  def edgeForField(file: qq.editor.File, field: api.FieldAccess[_]): Option[AbstractEdge] = {
    val τ = file.s.pool(skillObject)
    if (τ.allFields.contains(field)) { // only has edges for own fields
      if (file.fieldPreferences(field).visibilityIn(skillObject).showAsNode) {
        Some(new SkillFieldEdge(skillObject, field))
      } else {
        None
      }
    } else {
      None
    }
  }
  override def hashCode = skillObject.hashCode()
  override def equals(that: Any) = that match {
    case that: SkillObjectNode ⇒ that.skillObject == skillObject
    case _                     ⇒ false
  }
  override def name(graph: Graph) = graph.file.idOfObj(skillObject)
}

/** A node representing a value of a primitive type */
case class ValueNode[T](val skillObject: internal.Obj, val field: api.FieldAccess[T])
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = {
    UIElements.value(graph, this, field.get(skillObject))
  }
  override def getOutEdge(file: qq.editor.File) = Iterator() // values are leaves
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: ValueNode[T] ⇒ that.skillObject == skillObject && that.field == field
    case _                  ⇒ false
  }
  override def name(graph: Graph) = UIElements.valueShortString(field.get(skillObject))

}
/** A node representing null */
case class NullNode[T](val skillObject: internal.Obj, val field: api.FieldAccess[T])
    extends AbstractNode() {
  override def getUiElement(graph: Graph) = UIElements.nil(graph)
  override def getOutEdge(file: qq.editor.File) = Iterator() // nulls are leaves

  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: ValueNode[T] ⇒ that.skillObject == skillObject && that.field == field
    case _                  ⇒ false
  }
  override def name(graph: Graph) = "⊥"
}
/** A node for a list field */
case class ListNode[E, C[E] <: Buffer[E]](val skillObject: internal.Obj, val field: api.FieldAccess[C[E]])
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = {
    UIElements.list(graph, this, skillObject, field)
  }
  override def getOutEdge(file: qq.editor.File) = {
    if (field.get(skillObject).size <= qq.editor.Main.preferences.graphCollectionSmall()) {
      field.get(skillObject).indices.iterator.map(i ⇒ new ListMemberEdge(skillObject, field, i))
    } else {
      Iterator()
    }
  }
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: ListNode[E, C] ⇒ that.skillObject == skillObject && that.field == field
    case _                    ⇒ false
  }
  override def name(graph: Graph) = (if (field.t.isInstanceOf[ListType[_]]) "list of " else "array of ") + field.get(skillObject).size
}
/** A node for a primitive type value as list element */
case class ListValueNode[E, C[E] <: Buffer[E]](val skillObject: internal.Obj, val field: api.FieldAccess[C[E]], val index: Int)
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = {
    UIElements.value(graph, this, field.get(skillObject)(index))
  }
  override def getOutEdge(file: qq.editor.File) = Iterator() // values are leaves
  override def hashCode = java.util.Objects.hash(skillObject, field) ^ (index * 33)
  override def equals(that: Any) = that match {
    case that: ListValueNode[E, C] ⇒ that.skillObject == skillObject && that.field == field && that.index == index
    case _                         ⇒ false
  }
  override def name(graph: Graph) = UIElements.valueShortString(field.get(skillObject)(index))
}
/** Null as list element*/
case class ListNullNode[E, C[E] <: Buffer[E]](val skillObject: internal.Obj, val field: api.FieldAccess[C[E]], val index: Int)
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = UIElements.nil(graph)
  override def getOutEdge(file: qq.editor.File) = Iterator() // nulls are leaves
  override def hashCode = java.util.Objects.hash(skillObject, field) ^ (index * 33)
  override def equals(that: Any) = that match {
    case that: ListNullNode[E, C] ⇒ that.skillObject == skillObject && that.field == field && that.index == index
    case _                        ⇒ false
  }
  override def name(graph: Graph) = "⊥"
}
/** Node for the set field */
case class SetNode[E, C[E] <: HashSet[E]](val skillObject: internal.Obj, val field: api.FieldAccess[C[E]])
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = {
    UIElements.set(graph, this, skillObject, field)
  }
  override def getOutEdge(file: qq.editor.File) = {
    if (field.get(skillObject).size <= qq.editor.Main.preferences.graphCollectionSmall()) {
      field.get(skillObject).iterator.map(e ⇒ new SetMemberEdge(skillObject, field, e))
    } else {
      Iterator()
    }
  }
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: SetNode[E, C] ⇒ that.skillObject == skillObject && that.field == field
    case _                   ⇒ false
  }
  override def name(graph: Graph) = "set of " + field.get(skillObject).size
}
/** Primitive value element of a set */
case class SetValueNode[E, C[E] <: HashSet[E]](val skillObject: internal.Obj, val field: api.FieldAccess[C[E]], val element: E)
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = {
    UIElements.value(graph, this, element)
  }
  override def getOutEdge(file: qq.editor.File) = Iterator() // values are leaves
  override def hashCode = java.util.Objects.hash(skillObject, field) ^ (if (element == null) 0 else element.hashCode())
  override def equals(that: Any) = that match {
    case that: SetValueNode[E, C] ⇒ that.skillObject == skillObject && that.field == field && ne(that.element, element)
    case _                        ⇒ false
  }
  override def name(graph: Graph) = UIElements.valueShortString(element)
}
// at most one null per set
/** Null in a set */
case class SetNullNode[E, C[E] <: HashSet[E]](val skillObject: internal.Obj, val field: api.FieldAccess[C[E]])
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = UIElements.nil(graph)
  override def getOutEdge(file: qq.editor.File) = Iterator() // nulls are leaves
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: SetNullNode[E, C] ⇒ that.skillObject == skillObject && that.field == field
    case _                       ⇒ false
  }
  override def name(graph: Graph) = "⊥"
}
/** Map field */
case class MapNode[K, V, C[K, V] <: HashMap[K, V]](val skillObject: internal.Obj, val field: api.FieldAccess[C[K, V]])
    extends AbstractNode() {

  private def fieldType = field.t.asInstanceOf[MapType[K, V]]
  override def getUiElement(graph: Graph) = {
    UIElements.map(graph, this, skillObject, field)
  }
  override def getOutEdge(file: qq.editor.File) = {
    if (FlattenedMap.size(field.get(skillObject), fieldType) <= qq.editor.Main.preferences.graphCollectionSmall()) {
      FlattenedMap.keys(field.get(skillObject), fieldType).iterator.map(e ⇒ new MapMemberEdge(skillObject, field, e))
    } else {
      Iterator()
    }
  }
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: MapNode[K, V, C] ⇒ that.skillObject == skillObject && that.field == field
    case _                      ⇒ false
  }
  override def name(g: Graph) = "map of "+FlattenedMap.size(field.get(skillObject),field.t.asInstanceOf[MapType[K,V]])
}
/** Primitie value in a map */
case class MapValueNode[K, V, C[K, V] <: HashMap[K, V]](val skillObject: internal.Obj, val field: api.FieldAccess[C[K, V]], val index: Seq[Any])
    extends AbstractNode() {

  private def fieldType = field.t.asInstanceOf[MapType[K, V]]
  override def getUiElement(graph: Graph) = {
    UIElements.value(graph, this, FlattenedMap.get(field.get(skillObject), fieldType, index))
  }
  override def getOutEdge(file: qq.editor.File) = Iterator() // values are leaves
  override def hashCode = java.util.Objects.hash(skillObject, field, index)
  override def equals(that: Any) = that match {
    case that: MapValueNode[K, V, C] ⇒ that.skillObject == skillObject && that.field == field && that.index == index
    case _                           ⇒ false
  }
  override def name(graph: Graph) = UIElements.valueShortString(FlattenedMap.get(field.get(skillObject), fieldType, index))
}
/** Null in a map*/
case class MapNullNode[K, V, C[K, V] <: HashMap[K, V]](val skillObject: internal.Obj, val field: api.FieldAccess[C[K, V]], val index: Seq[Any])
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = UIElements.nil(graph)
  override def getOutEdge(file: qq.editor.File) = Iterator() // nulls are leaves
  override def hashCode = java.util.Objects.hash(skillObject, field, index)
  override def equals(that: Any) = that match {
    case that: MapNullNode[K, V, C] ⇒ that.skillObject == skillObject && that.field == field && that.index == index
    case _                          ⇒ false
  }
  override def name(graph: Graph) = "⊥"
}