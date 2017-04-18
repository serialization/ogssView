package qq.graph

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes._;
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

case class SkillObjectNode(val skillObject: api.SkillObject)
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = UIElements.skillObject(graph, this, skillObject)
  override def getOutEdge(file: qq.editor.File) = {
    val τ = file.s(skillObject.getTypeName)
    for (
      f ← τ.allFields if !file.fieldPreferences(f).isDeleted && file.fieldPreferences(f).visibilityIn(skillObject).showAsNode
    ) yield new SkillFieldEdge(skillObject, f)
  }
  def edgeForField(file: qq.editor.File, field: api.FieldDeclaration[_]): Option[AbstractEdge] = {
    val τ = file.s(skillObject.getTypeName)
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

case class ValueNode[T](val skillObject: api.SkillObject, val field: api.FieldDeclaration[T])
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = {
    UIElements.value(graph, this, skillObject.get(field))
  }
  override def getOutEdge(file: qq.editor.File) = Iterator() // values are leaves
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: ValueNode[T] ⇒ that.skillObject == skillObject && that.field == field
    case _                  ⇒ false
  }
  override def name(graph: Graph) = UIElements.valueShortString(skillObject.get(field))

}
case class NullNode[T](val skillObject: api.SkillObject, val field: api.FieldDeclaration[T])
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
case class ListNode[E, C[E] <: Buffer[E]](val skillObject: api.SkillObject, val field: api.FieldDeclaration[C[E]])
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = {
    UIElements.list(graph, this, skillObject, field)
  }
  override def getOutEdge(file: qq.editor.File) = {
    if (skillObject.get(field).size <= qq.editor.Main.preferences.graphCollectionSmall()) {
      skillObject.get(field).indices.iterator.map(i ⇒ new ListMemberEdge(skillObject, field, i))
    } else {
      Iterator()
    }
  }
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: ListNode[E, C] ⇒ that.skillObject == skillObject && that.field == field
    case _                    ⇒ false
  }
  override def name(graph: Graph) = (if (field.t.isInstanceOf[ListType[_]]) "list of " else "array of ") + skillObject.get(field).size
}
case class ListValueNode[E, C[E] <: Buffer[E]](val skillObject: api.SkillObject, val field: api.FieldDeclaration[C[E]], val index: Int)
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = {
    UIElements.value(graph, this, skillObject.get(field)(index))
  }
  override def getOutEdge(file: qq.editor.File) = Iterator() // values are leaves
  override def hashCode = java.util.Objects.hash(skillObject, field) ^ (index * 33)
  override def equals(that: Any) = that match {
    case that: ListValueNode[E, C] ⇒ that.skillObject == skillObject && that.field == field && that.index == index
    case _                         ⇒ false
  }
  override def name(graph: Graph) = UIElements.valueShortString(skillObject.get(field)(index))
}
case class ListNullNode[E, C[E] <: Buffer[E]](val skillObject: api.SkillObject, val field: api.FieldDeclaration[C[E]], val index: Int)
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
case class SetNode[E, C[E] <: HashSet[E]](val skillObject: api.SkillObject, val field: api.FieldDeclaration[C[E]])
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = {
    UIElements.set(graph, this, skillObject, field)
  }
  override def getOutEdge(file: qq.editor.File) = {
    if (skillObject.get(field).size <= qq.editor.Main.preferences.graphCollectionSmall()) {
      skillObject.get(field).iterator.map(e ⇒ new SetMemberEdge(skillObject, field, e))
    } else {
      Iterator()
    }
  }
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: SetNode[E, C] ⇒ that.skillObject == skillObject && that.field == field
    case _                   ⇒ false
  }
  override def name(graph: Graph) = "set of " + skillObject.get(field).size
}
case class SetValueNode[E, C[E] <: HashSet[E]](val skillObject: api.SkillObject, val field: api.FieldDeclaration[C[E]], val element: E)
    extends AbstractNode() {

  override def getUiElement(graph: Graph) = {
    UIElements.value(graph, this, element)
  }
  override def getOutEdge(file: qq.editor.File) = Iterator() // values are leaves
  override def hashCode = java.util.Objects.hash(skillObject, field) ^ (if (element == null) 0 else element.hashCode())
  override def equals(that: Any) = that match {
    case that: SetValueNode[E, C] ⇒ that.skillObject == skillObject && that.field == field && !qq.util.Neq(that.element, element)
    case _                        ⇒ false
  }
  override def name(graph: Graph) = UIElements.valueShortString(element)
}
// at most one null per set
case class SetNullNode[E, C[E] <: HashSet[E]](val skillObject: api.SkillObject, val field: api.FieldDeclaration[C[E]])
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
case class MapNode[K, V, C[K, V] <: HashMap[K, V]](val skillObject: api.SkillObject, val field: api.FieldDeclaration[C[K, V]])
    extends AbstractNode() {

  private def fieldType = field.t.asInstanceOf[MapType[K, V]]
  override def getUiElement(graph: Graph) = {
    UIElements.map(graph, this, skillObject, field)
  }
  override def getOutEdge(file: qq.editor.File) = {
    if (FlattenedMap.size(skillObject.get(field), fieldType) <= qq.editor.Main.preferences.graphCollectionSmall()) {
      FlattenedMap.keys(skillObject.get(field), fieldType).iterator.map(e ⇒ new MapMemberEdge(skillObject, field, e))
    } else {
      Iterator()
    }
  }
  override def hashCode = java.util.Objects.hash(skillObject, field)
  override def equals(that: Any) = that match {
    case that: MapNode[K, V, C] ⇒ that.skillObject == skillObject && that.field == field
    case _                      ⇒ false
  }
  override def name(g: Graph) = "map of "+FlattenedMap.size(skillObject.get(field),field.t.asInstanceOf[MapType[K,V]])
}
case class MapValueNode[K, V, C[K, V] <: HashMap[K, V]](val skillObject: api.SkillObject, val field: api.FieldDeclaration[C[K, V]], val index: Seq[Any])
    extends AbstractNode() {

  private def fieldType = field.t.asInstanceOf[MapType[K, V]]
  override def getUiElement(graph: Graph) = {
    UIElements.value(graph, this, FlattenedMap.get(skillObject.get(field), fieldType, index))
  }
  override def getOutEdge(file: qq.editor.File) = Iterator() // values are leaves
  override def hashCode = java.util.Objects.hash(skillObject, field, index)
  override def equals(that: Any) = that match {
    case that: MapValueNode[K, V, C] ⇒ that.skillObject == skillObject && that.field == field && that.index == index
    case _                           ⇒ false
  }
  override def name(graph: Graph) = UIElements.valueShortString(FlattenedMap.get(skillObject.get(field), fieldType, index))
}
case class MapNullNode[K, V, C[K, V] <: HashMap[K, V]](val skillObject: api.SkillObject, val field: api.FieldDeclaration[C[K, V]], val index: Seq[Any])
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