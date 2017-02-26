package qq.editor.binding

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes.SingleBaseTypeContainer;
import de.ust.skill.common.scala.internal.fieldTypes.FieldType;
import scala.collection.mutable.HashMap;

class MapContainerField[O <: api.SkillObject, K, V, C[K,V] <: HashMap[K,V], G](
  owner0: qq.util.binding.PropertyOwner,
  val file: qq.editor.File,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[C[K,V]],
  val index: Seq[Any], 
  val groundType: api.FieldType[G],
  val initValue: G)
    extends qq.util.binding.Property[G](owner0, index.toString(), initValue) {

  description = s"element $index of ${field.name} in ${file.idOfObj(obj)}"  
  // TODO restrictions ++= Restrictions(field)
  restrictions ++= Restrictions(file, groundType)

  /**
   * when obj.get(field)(index) is the last element and is removed, this object
   *  disables itself so that it can do no harm. Its owner will remove it.
   */
  private var disabled = false

  private val fileEditHandler: (qq.editor.Edit[_] ⇒ Unit) = { x ⇒
    if (!disabled && x.obj == obj && x.isInstanceOf[qq.editor.MapEdit[_, _]]) {
      val y = x.asInstanceOf[qq.editor.MapEdit[O, C[K,V]]]
      if (y.field == field) {
        y match {
          case ins: qq.editor.MapInsert[O, C[K,V]] ⇒
            // ignore
          case del: qq.editor.MapRemove[O, C[K,V]] ⇒
            if (del.index == index) {
                disabled = true
            }
          case mod: qq.editor.MapModify[O, C[K,V]] ⇒
            if (mod.index == index) {
              this.assignUnchecked(mod.newValue.asInstanceOf[G])
            }
        }
      }
    }
  }
  file.onEdit.weak += fileEditHandler

  private def selfChangeHandler(x: G): Unit = {
    new qq.editor.UserMapModify(file, pool, obj, field, index, x)
  }
  onChange.strong += selfChangeHandler

}

object MapContainerField {
  /** construct map field according to the ground type of the value */
  def apply[O <: api.SkillObject, K, V, C[K,V] <: HashMap[K,V], G](
    owner0: qq.util.binding.PropertyOwner,
    file: qq.editor.File,
    pool: api.Access[O],
    obj: O,
    field: api.FieldDeclaration[C[K,V]],
    index: Seq[Any], 
    groundType: api.FieldType[G]): MapContainerField[O, K, V, C, G]=  {
    
    import scala.collection.mutable.HashMap
    import de.ust.skill.common.scala.internal.fieldTypes._
    import qq.util.FlattenedMap.get
    
    val initValue = get(obj.get(field),
        field.t.asInstanceOf[MapType[K,V]], index).asInstanceOf[G]
    new MapContainerField(owner0, file, pool, obj, field, index, groundType, initValue)
    
  }
  
}
