package qq.editor.binding

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes.SingleBaseTypeContainer;
import de.ust.skill.common.scala.internal.fieldTypes.FieldType;
import scala.collection.mutable.Buffer;

class MapField[O <: api.SkillObject, F, G](
  owner0: qq.util.binding.PropertyOwner,
  val file: qq.editor.File,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[F],
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
      val y = x.asInstanceOf[qq.editor.MapEdit[O, F]]
      if (y.field == field) {
        y match {
          case ins: qq.editor.MapInsert[O, F] ⇒
            // ignore
          case del: qq.editor.MapRemove[O, F] ⇒
            if (del.index == index) {
                disabled = true
            }
          case mod: qq.editor.MapModify[O, F] ⇒
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

object MapField {
  /** construct map field according to the ground type of the value */
  def apply[O <: api.SkillObject, F, G](
    owner0: qq.util.binding.PropertyOwner,
    file: qq.editor.File,
    pool: api.Access[O],
    obj: O,
    field: api.FieldDeclaration[F],
    index: Seq[Any], 
    groundType: api.FieldType[G]): MapField[O, F, G]=  {
    
    import scala.collection.mutable.HashMap
    import de.ust.skill.common.scala.internal.fieldTypes._
    import qq.editor.objects.MapEdit.get
    
    val initValue = get(obj.get(field).asInstanceOf[HashMap[Any,Any]],
        field.t.asInstanceOf[MapType[_,_]], index).asInstanceOf[G]
    new MapField(owner0, file, pool, obj, field, index, groundType, initValue)
    
  }
  
}
