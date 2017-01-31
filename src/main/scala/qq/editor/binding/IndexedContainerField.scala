package qq.editor.binding

import de.ust.skill.common.scala.api;

class IndexedContainerField[O <: api.SkillObject, C <: scala.collection.mutable.ArrayBuffer[F], F](
  owner0: qq.util.binding.PropertyOwner,
  val file: qq.editor.File,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[C],
  val index: Int)
    extends qq.util.binding.Property[F](owner0, field.name, obj.get(field)(index)) {

  /* TODO restrictions */

  /**
   * when obj.get(field)(index) is the last element and is removed, this object
   *  disables itself so that it can do no harm. Its owner will remove it.
   */
  private var disabled = false

  private val fileEditHandler: (qq.editor.Edit[_] ⇒ Unit) = { x ⇒
    if (!disabled && x.obj == obj && x.isInstanceOf[qq.editor.IndexedContainerEdit[_, _, _]]) {
      val y = x.asInstanceOf[qq.editor.IndexedContainerEdit[O, C, F]]
      if (y.field == field) {
        y match {
          case ins: qq.editor.IndexedContainerInsert[O, C, F] ⇒
            if (ins.index == index) {
              this.assignUnchecked(ins.value)
            } else if (ins.index < index) {
              this.assignUnchecked(obj.get(field)(index))
            }
          case del: qq.editor.IndexedContainerRemove[O, C, F] ⇒
            if (del.index <= index) {
              if (index >= obj.get(field).size) {
                disabled = true
              } else {
                this.assignUnchecked(obj.get(field)(index))
              }
            }
          case mod: qq.editor.IndexedContainerModify[O, C, F] ⇒
            if (mod.index == index) {
              this.assignUnchecked(mod.newValue)
            }
        }
      }
    }
  }
  file.onEdit.weak += fileEditHandler

  private def selfChangeHandler(x: F): Unit = {
    new qq.editor.UserIndexedContainerModify(file, pool, obj, field, index, x)
  }
  onChange.strong += selfChangeHandler

}