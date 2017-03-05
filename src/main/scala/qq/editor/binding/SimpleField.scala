package qq.editor.binding

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;

class SimpleField[O <: api.SkillObject, F](
  owner0: qq.util.binding.PropertyOwner,
  val file: qq.editor.File,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[F])
    extends SkillFieldProperty[F](owner0, field.name, obj.get(field)) {

  def groundType = field.t

  description = s"${field.name} in ${file.idOfObj(obj)}"

  restrictions ++= Restrictions(field)
  restrictions ++= Restrictions(file, field.t)

  private val fileEditHandler: (qq.editor.Edit[_] ⇒ Unit) = { x ⇒
    if (x.obj == obj && x.isInstanceOf[qq.editor.SimpleFieldEdit[_, _]]) {
      val y = x.asInstanceOf[qq.editor.SimpleFieldEdit[O, F]]
      if (y.field == field) {
        this.assignUnchecked(y.newValue)
      }
    }
  }
  file.onEdit.weak += fileEditHandler

  private def selfChangeHandler(x: F): Unit = {
    new qq.editor.UserSimpleFieldEdit(file, pool, obj, field, x)
  }
  onChange.strong += selfChangeHandler

}