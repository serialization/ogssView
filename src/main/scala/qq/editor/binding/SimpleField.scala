package qq.editor.binding

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;

class SimpleField[O <: api.SkillObject, F](
  owner0: qq.util.binding.PropertyOwner,
  val file: qq.editor.File,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[F])
    extends qq.util.binding.Property[F](owner0, field.name, obj.get(field)) {

  description = s"${field.name} in ${file.idOfObj(obj)}"

  restrictions ++= Restrictions(field)

  if (field.t.isInstanceOf[internal.fieldTypes.UserType[_]]) {
    restrictions += qq.util.binding.Restriction(
      { (x: F) ⇒
        x == null || {
          val t = file.s(x.asInstanceOf[api.SkillObject].getTypeName)
          t == field.t || file.superTypes(t).contains(field.t)
        }
      },
      "object must have type " + field.t + " or sub-type")
  }
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