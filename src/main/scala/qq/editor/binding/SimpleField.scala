package qq.editor.binding

import de.ust.skill.common.scala.api;

class SimpleField[O <: api.SkillObject, F](
    owner0: qq.util.binding.PropertyOwner,
    val file: qq.editor.File,
    val pool: api.Access[O],
    val obj: O,
    val field: api.FieldDeclaration[F])
    extends qq.util.binding.Property[F](owner0, field.name + " of " + obj, obj.get(field))  {

  
  /* TODO restrictions */

  def onEdit(x: qq.editor.Edit[_]): Unit = {
    if (x.obj == obj && x.isInstanceOf[qq.editor.SimpleFieldEdit[_, _]]) {
      val y = x.asInstanceOf[qq.editor.SimpleFieldEdit[O, F]]
      if (y.field == field) {
    println("onEdit" + y.newValue + " ("+ y.oldValue + ")")
    
        this := y.newValue
      }
    }
  }
  file.onEdit.weak += onEdit
  
  def onSelfChange(x: F): Unit = {
    println("onSelfChange" + x)
    new qq.editor.UserSimpleFieldEdit(file, pool, obj, field, x)
  }
  onChange.strong += onSelfChange
  
}