package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import de.ust.skill.common.scala.internal.fieldTypes;

class ObjectEdit[P <: api.SkillObject](
  val page: ObjectPage,
  val obj: P)
    extends swing.BoxPanel(swing.Orientation.Vertical)
    with qq.util.binding.PropertyOwner {

  val pool: api.Access[P] = page.file.s(obj.getTypeName).asInstanceOf[api.Access[P]]

 
  pool.allFields.foreach { f ⇒ contents += new FieldEdit(page, pool, obj, f)}

}