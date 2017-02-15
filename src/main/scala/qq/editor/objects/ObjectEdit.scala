package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import de.ust.skill.common.scala.internal.fieldTypes;

/** Edit the fields of \c obj. Can be used as the expandable pane below a reference field */
class ObjectEdit[P <: api.SkillObject](
  val page: ObjectPage,
  val obj: P)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val pool: api.Access[P] = page.file.s(obj.getTypeName).asInstanceOf[api.Access[P]]

  contents ++= pool.allFields.map { f ⇒ new FieldEdit(page, pool, obj, f) }

}
/** Top level version of ObjectEdit, comes with scroll bars  */
class TopObjectEdit[P <: api.SkillObject](
  val page: ObjectPage,
  val obj: P)
    extends qq.util.VScrollPane {
  contents = qq.util.Swing.VBox(new ObjectEdit(page, obj), swing.Swing.VGlue)
}