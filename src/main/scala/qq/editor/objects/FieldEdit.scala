package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import de.ust.skill.common.scala.internal.fieldTypes;

class FieldEdit[F, O <: api.SkillObject](
  val page: ObjectPage,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[F])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  field.t.asInstanceOf[fieldTypes.FieldType[_]] match {
    case _: fieldTypes.AnnotationType
      |      _: fieldTypes.UserType[_] ⇒
      val p = new qq.editor.binding.SimpleField(null, page.file, pool, obj, field)
      val ed = new qq.util.binding.LabeledEdit(
        new qq.util.binding.TextEdit(p.asInstanceOf[qq.util.binding.Property[api.SkillObject]],
          (x: String) ⇒ if (x == "") null else page.file.objOfId(x),
          (x: api.SkillObject) ⇒ if (x == null) "" else x.prettyString))
      val en = new qq.util.ExpandableNode(ed) {
        lazySubPart = { x ⇒ new ObjectEdit(page, p.asInstanceOf[qq.util.binding.Property[api.SkillObject]]()) }
      }
      p.onChange.strong += (_ ⇒ en.collapse())
      contents += en
    case _: fieldTypes.ListType[_] =>
        contents += new IndexedContainerEdit(page, pool, obj, field.asInstanceOf[api.FieldDeclaration[scala.collection.mutable.ArrayBuffer[_]]])
    case _: fieldTypes.VariableLengthArray[_] ⇒
        contents += new IndexedContainerEdit(page, pool, obj, field.asInstanceOf[api.FieldDeclaration[scala.collection.mutable.ArrayBuffer[_]]])
     case c: fieldTypes.SetType[_]             ⇒ contents += new swing.Label("Todo")
   case _: fieldTypes.ConstantLengthArray[_] ⇒ contents += new swing.Label("Todo")
    case m: fieldTypes.MapType[_, _]          ⇒ contents += new swing.Label("Todo")
    /* constants are only shown in the type; they're stored there, anyway */
    case fieldTypes.ConstantI8(_)             ⇒ ()
    case fieldTypes.ConstantI16(_)            ⇒ ()
    case fieldTypes.ConstantI32(_)            ⇒ ()
    case fieldTypes.ConstantI64(_)            ⇒ ()
    case fieldTypes.ConstantV64(_)            ⇒ ()
    case _ ⇒
      val p = new qq.editor.binding.SimpleField(null, page.file, pool, obj, field)
      val ed = new qq.util.binding.LabeledEdit(p.defaultEditor)
      contents += new qq.util.ExpandableNode(ed)
  }

}