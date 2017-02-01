package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import de.ust.skill.common.scala.internal.fieldTypes._;
import scala.collection.mutable.Buffer

class FieldEdit[F, O <: api.SkillObject](
  val page: ObjectPage,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[F])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  field.t.asInstanceOf[FieldType[_]] match {
    case _: AnnotationType
      | _: UserType[_] ⇒
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
    case c: ListType[f] ⇒
      contents += new IndexedContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[Buffer[f]]],
        () ⇒ NewValue.default(c.groundType))
    case c: VariableLengthArray[f] ⇒
      contents += new IndexedContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[Buffer[f]]],
        () ⇒ NewValue.default(c.groundType))
    case c: SetType[_] ⇒ contents += new swing.Label("Todo")
    case c: ConstantLengthArray[f] ⇒
      contents += new IndexedContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[Buffer[f]]],
        canResize = false)
    case m: MapType[_, _] ⇒ contents += new swing.Label("Todo")
    /* constants are only shown in the type; they're stored there, anyway */
    case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_)
      | ConstantV64(_) ⇒
      ()
    case BoolType | _: StringType | I8 | I16 | I32 | I64 | V64 | F32 | F64 ⇒
      val p = new qq.editor.binding.SimpleField(null, page.file, pool, obj, field)
      val ed = new qq.util.binding.LabeledEdit(p.defaultEditor)
      contents += new qq.util.ExpandableNode(ed)
  }

}