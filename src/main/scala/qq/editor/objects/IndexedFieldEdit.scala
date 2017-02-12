package qq.editor.objects

import de.ust.skill.common.scala.api
import de.ust.skill.common.scala.internal.fieldTypes._
import scala.collection.mutable.Buffer

class IndexedFieldEdit[E, F[E] <: Buffer[E], O <: api.SkillObject](
  val page: ObjectPage,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[F[E]],
  val index: Int)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  field.t.asInstanceOf[SingleBaseTypeContainer[F[E],E]].groundType.asInstanceOf[FieldType[_]] match {
    case _: AnnotationType
      | _: UserType[_] ⇒
      val p = new qq.editor.binding.IndexedContainerField(null, page.file, pool, obj, field, index)
      val ed = new qq.util.binding.LabeledEdit(
        new qq.util.binding.TextEdit(p.asInstanceOf[qq.util.binding.Property[api.SkillObject]],
          page.file.objOfId(_),
          (x:api.SkillObject) => page.file.idOfObj(x)))
      val en = new qq.util.ExpandableNode(ed) {
        lazySubPart = { x ⇒ new ObjectEdit(page, p.asInstanceOf[qq.util.binding.Property[api.SkillObject]]()) }
      }
      p.onChange.strong += (_ ⇒ en.collapse())
      contents += en
    case _: ListType[_]
      | _: VariableLengthArray[_]
      | _: SetType[_]
      | _: ConstantLengthArray[_]
      | _: MapType[_, _] ⇒
      throw new Exception("$obj . $field ($index) is container of container")
    case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) =>
      throw new Exception("$obj . $field ($index) is const in container")
    case I8 | I16 | I32 | I64 | V64 | F32 | F64 | BoolType | _: StringType ⇒
      val p = new qq.editor.binding.IndexedContainerField(null, page.file, pool, obj, field, index)
      val ed = new qq.util.binding.LabeledEdit(p.defaultEditor)
      contents += new qq.util.ExpandableNode(ed)
  }

}