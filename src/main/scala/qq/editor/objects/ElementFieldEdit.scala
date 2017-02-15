package qq.editor.objects

import de.ust.skill.common.scala.api
import de.ust.skill.common.scala.internal.fieldTypes._
import scala.collection.mutable.Buffer
import qq.util.binding.Property

class ElementFieldEdit[E, O <: api.SkillObject](
  val page: ObjectPage,
  val typ: FieldType[_],
  val fieldProperty: Property[E])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  typ match {
    case _: AnnotationType
      | _: UserType[_] ⇒
      val ed = new qq.util.binding.LabeledEdit(
        new qq.util.binding.TextEdit(fieldProperty.asInstanceOf[Property[api.SkillObject]],
          page.file.objOfId(_),
          (x:api.SkillObject) => page.file.idOfObj(x)))
      val en = new qq.util.ExpandableNode(ed) {
        lazySubPart = { x ⇒ new ObjectEdit(page, fieldProperty.asInstanceOf[Property[api.SkillObject]]()) }
      }
      fieldProperty.onChange.strong += (_ ⇒ en.collapse())
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
      val ed = new qq.util.binding.LabeledEdit(fieldProperty.defaultEditor)
      contents += new qq.util.ExpandableNode(ed)
  }

}