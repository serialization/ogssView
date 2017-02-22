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

  val editField = typ match {
    case _: AnnotationType
      | _: UserType[_] ⇒
      new qq.util.binding.LabeledEdit(
        new qq.util.binding.TextEdit(fieldProperty.asInstanceOf[Property[api.SkillObject]],
          page.file.objOfId(_),
          (x: api.SkillObject) ⇒ page.file.idOfObj(x)))
    case _: ListType[_]
      | _: VariableLengthArray[_]
      | _: SetType[_]
      | _: ConstantLengthArray[_]
      | _: MapType[_, _] ⇒
      throw new Exception(s"required ground type, found container ${typ}")
    case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒
      throw new Exception(s"required ground type, found constannt ${typ}")
    case I8 | I16 | I32 | I64 | V64 | F32 | F64 | BoolType | _: StringType ⇒
      new qq.util.binding.LabeledEdit(fieldProperty.defaultEditor)
  }
  val en = typ match {
    case _: AnnotationType
      | _: UserType[_] ⇒
      new qq.util.ExpandableNode(editField) {
        lazySubPart = { x ⇒ new ObjectEdit(page, fieldProperty.asInstanceOf[Property[api.SkillObject]]()) }
        fieldProperty.onChange.strong += (_ ⇒ this.collapse())
      }

    case _: ListType[_]
      | _: VariableLengthArray[_]
      | _: SetType[_]
      | _: ConstantLengthArray[_]
      | _: MapType[_, _] ⇒
      throw new Exception(s"required ground type, found container ${typ}")
    case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒
      throw new Exception(s"required ground type, found constannt ${typ}")
    case I8 | I16 | I32 | I64 | V64 | F32 | F64 | BoolType | _: StringType ⇒
      new qq.util.ExpandableNode(editField)
  }
  contents += en

}