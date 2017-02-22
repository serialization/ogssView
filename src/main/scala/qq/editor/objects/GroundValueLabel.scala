package qq.editor.objects

import de.ust.skill.common.scala.api
import de.ust.skill.common.scala.internal.fieldTypes._

/** display a value of ground type; references get a expandable pane that shows their referent */
class GroundValueLabel(
  val page: ObjectPage,
  val typ: api.FieldType[_],
  val value: Any)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val labelField = typ match {
    case _: AnnotationType
      | _: UserType[_] ⇒
      new swing.Label(page.file.idOfObj(value.asInstanceOf[api.SkillObject]))
    case _: ListType[_]
      | _: VariableLengthArray[_]
      | _: SetType[_]
      | _: ConstantLengthArray[_]
      | _: MapType[_, _] ⇒
      throw new Exception(s"required ground type, found container ${typ}")
    case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒
      throw new Exception(s"required ground type, found constant ${typ}")
    case I8 | I16 | I32 | I64 | V64 | F32 | F64 | BoolType | _: StringType ⇒
      if (value == null) {
        new swing.Label("(null)")
      } else {
        new swing.Label(value.toString)
      }
  }
  val en = new qq.util.ExpandableNode(labelField)

  typ match {
    case _: AnnotationType
      | _: UserType[_] ⇒
      if (value != null) en.lazySubPart = { x ⇒ new ObjectEdit(page, value.asInstanceOf[api.SkillObject]) }
    case _ ⇒
  }
  contents += en

}