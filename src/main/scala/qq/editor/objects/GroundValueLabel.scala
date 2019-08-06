package qq.editor.objects

import ogss.common.scala.api
import ogss.common.scala.internal
import ogss.common.scala.internal.fieldTypes._
import qq.util.Swing.HBoxD
import swing.Swing.HGlue

/** Swing UI element for displaying the value of ground type (without a way to modify it).
 *  
 *  references get a expandable pane that shows their referent */
class GroundValueLabel(
  val page: qq.editor.Page,
  val typ: api.FieldType[_],
  val value: Any)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val labelField = typ match {
    case _: internal.AnyRefType
      | _: internal.Pool[_] ⇒
      new swing.Label(page.file.idOfObj(value.asInstanceOf[internal.Obj]))
    case _: ListType[_]
      | _: SetType[_]
      | _: ArrayType[_]
      | _: MapType[_, _] ⇒
      throw new Exception(s"required ground type, found container ${typ}")
    case I8 | I16 | I32 | I64 | V64 | F32 | F64 | Bool | _: internal.StringPool ⇒
      if (value == null) {
        new swing.Label("(null)")
      } else {
        new swing.Label(value.toString)
      }
  }
  val en = new qq.util.ExpandableNode(HBoxD(labelField, HGlue), false)

  typ match {
    case _: internal.AnyRefType
      | _: internal.Pool[_] ⇒
      if (value != null) en.lazySubPart = { x ⇒ new ObjectEdit(page, value.asInstanceOf[internal.Obj]) }
    case _ ⇒
  }
  contents += en

}