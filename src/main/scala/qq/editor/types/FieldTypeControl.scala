package qq.editor.types

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes;

/**
 * A control for displaying a field type on a page with user types turned into links
 */
class FieldTypeControl(
    val page: TypePage,
    val file: qq.editor.File,
    val fieldType: api.FieldType[_])
    extends swing.BoxPanel(swing.Orientation.Horizontal) {

  background = java.awt.SystemColor.text

  contents ++=
    (fieldType.asInstanceOf[fieldTypes.FieldType[_]] match {
      case u: fieldTypes.UserType[_] ⇒ Seq(new TypeNameControl(page, u))
      case c: fieldTypes.ListType[_] ⇒ Seq(
        new qq.util.PlainLabel("list<"),
        new FieldTypeControl(page, file, c.groundType),
        new qq.util.PlainLabel(">"))
      case c: fieldTypes.SetType[_] ⇒ Seq(
        new qq.util.PlainLabel("set<"),
        new FieldTypeControl(page, file,c.groundType),
        new qq.util.PlainLabel(">"))
      case c: fieldTypes.VariableLengthArray[_] ⇒ Seq(
        new FieldTypeControl(page, file,c.groundType),
        new qq.util.PlainLabel("[]"))
      case c: fieldTypes.ConstantLengthArray[_] ⇒ Seq(
        new FieldTypeControl(page, file,c.groundType),
        new qq.util.PlainLabel("[" + c.length + "]"))
      case m: fieldTypes.MapType[_, _] ⇒ Seq(
        new qq.util.PlainLabel("map<"),
        new FieldTypeControl(page, file,m.keyType),
        new qq.util.PlainLabel(", "),
        new FieldTypeControl(page, file,m.valueType),
        new qq.util.PlainLabel(">"))
      case _ ⇒ Seq(new qq.util.PlainLabel(fieldType.toString()))
    })
    peer.setMaximumSize(peer.getMinimumSize)

}