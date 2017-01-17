package qq.editor.types

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes;

/**
 * A control for displaying a field type on a page with user types turned into links
 */
class FieldTypeControl(val page: TypePage, val fieldType: api.FieldType[_])
    extends swing.BoxPanel(swing.Orientation.Horizontal) {

  contents ++=
    (fieldType.asInstanceOf[fieldTypes.FieldType[_]] match {
      case u: fieldTypes.UserType[_] ⇒ Seq(new TypeNameControl(page, u))
      // case c: fieldTypes.SingleBaseTypeContainer[_, T] ⇒ o.get(field).asInstanceOf[Iterable[T]].size == 0
      case c: fieldTypes.ListType[_] ⇒ Seq(
        new swing.Label("list<"),
        new FieldTypeControl(page, c.groundType),
        new swing.Label(">"))
      case c: fieldTypes.SetType[_] ⇒ Seq(
        new swing.Label("set<"),
        new FieldTypeControl(page, c.groundType),
        new swing.Label(">"))
      case c: fieldTypes.VariableLengthArray[_] ⇒ Seq(
        new FieldTypeControl(page, c.groundType),
        new swing.Label("[]"))
      case c: fieldTypes.ConstantLengthArray[_] ⇒ Seq(
        new FieldTypeControl(page, c.groundType),
        new swing.Label("[" + c.length + "]"))
      case m: fieldTypes.MapType[_, _] ⇒ Seq(
        new swing.Label("map<"),
        new FieldTypeControl(page, m.keyType),
        new swing.Label(", "),
        new FieldTypeControl(page, m.valueType),
        new swing.Label(">"))
      //    case fieldTypes.ConstantI8(_)                    ⇒ true
      //    case fieldTypes.ConstantI16(_)                   ⇒ true
      //    case fieldTypes.ConstantI32(_)                   ⇒ true
      //    case fieldTypes.ConstantI64(_)                   ⇒ true
      //     case fieldTypes.ConstantV64(_)                   ⇒ true
      //      case _: fieldTypes.AnnotationType                ⇒ 
      case _ ⇒ Seq(new swing.Label(fieldType.toString()))
    })
    peer.setMaximumSize(peer.getMinimumSize)

}