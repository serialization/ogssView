package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import de.ust.skill.common.scala.internal.fieldTypes;

class ObjectEdit[P <: api.SkillObject](
  val page: ObjectPage, 
  val obj: P)
    extends swing.BoxPanel(swing.Orientation.Vertical) 
    with qq.util.binding.PropertyOwner {

  val pool: api.Access[P] = page.file.s(obj.getTypeName).asInstanceOf[api.Access[P]]

  private def fieldEdit(field: api.FieldDeclaration[_]) = {
    field.t.asInstanceOf[fieldTypes.FieldType[_]] match {
      case u: fieldTypes.UserType[_] ⇒  new swing.Label("Todo")
      case c: fieldTypes.ListType[_] ⇒ new swing.Label("Todo")
      case c: fieldTypes.SetType[_] ⇒ new swing.Label("Todo")
      case c: fieldTypes.VariableLengthArray[_] ⇒ new swing.Label("Todo")
      case c: fieldTypes.ConstantLengthArray[_] ⇒ new swing.Label("Todo")
      case m: fieldTypes.MapType[_, _] ⇒ new swing.Label("Todo")
          case fieldTypes.ConstantI8(_)                    ⇒ new swing.Label("Todo")
          case fieldTypes.ConstantI16(_)                   ⇒ new swing.Label("Todo")
          case fieldTypes.ConstantI32(_)                   ⇒ new swing.Label("Todo")
          case fieldTypes.ConstantI64(_)                   ⇒ new swing.Label("Todo")
           case fieldTypes.ConstantV64(_)                   ⇒ new swing.Label("Todo")
            case _: fieldTypes.AnnotationType                ⇒  new swing.Label("Todo")
      case _ ⇒ 
        val p = new qq.editor.binding.SimpleField(this, page.file, pool, obj, field)
        val ed = new qq.util.binding.LabeledEdit(p.defaultEditor)
        new qq.util.ExpandableNode(ed)
    }
  }
  
  pool.allFields.foreach { f => contents += fieldEdit(f) }
  
}