package qq.editor.objects

import ogss.common.scala.api
import ogss.common.scala.internal
import ogss.common.scala.internal.fieldTypes._
import scala.collection.mutable.Buffer
import qq.editor.binding.SkillFieldProperty
/**
 * swing UI element for modifying a [[SkillFieldProperty]].
 * 
 * @param The [[qq.editor.Page]] in which this UI element is shown 
 * @param typ type of the property (can probably be removed, now that the fieldProperty exports its groundType)
 * @param fieldProperty the element of a container field that is modified
 * @param addLabel whether or not a label with the name of the fieldProperty is shown
 * */
class ElementFieldEdit[E, O <: internal.Obj](
  val page: qq.editor.Page,
  val typ: internal.FieldType[_],
  val fieldProperty: SkillFieldProperty[E],
  val addLabel: Boolean = true)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val (editField: qq.util.binding.EditControl[E], wholeComponent) = (typ match {
    case _: internal.AnyRefType
      | _: internal.Pool[_] ⇒
      val ed = new ReferenceEdit(fieldProperty.asInstanceOf[SkillFieldProperty[internal.Obj]], page, addLabel)
      (ed.editField, ed)    
    case _: ListType[_]
      | _: SetType[_]
      | _: ArrayType[_]
      | _: MapType[_, _] ⇒
      throw new Exception(s"required ground type, found container ${typ}")
    case I8 | I16 | I32 | I64 | V64 | F32 | F64 | Bool | _: internal.StringPool ⇒
      val editField = if (typ.isInstanceOf[internal.StringPool]) 
        new qq.util.binding.TextEdit(fieldProperty.asInstanceOf[SkillFieldProperty[String]],
        {x ⇒ val s = x.trim()
            if (s == "(null)") null 
            else if (s.head == '"' && s.last == '"')
              scala.StringContext.treatEscapes(s.tail.dropRight(1))
            else throw new qq.util.binding.RestrictionException("expected string in double quotation marks")
        },
        {(x:String)  ⇒ if (x == null) "(null)"
          // TODO better escaping function: ….Literal(….Constant("\0011")) = "\"\\011\"" → no round trip
           else ""+scala.reflect.runtime.universe.Literal(scala.reflect.runtime.universe.Constant(x))
        })
      else fieldProperty.defaultEditor
      val optLabel = if (addLabel) new qq.util.binding.LabeledEdit(editField) else editField
      (editField, new qq.util.ExpandableNode(optLabel, false))
  })

  contents += wholeComponent
}