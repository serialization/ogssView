package qq.editor.objects

import de.ust.skill.common.scala.api
import de.ust.skill.common.scala.internal.fieldTypes._
import scala.collection.mutable.Buffer
import qq.editor.binding.SkillFieldProperty

class ElementFieldEdit[E, O <: api.SkillObject](
  val page: qq.editor.Page,
  val typ: FieldType[_],
  val fieldProperty: SkillFieldProperty[E],
  val addLabel: Boolean = true)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val (editField: qq.util.binding.EditControl[E], wholeComponent) = (typ match {
    case _: AnnotationType
      | _: UserType[_] ⇒
      val ed = new ReferenceEdit(fieldProperty.asInstanceOf[SkillFieldProperty[api.SkillObject]], page, addLabel)
      (ed.editField, ed)    
    case _: ListType[_]
      | _: VariableLengthArray[_]
      | _: SetType[_]
      | _: ConstantLengthArray[_]
      | _: MapType[_, _] ⇒
      throw new Exception(s"required ground type, found container ${typ}")
    case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_) | ConstantV64(_) ⇒
      throw new Exception(s"required ground type, found constannt ${typ}")
    case I8 | I16 | I32 | I64 | V64 | F32 | F64 | BoolType | _: StringType ⇒
      val editField = if (typ.isInstanceOf[StringType]) 
        new qq.util.binding.TextEdit(fieldProperty.asInstanceOf[SkillFieldProperty[String]],
        {x ⇒ val s = x.trim()
            if (s == "(null)") null 
            else if (s.head == '"' && s.last == '"')
              scala.StringContext.treatEscapes(s.tail.dropRight(1))
            else throw new qq.util.binding.RestrictionException("expected string in double quotation marks")
        },
        {(x:String)  ⇒ if (x == null) "(null)"
           else ""+scala.reflect.runtime.universe.Literal(scala.reflect.runtime.universe.Constant(x))
        })
      else fieldProperty.defaultEditor
      val optLabel = if (addLabel) new qq.util.binding.LabeledEdit(editField) else editField
      (editField, new qq.util.ExpandableNode(optLabel, false))
  })

  contents += wholeComponent
}