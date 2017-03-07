package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import de.ust.skill.common.scala.internal.fieldTypes._;
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import qq.editor.binding.SkillFieldProperty

class FieldEdit[F, O <: api.SkillObject](
  val page: ObjectPage,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldDeclaration[F])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  field.t.asInstanceOf[FieldType[_]] match {
    case _: AnnotationType
      | _: UserType[_] ⇒
      val p = new qq.editor.binding.SimpleField(null, page.file, pool, obj, field)
      contents += new ReferenceEdit(p.asInstanceOf[SkillFieldProperty[api.SkillObject]], page)
    case c: ListType[f] ⇒
      contents += new IndexedContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[Buffer[f]]],
        () ⇒ NewValue.default(c.groundType))
    case c: VariableLengthArray[f] ⇒
      contents += new IndexedContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[Buffer[f]]],
        () ⇒ NewValue.default(c.groundType))
    case c: SetType[e] ⇒ 
      contents += new SetContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[HashSet[e]]],
        () ⇒ NewValue.prompt(c.groundType,"New entry:",page))       
    case c: ConstantLengthArray[f] ⇒
      contents += new IndexedContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[Buffer[f]]],
        canResize = false)
    case m: MapType[k, v] ⇒
            contents += new MapContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldDeclaration[HashMap[k,v]]])
    /* constants are only shown in the type; they're stored there, anyway */
    case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_)
      | ConstantV64(_) ⇒
      ()
    case BoolType | _: StringType | I8 | I16 | I32 | I64 | V64 | F32 | F64 ⇒
      val p = new qq.editor.binding.SimpleField(null, page.file, pool, obj, field)
      val editField = if ( field.t.asInstanceOf[FieldType[_]].isInstanceOf[StringType]) 
        new qq.util.binding.TextEdit(p.asInstanceOf[SkillFieldProperty[String]],
        {x ⇒ val s = x.trim()
            if (s == "(null)") null 
            else if (s.head == '"' && s.last == '"')
              scala.StringContext.treatEscapes(s.tail.dropRight(1))
            else throw new qq.util.binding.RestrictionException("expected string in double quotation marks")
        },
        {(x:String)  ⇒ if (x == null) "(null)"
           else ""+scala.reflect.runtime.universe.Literal(scala.reflect.runtime.universe.Constant(x))
        })
      else p.defaultEditor
      val ed = new qq.util.binding.LabeledEdit(editField)
      contents += new qq.util.ExpandableNode(ed, false)
    
  }

}