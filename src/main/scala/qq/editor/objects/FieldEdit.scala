package qq.editor.objects

import ogss.common.scala.api;
import ogss.common.scala.internal;
import ogss.common.scala.internal.fieldTypes._;
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import qq.editor.binding.SkillFieldProperty

/**
 * swing UI element for modifying a field of the SKilL file.
 * 
 * @param The [[qq.editor.Page]] in which this UI element is shown 
 * @param pool a user type
 * @param obj an object in `pool`
 * @param field a field of `obj`
 * 
 * TODO Should be possible to reuse [[ElementFieldEdit]] for ground types
 * */

class FieldEdit[F, O <: internal.Obj](
  val page: qq.editor.Page,
  val pool: api.Access[O],
  val obj: O,
  val field: api.FieldAccess[F])
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val restrictions = field.asInstanceOf[internal.Field[F,O]].restrictions
  field.t.asInstanceOf[internal.FieldType[_]] match {
    case _: internal.AnyRefType
      | _: internal.Pool[_] ⇒
      val p = new qq.editor.binding.SimpleField(null, page.file, pool, obj, field)
      contents += new ReferenceEdit(p.asInstanceOf[SkillFieldProperty[internal.Obj]], page)
    case c: ListType[f] ⇒
      contents += new IndexedContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldAccess[Buffer[f]]],
        () ⇒ NewValue.default(c.base))//, restrictions))
    case c: ArrayType[f] ⇒
      contents += new IndexedContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldAccess[Buffer[f]]],
        () ⇒ NewValue.default(c.base))//, restrictions))
    case c: SetType[e] ⇒ 
      contents += new SetContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldAccess[HashSet[e]]])       
    case m: MapType[k, v] ⇒
            contents += new MapContainerEdit(page, pool, obj,
        field.asInstanceOf[api.FieldAccess[HashMap[k,v]]])
    case Bool | _: internal.StringPool | I8 | I16 | I32 | I64 | V64 | F32 | F64 ⇒
      val p = new qq.editor.binding.SimpleField(null, page.file, pool, obj, field)
      val editField = if ( field.t.asInstanceOf[internal.FieldType[_]].isInstanceOf[internal.StringPool]) 
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