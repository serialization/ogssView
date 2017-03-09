package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes._;
import de.ust.skill.common.scala.internal.restrictions._;
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import qq.editor.binding.PromptSkillFieldProperty

/**
 * Functions to create a new value of a ground type; either default or ask the user
 */
object NewValue {
  def default[T](τ: api.FieldType[T], restrictions: HashSet[FieldRestriction]): T = {
    // default?
    restrictions.find(_.isInstanceOf[DefaultRestriction[T]]) match {
      case Some(r: DefaultRestriction[T]) => return r.value;
      case None => ()
    }
    // range?
    restrictions.foreach { 
      case Range.RangeI8(min, max) => if (0 < min) return min.asInstanceOf[T];
      case Range.RangeI16(min, max) => if (0 < min) return min.asInstanceOf[T];
      case Range.RangeI32(min, max) => if (0 < min) return min.asInstanceOf[T];
       case Range.RangeI64(min, max) => if (0L < min) return min.asInstanceOf[T];
       case Range.RangeF32(min, max) => if (0L < min) return min.asInstanceOf[T];
       case Range.RangeF64(min, max) => if (0L < min) return min.asInstanceOf[T];
       case _ => ()
    }
 
    τ.asInstanceOf[FieldType[T]] match {
      case BoolType  ⇒ false.asInstanceOf[T]
      case I8        ⇒ 0.toByte.asInstanceOf[T]
      case I16       ⇒ 0.toShort.asInstanceOf[T]
      case I32       ⇒ 0.asInstanceOf[T]
      case I64 | V64 ⇒ 0L.asInstanceOf[T]
      case F64       ⇒ 0.0.asInstanceOf[T]
      case F32       ⇒ 0.0f.asInstanceOf[T]
      case _: AnnotationType | _: UserType[_] ⇒
        null.asInstanceOf[T]
      case _: StringType ⇒ "".asInstanceOf[T]
      case τ: ConstantLengthArray[e] ⇒
        val x = new ArrayBuffer[e](τ.length)
        for (_ ← 0 until τ.length) {
          x += default(τ.groundType, restrictions)
        }
        x.asInstanceOf[T]
      case _: ListType[e] ⇒
        (new ListBuffer[e]()).asInstanceOf[T]
      case _: VariableLengthArray[e] ⇒
        (new ArrayBuffer[e]()).asInstanceOf[T]
      case _: SetType[e] ⇒
        (new HashSet[e]()).asInstanceOf[T]
      case _: MapType[k, v] ⇒
        (new HashMap[k, v]()).asInstanceOf[T]
      case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_)
        | ConstantV64(_) ⇒
        throw new Exception(s"constant field typ $τ does not have a default value")
    }
  }

  def prompt[T](τ: api.FieldType[T], prompt: String, page: ObjectPage, restrictions: HashSet[FieldRestriction]): T = {
    // special case for references TODO
    val p = new PromptSkillFieldProperty(null, prompt,  default(τ, restrictions) , τ)
    val dlg = new swing.Dialog()
      val ed = new ElementFieldEdit(page, τ.asInstanceOf[FieldType[T]], p) 
      dlg.contents = qq.util.Swing.VBoxD(
           ed,
           new swing.Button(swing.Action("Ok") {
             ed.editField.componentToProperty()
             dlg.close
             }))
      dlg.modal = true
    dlg.open()
    val result = p().asInstanceOf[T]
    if (τ.isInstanceOf[StringType]) {
      /* explicitly add strings to the string pool, otherwise serialisation
       * fails when it is used as key of a map. */
      page.file.s.String.add(result.asInstanceOf[String])
    }
    result
  }
}