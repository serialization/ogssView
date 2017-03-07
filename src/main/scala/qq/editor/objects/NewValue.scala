package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes._;
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import qq.editor.binding.PromptSkillFieldProperty

/**
 * Functions to create a new value of a ground type; either default or ask the user
 * TODO restrictions
 */
object NewValue {
  def default[T](τ: api.FieldType[T]): T = {
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
          x += default(τ.groundType)
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

  def prompt[T](τ: api.FieldType[T], prompt: String, page: ObjectPage): T = {
    // special case for references TODO
    val p = τ.asInstanceOf[FieldType[T]] match {
      case BoolType  ⇒ new PromptSkillFieldProperty(null, prompt, false, τ)
      case I8        ⇒ new PromptSkillFieldProperty(null, prompt, 0.toByte, τ)
      case I16       ⇒ new PromptSkillFieldProperty(null, prompt, 0.toShort, τ)
      case I32       ⇒ new PromptSkillFieldProperty(null, prompt, 0, τ)
      case I64 | V64 ⇒ new PromptSkillFieldProperty(null, prompt, 0L, τ)
      case F64       ⇒ new PromptSkillFieldProperty(null, prompt, 0.0, τ)
      case F32       ⇒ new PromptSkillFieldProperty(null, prompt, 0.0f, τ)
      case _: AnnotationType | _: UserType[_] ⇒
        new PromptSkillFieldProperty[T](null, prompt, null.asInstanceOf[T], τ)
      case _: StringType ⇒ new PromptSkillFieldProperty(null, prompt, "", τ)
      case _: ConstantLengthArray[_] | _: ListType[_] | _: VariableLengthArray[_]
        | _: SetType[_] | _: MapType[_, _] ⇒
        throw new Exception(s"non-ground field type $τ does not have prompt")
      case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_)
        | ConstantV64(_) ⇒
        throw new Exception(s"constant field type $τ does not have prompt")
    }
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
      /* explicitly add strings to the string pool, otherwise serialsation
       * fails when it is used as key of a map. */
      page.file.s.String.add(result.asInstanceOf[String])
    }
    result
  }
}