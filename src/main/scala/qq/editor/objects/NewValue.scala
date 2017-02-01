package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes._;

/**
 * Functions to create a new value of a ground type; either default or ask the user
 */
object NewValue {
  def default[T](τ: api.FieldType[T]): T = {
    τ.asInstanceOf[FieldType[T]] match {
      case BoolType                   ⇒ false.asInstanceOf[T]
      case I8 | I16 | I32 | I64 | V64 ⇒ 0.asInstanceOf[T]
      case F32 | F64                  ⇒ 0.0.asInstanceOf[T]
      case _: AnnotationType | _: UserType[_] ⇒
        null.asInstanceOf[T]
      case _: StringType ⇒ "".asInstanceOf[T]
      case ConstantI8(_) | ConstantI16(_) | ConstantI32(_) | ConstantI64(_)
        | ConstantV64(_) | _: SingleBaseTypeContainer[_, _] | _: MapType[_, _] ⇒
        throw new Exception("field type $τ is not ground and does not have a default value")
    }
  }
}