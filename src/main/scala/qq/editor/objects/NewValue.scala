package qq.editor.objects

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal.fieldTypes._;
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

/**
 * Functions to create a new value of a ground type; either default or ask the user
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
        throw new Exception("field type $τ is not ground and does not have a default value")
    }
  }
}