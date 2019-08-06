package qq.editor.binding

import ogss.common.scala.api
import ogss.common.scala.internal.Obj
import ogss.common.scala.internal.Field
//import ogss.common.scala.internal.restrictions.CheckableFieldRestriction
import ogss.common.scala.internal.Pool
//import ogss.common.scala.api.RestrictionCheckFailed
import qq.util.binding.Restriction

/** Make [[qq.util.binding.Restriction]]s for SKilL fields */
object Restrictions {
  /** internal representation of checkable field restrictions of field x */
  def apply[E, T](x: api.FieldAccess[T]): Iterator[Restriction[E]] = {
    x.asInstanceOf[Field[T, _]].restrictions.iterator.flatMap {
//      case cr: CheckableFieldRestriction[E] ⇒
//        Some(new Restriction[E]() {
//          override val conditionMessage = "unknown"
//          override val test: E ⇒ Boolean = validationMessage(_).isEmpty
//          override def validationMessage(x: E): Option[String] = {
//            try {
//              cr.check(x)
//              None
//            } catch {
//              case e: RestrictionCheckFailed ⇒ Some(e.msg)
//            }
//          }
//        })
      case _ ⇒ None
    }
  }
  /** restrict values to τ and sub-type for user types */
  def apply[T](file: qq.editor.File, τ: api.FieldType[T]): Iterator[Restriction[T]] = {
    if (τ.isInstanceOf[Pool[_]]) {
      Iterator(Restriction(
        { (x: T) ⇒
          x == null || {
            val t = file.s.pool(x.asInstanceOf[Obj]).asInstanceOf[Pool[_ <: Obj]]
            t == τ || file.superTypes(t).contains(τ)
          }
        },
        "object must have type " + τ + " or sub-type"))
    } else { Iterator() }
  }

}