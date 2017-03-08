package qq.editor.binding

import de.ust.skill.common.scala.api
import de.ust.skill.common.scala.internal.FieldDeclaration
import de.ust.skill.common.scala.internal.restrictions.CheckableFieldRestriction
import de.ust.skill.common.scala.internal.fieldTypes.UserType
import de.ust.skill.common.scala.api.RestrictionCheckFailed
import qq.util.binding.Restriction

object Restrictions {
  /** internal representation of checkable field restrictions of field x */
  def apply[E, T](x: api.FieldDeclaration[T]): Iterator[Restriction[E]] = {
    x.asInstanceOf[FieldDeclaration[T, _]].restrictions.iterator.flatMap {
      case cr: CheckableFieldRestriction[E] ⇒
        Some(new Restriction[E]() {
          override val conditionMessage = "unknown"
          override val test: E ⇒ Boolean = validationMessage(_).isEmpty
          override def validationMessage(x: E): Option[String] = {
            try {
              cr.check(x)
              None
            } catch {
              case e: RestrictionCheckFailed ⇒ Some(e.msg)
            }
          }
        })
      case _ ⇒ None
    }
  }
  /** restrict values to τ and sub-type for user types */
  def apply[T](file: qq.editor.File, τ: api.FieldType[T]): Iterator[Restriction[T]] = {
    if (τ.isInstanceOf[UserType[_]]) {
      Iterator(Restriction(
        { (x: T) ⇒
          x == null || {
            val t = file.s(x.asInstanceOf[api.SkillObject].getTypeName)
            t == τ || file.superTypes(t).contains(τ)
          }
        },
        "object must have type " + τ + " or sub-type"))
    } else { Iterator() }
  }

}