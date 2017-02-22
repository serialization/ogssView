package qq.editor.binding

import de.ust.skill.common.scala.api
import de.ust.skill.common.scala.internal.FieldDeclaration
import de.ust.skill.common.scala.internal.restrictions.CheckableFieldRestriction
import de.ust.skill.common.scala.api.RestrictionCheckFailed
import qq.util.binding.Restriction

object Restrictions {
  // TODO how to check restrictions for containers? internal.FieldDeclaration[T, ] uses CheckableFieldRestriction[T] internally, but those check T and not elements of T?
  def apply[T](x: api.FieldDeclaration[T]): Iterator[Restriction[T]] = {
  x.asInstanceOf[FieldDeclaration[T, _]].restrictions.iterator.flatMap {
      case cr: CheckableFieldRestriction[T] ⇒
        Some(new Restriction[T]() {
          override val conditionMessage = "unknown"
          override val test: T => Boolean = validationMessage(_).isEmpty
          override def validationMessage(x: T): Option[String] = {
            try {
              cr.check(x)
              None
            } catch {
              case e: RestrictionCheckFailed => Some(e.msg)
            }
          }
        })
      case _ ⇒ None
    }
  } 
}