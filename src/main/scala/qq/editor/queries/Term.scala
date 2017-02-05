package qq.editor.queries

abstract class Term {
  def canBeSubject: Boolean
  def canBeObject: Boolean
  def apply(assignment: Map[String, Any]): Any
  def definedIn(assignment: Map[String, Any]): Boolean
}

class VarTerm(val variable: String)
    extends Term {
  override def canBeSubject = true
  override def canBeObject = true
  override def apply(assignment: Map[String, Any]) = assignment(variable)
  override def definedIn(assignment: Map[String, Any]) = assignment.contains(variable)
}

class ConstTerm(val value: Any)
    extends Term {
  override def canBeSubject = value.isInstanceOf[de.ust.skill.common.scala.api.SkillObject]
  override def canBeObject = true
  override def apply(assignment: Map[String, Any]) = value
  override def definedIn(assignment: Map[String, Any]) = true
}