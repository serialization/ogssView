package de.ust.skill.common.scala.hacks

import de.ust.skill.common.scala.api.SkillObject
/** Hack: an object in a package that is allowed to access the skillID of a SkillObject. */
object GetSkillId {
  /** Return the value of the protected field [[SkillObject.skillID]]
   *  @param o a [[SkillObject]]
   *  @return the [[SkillObject.skillID]] of `o`*/
  def apply(o: SkillObject) = o.skillID
}