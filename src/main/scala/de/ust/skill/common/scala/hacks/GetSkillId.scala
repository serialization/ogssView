package ogss.common.scala.hacks

import ogss.common.scala.internal.Obj
/** Hack: an object in a package that is allowed to access the skillID of a SkillObject. */
object GetSkillId {
  /** Return the value of the protected field [[SkillObject.skillID]]
   *  @param o a [[SkillObject]]
   *  @return the [[SkillObject.skillID]] of `o`*/
  def apply(o: Obj) = o.ID
}