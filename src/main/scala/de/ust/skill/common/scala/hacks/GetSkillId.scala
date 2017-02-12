package de.ust.skill.common.scala.hacks

import de.ust.skill.common.scala.api.SkillObject

object GetSkillId {
  def apply(o: SkillObject) = o.skillID
}