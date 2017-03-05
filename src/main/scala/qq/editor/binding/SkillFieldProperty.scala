package qq.editor.binding

abstract class SkillFieldProperty[T](
    owner0: qq.util.binding.PropertyOwner,
    name0: String,
    init0: T) extends qq.util.binding.Property[T](owner0, name0, init0) {

  def groundType: de.ust.skill.common.scala.api.FieldType[T]

}

class PromptSkillFieldProperty[T](
    owner0: qq.util.binding.PropertyOwner,
    name0: String,
    init0: T,
    val groundType0: de.ust.skill.common.scala.api.FieldType[T]) extends SkillFieldProperty[T](owner0, name0, init0) {

  def groundType = groundType0

}

