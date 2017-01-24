package qq.editor.queries

import de.ust.skill.common.scala.api;

/** Special case: retrieve a specific object */
class IdQuery(
    val file0: qq.editor.File,
    val variable: String,
    val obj: api.SkillObject) extends Query(file0) {

  override def variables = Seq(variable)
  override def find() = {
    Seq(Map(variable -> obj)).toIterator
  }
  override def find(bound: Iterator[Map[String, Any]]) = {
    /* we must ensure (outside) that b contains varialbe: we want to disallow the generation of full cartesian products */
    for (
      b ← bound if b(variable).isInstanceOf[api.SkillObject] if obj == b(variable)
    ) yield b
  }
}