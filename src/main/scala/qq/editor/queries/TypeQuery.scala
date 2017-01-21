package qq.editor.queries

import de.ust.skill.common.scala.api;

/** ?x type identifier */
class TypeQuery(val file0: qq.editor.File,
    val variable: String,
    val pool: api.Access[_]) extends Query(file0) {
  
  override def variables = Seq(variable)
  override def find() = {
    for (o <- pool.all) yield Map(variable -> o)
  }
  override def find(bound: Iterator[Map[String, Any]]) = {
    /* we must ensure (outside) that b contains varialbe: we want to disallow the generation of full cartesian products */
    for (b <- bound
        if b(variable).isInstanceOf[api.SkillObject]
        if file.superTypes(file.s(b(variable).asInstanceOf[api.SkillObject].getTypeName)).contains(pool))
      yield b
  }
  
  
}