package qq.editor.queries

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;

class DirectTypeQuery(val file0: qq.editor.File,
    val variable: String,
    val pool: api.Access[_]) extends Query(file0) {
  
  override def variables = Seq(variable)
  override def find() = {
    for (o <- pool.asInstanceOf[internal.StoragePool[_, _]].staticInstances) yield Map(variable -> o)
  }
  override def find(bound: Iterator[Map[String, Any]]) = {
    /* we must ensure (outside) that b contains varialbe: we want to disallow the generation of full cartesian products */
    for (b <- bound
        if b(variable).isInstanceOf[api.SkillObject]
        if pool.name == b(variable).asInstanceOf[api.SkillObject].getTypeName) 
      yield b
  }
  
  
}