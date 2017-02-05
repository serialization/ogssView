package qq.editor.queries

import de.ust.skill.common.scala.api;

/** ?x type identifier */
class TypeQuery(val file0: qq.editor.File,
                val variable: String,
                val pool: api.Access[_]) extends Query(file0) {

  override def variables = Seq(variable)
  override def find() = {
    for (o ← pool.all) yield Map(variable -> o)
  }
  override def prepare(assigned: Seq[String]) = ()
  override def find(assigment: Map[String, Any]) = {
    /* we must ensure (outside) that b contains variable: we want to disallow the generation of full cartesian products */
    if (assigment(variable).isInstanceOf[api.SkillObject]
      && file.superTypes(file.s(assigment(variable).asInstanceOf[api.SkillObject].getTypeName)).contains(pool)) {
      Iterator(assigment)
    } else {
      Iterator()
    }
  }

  override def costSizeEstimate = {
    /* iterate over n instances and return all of them */
    val n = pool.size
    (n.toDouble, n.toDouble)
  }
  override def costSizeEstimate(assigned: Seq[String]) = {
    if (!assigned.contains(variable)) {
      (Double.PositiveInfinity, Double.PositiveInfinity)
    } else {
      /* check whether bound value is an instance, assume to filter out 9 of 10 */
      (1.0, 0.1)
    }
  }

}