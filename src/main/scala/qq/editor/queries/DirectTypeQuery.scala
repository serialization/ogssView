package qq.editor.queries

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;

/** Search query returning all members of type `pool` but not of its subtypes. */
class DirectTypeQuery(val file0: qq.editor.File,
                      val variable: String,
                      val pool: api.Access[_]) extends Query(file0) {

  override def variables = Seq(variable)
  override def find() = {
    for (
      o ← pool.asInstanceOf[internal.StoragePool[_ <: api.SkillObject, _ <: api.SkillObject]].staticInstances
      if !file.deletedObjects.contains(o)
    ) yield Map(variable -> o)
  }
  override def prepare(assigned: Seq[String]) = ()
  override def find(assigment: Map[String, Any]) = {
    /* we must ensure (outside) that b contains variable: we want to disallow the generation of full cartesian products */
    if (assigment(variable).isInstanceOf[api.SkillObject]
      && pool.name == assigment(variable).asInstanceOf[api.SkillObject].getTypeName) {
      Iterator(assigment)
    } else {
      Iterator()
    }

  }

  override def costSizeEstimate = {
    /* iterate over n static instances and return all of them */
    val n = pool.asInstanceOf[internal.StoragePool[_, _]].staticInstances.size
    (n.toDouble, n.toDouble)
  }
  override def costSizeEstimate(assigned: Seq[String]): Tuple2[Double, Double] = {
    if (!assigned.contains(variable)) return (Double.PositiveInfinity, Double.PositiveInfinity)
    /* check whether bound object is an instance, assume to filter out 9 of 10 */
    (1.0, 0.1)
  }

}


object DirectTypeQuery {
  def apply(file: qq.editor.File,
            s: Term,
            pool: api.Access[_ <: api.SkillObject]) = { 
    s match {
      case v: VarTerm => new DirectTypeQuery(file, v.variable, pool)
      case _ => throw new Exception("Variable expected")
    }
  }
}
