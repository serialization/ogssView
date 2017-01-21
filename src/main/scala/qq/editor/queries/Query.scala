package qq.editor.queries

import de.ust.skill.common.scala.api;

/**
 * A query that will return a iterable list of the (sets of) objects that match
 * the query.
 *
 * Queries will be used to
 * * list all objects in a pool
 * * list all values of a collection
 * * trivial case: retrieve an individual object
 *
 * in the end, it should become a sparql-like query language, but we will see how far we get
 */
abstract class Query(
    /**the file that is queried */
    val file: qq.editor.File) {
  /** the names of the variables that are bound by this query */
  def variables: Seq[String]
  /** return all variable bindings that satisfy this query */
  def find(): Iterator[Map[String, Any]]
  /** return all variable bindings that extend bound and satisfy this query */
  def find(bound: Iterator[Map[String, Any]]):  Iterator[Map[String, Any]]
}

object Query {
  // def parse(x: String): Query = {

  //}

}