package qq.editor.queries

import de.ust.skill.common.scala.api;

/**
 * A field as used in the queries. The user can leave the type unspecified, in which
 * case a field may correspond to different fields in different user types
 */
abstract class Field {
  /** all field declarations this field can refer to, and the pool they belong to */
  def apply(): Iterator[Tuple2[api.Access[_], api.FieldDeclaration[_]]]
  /**
   * all field declarations this field can refer to, and the pool they belong to
   * when this field should be a field of object o (may be empty)
   */
  def apply(o: api.SkillObject): Iterator[Tuple2[api.Access[_], api.FieldDeclaration[_]]]
}

class UnspecificField(
  val file: qq.editor.File,
  val name: String)
    extends Field {

  override def apply() = {
    file.fieldsByName(name).toIterator
  }

  def apply(o: api.SkillObject) = {
    val relevantTypes = file.s(o.getTypeName) +: file.superTypes(file.s(o.getTypeName))
    this().filter(x ⇒ relevantTypes.contains(x._1))
  }
}

class SpecificTypeField(
  val file: qq.editor.File,
  val typeName: String,
  val name: String)
    extends Field {

  override def apply() = {
    val pool = file.s(typeName).asInstanceOf[api.Access[_<:api.SkillObject]]
    val field = pool.fields.find(_.name == name).get
    Iterator((pool, field))
  }

  def apply(o: api.SkillObject) = {
    val relevantTypes = file.s(o.getTypeName) +: file.superTypes(file.s(o.getTypeName))
    this().filter(x ⇒ relevantTypes.contains(x._1))
  }
}  
  