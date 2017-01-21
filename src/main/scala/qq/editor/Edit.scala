package qq.editor

import de.ust.skill.common.scala.api;
import scala.collection.mutable._;

/**
 * All actions that modify the content of the skill file
 */
sealed abstract class Edit[T <: api.SkillObject](
    /** The type of the modified object*/
    val pool: api.Access[T],
    /** The object that is modified */
    val obj: T) {

}

/** object creation */
final case class CreateObject[T <: api.SkillObject](
  /** The type of the created object*/
  p: api.Access[T],
  /** The object that is created */
  o: T)
    extends Edit[T](p, o) {

}

/** object deletion */
final case class DeleteObject[T <: api.SkillObject](
  /** The type of the deleted object*/
  p: api.Access[T],
  /** The object that is deleted */
  o: T)
    extends Edit[T](p, o) {

}

/** modification of a simple field */
final case class SimpleFieldEdit[T <: api.SkillObject, F](
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field that is modified */
  val field: api.FieldDeclaration[F],
  /** Original value of the field */
  val oldValue: F,
  /** Value after modification */
  val newValue: F)
    extends Edit[T](p, o) {

}
/** edits of things like arrays and lists that have indexed objects */
sealed abstract class IndexedContainerEdit[T <: api.SkillObject, C <: Iterable[F], F](
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  val field: api.FieldDeclaration[C],
  /** The index of the modified member of the collection */
  val index: Int)
    extends Edit[T](p, o) {

}

/** insertion of a new value into an indexed container */
final case class IndexedContainerInsert[T <: api.SkillObject, C <: Iterable[F], F](
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  f: api.FieldDeclaration[C],
  /** The index of the modified member of the collection */
  i: Int,
  /** the value of the new member (the new f[i]; the old f[i] becomes f[i+1] &c.)*/
  val value: F)
    extends IndexedContainerEdit[T, C, F](p, o, f, i) {

}

/** removal of an value from an indexed container */
final case class IndexedContainerRemove[T <: api.SkillObject, C <: Iterable[F], F](
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  f: api.FieldDeclaration[C],
  /** The index of the modified member of the collection */
  i: Int,
  /** the value of the deleted member (the old f[i]; the new f[i] was former f[i+1] &c.)*/
  val value: F)
    extends IndexedContainerEdit[T, C, F](p, o, f, i) {

}

/** change of the value of a member of an indexed container */
final case class IndexedContainerModify[T <: api.SkillObject, C <: Iterable[F], F](
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  f: api.FieldDeclaration[C],
  /** The index of the modified member of the collection */
  i: Int,
  /** Value before modification */
  val oldvalue: F,
  /** Value after modification */
  val newvalue: F)
    extends IndexedContainerEdit[T, C, F](p, o, f, i) {

}

/** edits of sets */
sealed abstract class SetEdit[T <: api.SkillObject, C <: HashSet[F], F](
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (set) that is modified */
  val field: api.FieldDeclaration[C],
  /** The modifed element */
  val key: F)
    extends Edit[T](p, o) {

}

/** insertion of a new element into a set*/
final case class SetInsert[T <: api.SkillObject, C <: HashSet[F], F](
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (set) that is modified */
  f: api.FieldDeclaration[C],
  /** The new member */
  k: F)
    extends SetEdit[T, C, F](p, o, f, k) {

}

/** removal of an element from a set */
final case class SetRemove[T <: api.SkillObject, C <: HashSet[F], F](
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (set) that is modified */
  f: api.FieldDeclaration[C],
  /** The removed member */
  k: F)
    extends SetEdit[T, C, F](p, o, f, k) {

}

/** replace an element (remove+insert) in a set */
final case class SetReplace[T <: api.SkillObject, C <: HashSet[F], F](
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  f: api.FieldDeclaration[C],
  /** The index of the modified member of the collection */
  i: Int,
  /** The element that is replaced */
  k: F,
  /** the new value it is replaced with */
  val replacement: F)
    extends SetEdit[T, C, F](p, o, f, k) {

}


