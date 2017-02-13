package qq.editor

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import scala.collection.mutable._;
import javax.swing.undo._;

/**
 * All user triggered actions that modify the skill file.
 *
 * These are undoable edits; the original execution of a user edit and
 * its undos and redos all are qq.editor.Edits.
 *
 * Creating one of those objects will perform them by calling file.modify
 */
sealed abstract class UserEdit[T <: api.SkillObject](
  /** The file this belongs to */
  val file: qq.editor.File,
  /** The type of the modified object*/
  val pool: api.Access[T],
  /** The object that is modified */
  val obj: T)
    extends UndoableEdit {

  def toEdit(): qq.editor.Edit[T]

}

/** object creation */
final case class UserCreateObject[T <: api.SkillObject](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the created object*/
  p: api.Access[T])
    extends UserEdit[T](f, p, p.reflectiveAllocateInstance) {

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false
  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new DeleteObject(file, pool, obj))
  }
  override def getPresentationName = s"created new object ${file.idOfObj(obj)}"
  override def getRedoPresentationName = s"create new object ${file.idOfObj(obj)}"
  override def getUndoPresentationName = s"remove new object ${file.idOfObj(obj)}"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new CreateObject(file, pool, obj)

  file.registerCreatedObject(obj)
  /* initialise fields */
  private def fieldInitialisation[T](f: api.FieldDeclaration[T]): Unit = {
    obj.set(f, qq.editor.objects.NewValue.default(f.t))
  }
  for(f <- p.allFields if ! f.isInstanceOf[internal.fieldTypes.ConstantInteger[_]]) {
    fieldInitialisation(f)
  }
  
  file.modify(this)
}

final case class UserDeleteObject[T <: api.SkillObject](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the created object*/
  p: api.Access[T],
  /** Object that is deleted */
  o: T)
    extends UserEdit[T](f, p, o) {

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false

  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new CreateObject(file, pool, obj))
  }
  override def getPresentationName = s"deleted object ${file.idOfObj(obj)}"
  override def getRedoPresentationName = s"delete object ${file.idOfObj(obj)}"
  override def getUndoPresentationName = s"undelete object ${file.idOfObj(obj)}"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new DeleteObject(file, pool, obj)

  file.modify(this)
}

/** modification of a simple field */
final case class UserSimpleFieldEdit[T <: api.SkillObject, F](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field that is modified */
  val field: api.FieldDeclaration[F],
  /** Value after modification */
  val newValue: F)
    extends UserEdit[T](f, p, o) {

  val oldValue: F = obj.get(field)

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false

  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new SimpleFieldEdit(file, pool, obj, field, newValue, oldValue))
  }
  override def getPresentationName = s"changed ${field.name} of ${file.idOfObj(obj)} from $oldValue to $newValue"
  override def getRedoPresentationName = s"change ${field.name} of ${file.idOfObj(obj)} from $oldValue to $newValue"
  override def getUndoPresentationName = s"change ${field.name} of ${file.idOfObj(obj)} from $newValue back to $oldValue"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new SimpleFieldEdit(file, pool, obj, field, oldValue, newValue)

  if (oldValue != newValue) file.modify(this)
}

/** edits of things like arrays and lists that have indexed objects */
sealed abstract class UserIndexedContainerEdit[T <: api.SkillObject, C <: Iterable[F], F](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  val field: api.FieldDeclaration[C],
  /** The index of the modified member of the collection */
  val index: Int)
    extends UserEdit[T](f, p, o) {

}

/** insertion of a new value into an indexed container */
final case class UserIndexedContainerInsert[T <: api.SkillObject, C <: Buffer[F], F](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  fd: api.FieldDeclaration[C],
  /** The index of the modified member of the collection */
  i: Int,
  /** the value of the new member (the new f[i]; the old f[i] becomes f[i+1] &c.)*/
  val value: F)
    extends UserIndexedContainerEdit[T, C, F](f, p, o, fd, i) {

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false

  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new IndexedContainerRemove(file, pool, obj, field, index, value))
  }
  override def getPresentationName = s"inserted $value into ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def getRedoPresentationName = s"insert $value into ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def getUndoPresentationName = s"remove $value from ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new IndexedContainerInsert(file, pool, obj, field, index, value)

  file.modify(this)
}

/** insertion of a new value into an indexed container */
final case class UserIndexedContainerRemove[T <: api.SkillObject, C <: Buffer[F], F](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  fd: api.FieldDeclaration[C],
  /** The index of the modified member of the collection */
  i: Int)
    extends UserIndexedContainerEdit[T, C, F](f, p, o, fd, i) {

  val oldValue: F = obj.get(field)(index)

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false

  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new IndexedContainerInsert(file, pool, obj, field, index, oldValue))
  }
  override def getPresentationName = s"remove $oldValue from ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def getRedoPresentationName = s"remove $oldValue from ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def getUndoPresentationName = s"re-insert $oldValue into ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new IndexedContainerRemove(file, pool, obj, field, index, oldValue)

  file.modify(this)
}

/** change of the value of a member of an indexed container */
final case class UserIndexedContainerModify[T <: api.SkillObject, C <: Buffer[F], F](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  fd: api.FieldDeclaration[C],
  /** The index of the modified member of the collection */
  i: Int,
  /** Value after modification */
  val newValue: F)
    extends UserIndexedContainerEdit[T, C, F](f, p, o, fd, i) {

  val oldValue: F = obj.get(field)(i)

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false

  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new IndexedContainerModify(file, pool, obj, field, index, newValue, oldValue))
  }
  override def getPresentationName = s"changed ${field.name}($index) of ${file.idOfObj(obj)} from $oldValue to $newValue"
  override def getRedoPresentationName = s"change ${field.name}($index) of ${file.idOfObj(obj)} from $oldValue to $newValue"
  override def getUndoPresentationName = s"change ${field.name}($index) of ${file.idOfObj(obj)} from $newValue back to $oldValue"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new IndexedContainerModify(file, pool, obj, field, index, oldValue, newValue)

  if (oldValue != newValue) file.modify(this)
}
