package qq.editor

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import scala.collection.mutable.Buffer;
import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.ListBuffer;
import scala.collection.mutable.HashSet;
import scala.collection.mutable.HashMap;
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
  for (f ← p.allFields if !f.isInstanceOf[internal.fieldTypes.ConstantInteger[_]]) {
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

  val refdBy = getRefdBy

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
    // restore references
    for ((o, f) ← refdBy) {
      file.modify_(new SimpleFieldEdit(file, file.s(o.getTypeName).asInstanceOf[api.Access[api.SkillObject]], o, f, null.asInstanceOf[T], obj))
      val rs = f.asInstanceOf[internal.FieldDeclaration[_, _]].restrictions
      import de.ust.skill.common.scala.internal.restrictions._
      val crs = rs.filter(_.isInstanceOf[CheckableFieldRestriction[_]]).map(_.asInstanceOf[CheckableFieldRestriction[T]])
      for (cr ← crs) {
        try {
          cr.check(o.get(f))
          file.validationErrors -= ((o, f))
        } catch {
          case _: Exception ⇒ ()
        }
      }
    }
  }
  override def getPresentationName = s"deleted object ${file.idOfObj(obj)}"
  override def getRedoPresentationName = s"delete object ${file.idOfObj(obj)}"
  override def getUndoPresentationName = s"undelete object ${file.idOfObj(obj)}"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = {
    // ungy side effect… set all refs to this to null so that they do not have to be considered always when
    // ref fields are shown
    for ((o, f) ← refdBy) {
      file.modify_(new SimpleFieldEdit(file, file.s(o.getTypeName).asInstanceOf[api.Access[api.SkillObject]], o, f, o.get(f), null.asInstanceOf[T]))
      val rs = f.asInstanceOf[internal.FieldDeclaration[_, _]].restrictions
      import de.ust.skill.common.scala.internal.restrictions._
      val crs = rs.filter(_.isInstanceOf[CheckableFieldRestriction[_]]).map(_.asInstanceOf[CheckableFieldRestriction[T]])
      for (cr ← crs) {
        try {
          cr.check(o.get(f))
        } catch {
          case _: Exception ⇒
            file.validationErrors += ((o, f))
        }
      }
    }

    new DeleteObject(file, pool, obj)
  }

  /* check references */
  private def getRefdBy: ListBuffer[Tuple2[api.SkillObject, api.FieldDeclaration[T]]] = {
    import de.ust.skill.common.scala.internal.fieldTypes._
    val result = new ListBuffer[Tuple2[api.SkillObject, api.FieldDeclaration[T]]]()
    def fieldCanRefToO(field: api.FieldDeclaration[_]): Boolean = {
      field.t.isInstanceOf[AnnotationType] || (
        field.t.isInstanceOf[api.Access[_]] && (
          field.t.asInstanceOf[api.Access[_]] == file.s(o.getTypeName) ||
          file.superTypes(file.s(o.getTypeName)).contains(field.t.asInstanceOf[api.Access[_]])))

    }
    for (
      fields ← f.fieldsByName.values;
      (pool, field) ← fields if fieldCanRefToO(field);
      ob ← pool.all if ob.asInstanceOf[api.SkillObject].get(field) == obj
    ) {
      result += ((ob.asInstanceOf[api.SkillObject], field.asInstanceOf[api.FieldDeclaration[T]]))
    }
    result
  }
  if (refdBy.size > 0) {
    import swing.Dialog
    val x = Dialog.showConfirmation(null,
      "Object " + file.idOfObj(obj) + " is referenced by " + refdBy.size + " objects. Delete anyway?",
      "Confirm delete",
      Dialog.Options.YesNo,
      Dialog.Message.Question, null)

    if (x == Dialog.Result.Yes) {
      file.modify(this)
    }
  } else {
    // no references, no confirm
    file.modify(this)
  }
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

  if (qq.util.Neq(oldValue, newValue)) file.modify(this)
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

  if (qq.util.Neq(oldValue, newValue)) file.modify(this)
}

/** edits of things like arrays and lists that have indexed objects */
sealed abstract class UserSetEdit[T <: api.SkillObject, C <: HashSet[F], F](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  val field: api.FieldDeclaration[C],
  /** The member that is modified */
  val key: F)
    extends UserEdit[T](f, p, o) {

}

/** insertion of a new value into an indexed container */
final case class UserSetInsert[T <: api.SkillObject, C <: HashSet[F], F](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  fd: api.FieldDeclaration[C],
  /** inserted member (constructor will throw if it already exists) */
  k: F)
    extends UserSetEdit[T, C, F](f, p, o, fd, k) {

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false

  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new SetRemove(file, pool, obj, field, key))
  }
  override def getPresentationName = s"inserted $key into ${field.name} of ${file.idOfObj(obj)}"
  override def getRedoPresentationName = s"insert $key into ${field.name} of ${file.idOfObj(obj)}"
  override def getUndoPresentationName = s"remove $key from ${field.name} of ${file.idOfObj(obj)}"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new SetInsert(file, pool, obj, field, key)

  if (obj.get(field).contains(key)) {
    throw new IllegalStateException("Insert into set: element already exists")
  } else {
    file.modify(this)
  }
}
/** insertion of a new value into an indexed container */
final case class UserSetRemove[T <: api.SkillObject, C <: HashSet[F], F](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  fd: api.FieldDeclaration[C],
  /** deleted member (constructor will throw if it does not exist) */
  k: F)
    extends UserSetEdit[T, C, F](f, p, o, fd, k) {

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false

  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new SetInsert(file, pool, obj, field, key))
  }
  override def getPresentationName = s"removed $key from ${field.name} of ${file.idOfObj(obj)}"
  override def getRedoPresentationName = s"remove $key from ${field.name} of ${file.idOfObj(obj)}"
  override def getUndoPresentationName = s"insert $key into ${field.name} of ${file.idOfObj(obj)}"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new SetRemove(file, pool, obj, field, key)

  if (!obj.get(field).contains(key)) {
    throw new IllegalStateException("Remove from set: element does not exist")
  } else {
    file.modify(this)
  }
}

/** insertion of a new value into an indexed container */
final case class UserSetReplace[T <: api.SkillObject, C <: HashSet[F], F](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  fd: api.FieldDeclaration[C],
  /** member that is replaced (constructor will throw if it does not exist) */
  k: F,
  /** member that it is replaced with (throw is exist )*/
  newKey: F)
    extends UserSetEdit[T, C, F](f, p, o, fd, k) {

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false

  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new SetReplace(file, pool, obj, field, newKey, key))
  }
  override def getPresentationName = s"replaced $key with $newKey in ${field.name} of ${file.idOfObj(obj)}"
  override def getRedoPresentationName = s"replace $key with $newKey in ${field.name} of ${file.idOfObj(obj)}"
  override def getUndoPresentationName = s"replaced $newKey with $key in ${field.name} of ${file.idOfObj(obj)}"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new SetReplace(file, pool, obj, field, key, newKey)

  if (!obj.get(field).contains(key)) {
    throw new IllegalStateException("Replace in set: element does not exist")
  } else {
    if (obj.get(field).contains(newKey)) {
      throw new IllegalStateException("Replace in set: new value already exists")
    } else {
      file.modify(this)
    }
  }
}

/** edits maps as lists indexed by  key sequence */
sealed abstract class UserMapEdit[T <: api.SkillObject, K, V, C[K, V] <: HashMap[K, V]](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  val field: api.FieldDeclaration[C[K, V]],
  /** The index of the modified member of the collection */
  val index: Seq[Any])
    extends UserEdit[T](f, p, o) {

}

/** insertion of a new value into a map */
final case class UserMapInsert[T <: api.SkillObject, K, V, C[K, V] <: HashMap[K, V]](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  fd: api.FieldDeclaration[C[K, V]],
  /** The index of the modified member of the collection */
  i: Seq[Any],
  /** the value of the new member */
  val value: Any)
    extends UserMapEdit[T, K, V, C](f, p, o, fd, i) {

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false

  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new MapRemove(file, pool, obj, field, index, value))
  }
  override def getPresentationName = s"inserted $value into ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def getRedoPresentationName = s"insert $value into ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def getUndoPresentationName = s"remove $value from ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new MapInsert(file, pool, obj, field, index, value)

  import qq.util.FlattenedMap.contains
  import de.ust.skill.common.scala.internal.fieldTypes.MapType
  if (contains(obj.get(field), field.t.asInstanceOf[MapType[K, V]], index)) {
    throw new IllegalStateException("Insert into map: key already exists")
  } else {
    file.modify(this)
  }
}

/** insertion of a new value into an indexed container */
final case class UserMapRemove[T <: api.SkillObject, K, V, C[K, V] <: HashMap[K, V]](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  fd: api.FieldDeclaration[C[K, V]],
  /** The index of the modified member of the collection */
  i: Seq[Any])
    extends UserMapEdit[T, K, V, C](f, p, o, fd, i) {

  import qq.util.FlattenedMap.contains
  import qq.util.FlattenedMap.get
  import de.ust.skill.common.scala.internal.fieldTypes.MapType

  val oldValue: Any = get(obj.get(field), field.t.asInstanceOf[MapType[K, V]], index)

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false

  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new MapInsert(file, pool, obj, field, index, oldValue))
  }
  override def getPresentationName = s"remove $oldValue from ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def getRedoPresentationName = s"remove $oldValue from ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def getUndoPresentationName = s"re-insert $oldValue into ${field.name} of ${file.idOfObj(obj)} at index $index"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new MapRemove(file, pool, obj, field, index, oldValue)

  if (!contains(obj.get(field), field.t.asInstanceOf[MapType[K, V]], index)) {
    throw new IllegalStateException("Remove from map: key does not exists")
  } else {
    file.modify(this)
  }
}

/** change of the value of a member of an indexed container */
final case class UserMapModify[T <: api.SkillObject, K, V, C[K, V] <: HashMap[K, V]](
  /** The file this belongs to */
  f: qq.editor.File,
  /** The type of the modified object*/
  p: api.Access[T],
  /** The object that is modified */
  o: T,
  /** The field (collection) that is modified */
  fd: api.FieldDeclaration[C[K, V]],
  /** The index of the modified member of the collection */
  i: Seq[Any],
  /** Value after modification */
  val newValue: Any)
    extends UserMapEdit[T, K, V, C](f, p, o, fd, i) {

  import qq.util.FlattenedMap.contains
  import qq.util.FlattenedMap.get
  import de.ust.skill.common.scala.internal.fieldTypes.MapType

  val oldValue: Any = get(obj.get(field), field.t.asInstanceOf[MapType[K, V]], index)

  /* no merge */
  override def addEdit(x: UndoableEdit) = false
  override def replaceEdit(x: UndoableEdit) = false

  override def canRedo = true
  override def canUndo = true
  override def redo = {
    file.modify_(toEdit)
  }
  override def undo = {
    file.modify_(new MapModify(file, pool, obj, field, index, newValue, oldValue))
  }
  override def getPresentationName = s"changed ${field.name}($index) of ${file.idOfObj(obj)} from $oldValue to $newValue"
  override def getRedoPresentationName = s"change ${field.name}($index) of ${file.idOfObj(obj)} from $oldValue to $newValue"
  override def getUndoPresentationName = s"change ${field.name}($index) of ${file.idOfObj(obj)} from $newValue back to $oldValue"
  override def isSignificant = true
  override def die() = {}

  override def toEdit = new MapModify(file, pool, obj, field, index, oldValue, newValue)

  if (qq.util.Neq(oldValue, newValue)) {
    if (!contains(obj.get(field), field.t.asInstanceOf[MapType[K, V]], index)) {
      throw new IllegalStateException("Map modify: key does not exists")
    } else {
      file.modify(this)
    }

  }
}
