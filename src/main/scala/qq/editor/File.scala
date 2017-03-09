package qq.editor

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import scala.collection.mutable;

/** Represents a skill file while opened in the editor */
class File(fn0: java.nio.file.Path) {
  /** The name of the skill file this is about (may change with save as) */
  var fileName = fn0

  private def pathAndName = {
    val dirnamepattern = "^(?:(.*)[\\\\/])?([^\\\\/]+)$".r
    fileName.toAbsolutePath().toString() match { case dirnamepattern(d, n) ⇒ (d, n) }
  }

  def fileNameOnly: String = pathAndName._2
  /** Window title: filename, path, programme name, and a plus sign when modified (like gvim) */
  def windowTitle: String = {
    val (d, n) = pathAndName
    n + (if (isModified) " + " else "") + " (" + d + ") – SKilL edit"
  }
  /** the SKilL state from fileName */
  val s: empty.api.SkillFile = empty.api.SkillFile.open(fn0, api.Read, api.Write)

  /** Undo/redo queue of the changes that were applied to the file after it was opened */
  val undoManager = new qq.util.UndoManager()

  /** true if file was modified, i.e. there is something to save */
  def isModified: Boolean = {
    /* TODO either we clear the undo queue when saving, or we need to add a dummy save event */
    undoManager.canUndo()
  }
  /** event that fires when the value of modified changes, probably a lot more often */
  val onModifiednessChange: qq.util.binding.Event[Boolean] = new qq.util.binding.Event()

  /**
   * event that fires whenever the file is edited
   */
  val onEdit: qq.util.binding.Event[qq.editor.Edit[_]] = new qq.util.binding.Event;

  /**
   * Perform an edit and notify the rest of the programme about it
   */
  def modify_(e: qq.editor.Edit[_]): Unit = {
    e.doIt()
    onEdit.fire(e)
    onModifiednessChange.fire(isModified)
  }

  /**
   * Perform an edit, add it to the undo-queue, and notify the rest of the programme about it
   */
  def modify(e: qq.editor.UserEdit[_]): Unit = {
    val e2 = e.toEdit
    e2.doIt()
    undoManager.addEdit(e)
    onEdit.fire(e2)
    onModifiednessChange.fire(isModified)
  }

  /**
   * we mark objects as deleted by adding them to a deleted object queue. This
   * allows us to undelete them in a way such that all other edits in the undo/redo
   * queue stay valid.
   *
   * The objects are deleted from the skill state before it is saved to file
   */
  val deletedObjects: mutable.HashSet[api.SkillObject] = new mutable.HashSet()

  /**
   * When deleting causes null references in non-null restricted fields, we add
   * the affected field to this list.
   */
  val validationErrors: mutable.HashSet[Tuple2[api.SkillObject, api.FieldDeclaration[_]]] = new mutable.HashSet()

  /**
   * created objects do not have a SkillId; we give them temporary negative ones;
   * item 0 has ID -1 and so on
   */
  val createdObjects: mutable.ListBuffer[api.SkillObject] = new mutable.ListBuffer()
  val createdObjectId: mutable.HashMap[api.SkillObject, Int] = new mutable.HashMap()
  def registerCreatedObject(o: api.SkillObject): Unit = {
    createdObjects.synchronized {
      val id = -1 - createdObjects.size
      createdObjects += o
      createdObjectId(o) = id
    }
  }

  /* some auxiliary functions about types */

  /** parentType(a) = b if a is directly derived from b, a ∉ Dom(parentType) if a is a root type */
  val parentType: Map[api.Access[_ <: api.SkillObject], api.Access[_ <: api.SkillObject]] =
    (for (t ← s; sn ← t.superName) yield (t, s(sn).asInstanceOf[api.Access[_ <: api.SkillObject]])).toMap
  /** a ∈ childTypes(b) if a is directly derived from b */
  val childTypes: Map[api.Access[_ <: api.SkillObject], Seq[api.Access[_ <: api.SkillObject]]] =
    s.groupBy(parentType.getOrElse(_, null))
  /** a ∈ rootTypes if ~(∃x)(x = parentType(a)) */
  val rootTypes: Set[api.Access[_ <: api.SkillObject]] = (for (t ← s if !(parentType contains t)) yield t).toSet
  /** baseType(a) = b if b ∈ parentType*(a) ∩ rootTypes */
  def baseType(a: api.Access[_ <: api.SkillObject]) = a.asInstanceOf[internal.StoragePool[_, _]].basePool.asInstanceOf[api.Access[_]]
  /** parentType+ */
  def superTypes(a: api.Access[_ <: api.SkillObject]): List[api.Access[_ <: api.SkillObject]] = {
    if (parentType.contains(a)) {
      val p = parentType(a)
      p :: superTypes(p)
    } else {
      Nil
    }
  }

  def objOfId[T <: B, B <: api.SkillObject](pool: api.Access[T], id: Int): api.SkillObject = {
    var o = if (id > 0) {
      val bp = pool.asInstanceOf[internal.StoragePool[T, B]].basePool
      if (id > bp.size) {
        throw new qq.util.binding.RestrictionException(s"No such object: $pool#$id")
      }
      bp(id - 1)
    } else if (id < 0) {
      createdObjects(-1 - id)
    } else {
      throw new qq.util.binding.RestrictionException(s"No such object: $pool#$id")
    }
    if (s(o.getTypeName) == pool || superTypes(s(o.getTypeName)).contains(pool)) {
      o
    } else {
      throw new qq.util.binding.RestrictionException(s"No such object: $pool#$id")
    }
  }

  def objOfId(x: String): api.SkillObject = {
    val xt = x.trim()
    if (x.trim().equals("(null)")) return null
    val xts = xt.split("#")
    if (xts.size != 2) throw new qq.util.binding.RestrictionException("format error, expected format type#number")
    val pn = xts(0)
    val id = try Integer.parseInt(xts(1)) catch {
      case _: java.lang.NumberFormatException ⇒ throw new qq.util.binding.RestrictionException("format error, expected format type#number")
    }
    val pool = try { s(pn) } catch {
      case e: java.util.NoSuchElementException ⇒
        throw new qq.util.binding.RestrictionException(s"Unknown type in $pn#$id")
    }
    objOfId(s(pn), id)
  }

  def idOfObj(o: api.SkillObject): String = {
    if (o == null) {
      "(null)"
    } else if (de.ust.skill.common.scala.hacks.GetSkillId(o) == -1) {
      s"${o.getTypeName}#${createdObjectId(o)}"
    } else {
      o.prettyString
    }
  }
  /** field definition and type that it belongs to for each field name */
  val fieldsByName: Map[String, Seq[Tuple2[api.Access[_], api.FieldDeclaration[_]]]] =
    (for (t ← s; f ← t.fields) yield (f.name, (t, f))).groupBy(_._1).mapValues(_.map(_._2))

  /* type and field settings */
  val typeSettings: Map[api.Access[_], TypeSettings[_]] =
    (for (t ← s) yield (t, new TypeSettings(t, this))).toMap

  val fieldSettings: Map[api.FieldDeclaration[_], FieldSettings[_, _]] =
    (for (t ← typeSettings.values; fd ← t.typ.fields) yield (fd, t.fields(fd).asInstanceOf[FieldSettings[_, api.SkillObject]])).toMap

}