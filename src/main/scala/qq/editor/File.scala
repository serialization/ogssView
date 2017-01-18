package qq.editor

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import scala.collection.mutable;

/** Represents a skill file while opened in the editor */
class File(fn0: String) {
  /** The name of the skill file this is about (may change with save as) */
  var fileName = fn0

  private def pathAndName = {
    val dirnamepattern = "^(?:(.*)[\\\\/])?([^\\\\/]+)$".r
    fileName match { case dirnamepattern(d, n) ⇒ (d, n) }
  }

  def fileNameOnly: String = pathAndName._2
  /** Window title: filename, path, programme name, and a plus sign when modified (like gvim) */
  def windowTitle: String = {
    val (d, n) = pathAndName
    n + (if (isModified) " + " else "") + " (" + d + ") – SKilL edit"
  }
  /** the skill from fileName */
  val s: empty.api.SkillFile = empty.api.SkillFile.open(fn0)

  /** Undo/redo queue of the changes that were applied to the file after it was opened */
  val undoManager = new qq.util.UndoManager()

  /** true if file was modified, i.e. there is something to save */
  def isModified: Boolean = {
    /* TODO either we clear the undo queue when saving, or we need to add a dummy save event */
    undoManager.canUndo()
  }

  /**
   * event that fires whenever the file is edited
   *  TODO parameters of on edit event
   */
  val onEdit: qq.util.binding.Event[Unit] = new qq.util.binding.Event;

  /* some auxiliary functions about types */

  /** parentType(a) = b if a is directly derived from b, a ∉ Dom(parentType) if a is a root type */
  val parentType: Map[api.Access[_], api.Access[_]] =
    (for (t ← s; sn ← t.superName) yield (t, s(sn).asInstanceOf[api.Access[_ <: api.SkillObject]])).toMap
  /** a ∈ childTypes(b) if a is directly derived from b */
  val childTypes: Map[api.Access[_], Seq[api.Access[_]]] =
    s.groupBy(parentType.getOrElse(_, null))
  /** a ∈ rootTypes if ~(∃x)(x = parentType(a)) */
  val rootTypes: Seq[api.Access[_]] = (for (t ← s if !(parentType contains t)) yield t)
  /** baseType(a) = b if b ∈ parentType*(a) ∩ rootTypes */
  def baseType(a: api.Access[_]) = a.asInstanceOf[internal.StoragePool[_, _]].basePool.asInstanceOf[api.Access[_]]
  /** parentType+ */
  def superTypes(a: api.Access[_]): List[api.Access[_]] = {
    if (parentType.contains(a)) {
      val p = parentType(a)
      p :: superTypes(p)
    } else {
      Nil
    }
  }
  /* type and field settings */
  val typeSettings: Map[api.Access[_], TypeSettings[_]] =
    (for (t ← s) yield (t, new TypeSettings(t, this))).toMap

}