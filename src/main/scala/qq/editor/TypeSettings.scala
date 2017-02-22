package qq.editor

import de.ust.skill.common.scala.api;
import scala.collection.mutable;
/** Editor settings for a skill type.
 *  
 *  Mostly a container for field settings, probably.
 *  
 *  Note that somewhere near here profiles will have to enter */
class TypeSettings[T <: api.SkillObject](
  /** the skill type this is about */
  val typ: api.Access[T],
  /** the file this belongs to*/
  val containingFile: File
) {
  
  /**
   * Settings for the fields in this type.
   * 
   * only own field; behaviour of inherited fields is taken from parent
   */
  val fields: Map[api.FieldDeclaration[_], qq.editor.FieldSettings[_,T]] =
    (for (f <- typ.fields) yield (f, new FieldSettings(f, this))).toMap
  
  def parentTypeSettings: TypeSettings[_] = containingFile.typeSettings(
      containingFile.parentType(typ)
      )
      
  val expanded: mutable.HashSet[Seq[api.FieldDeclaration[_]]] = mutable.HashSet()
}