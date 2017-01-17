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
   * NB: all fields; default behaviour is to inherit the settings from the field
   * in the parent type, but one can change the settings in the sub type
   */
  val fields: Map[api.FieldDeclaration[_], qq.editor.FieldSettings[_,T]] =
    (for (f <- typ.allFields) yield (f, new FieldSettings(f, this))).toMap
  
  def parentTypeSettings: TypeSettings[_] = containingFile.typeSettings(
      containingFile.parentType(typ)
      )
}