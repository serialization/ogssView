package qq.editor

import de.ust.skill.common.scala.api;
import java.util.prefs.Preferences;
import de.ust.skill.common.scala.internal;
import de.ust.skill.common.scala.internal.fieldTypes;
import scala.collection.mutable;
import qq.util.Vector;
import qq.util.binding._;

class FieldSettings[T, U <: api.SkillObject](
    /** The field this is about */
    val field: api.FieldDeclaration[T],
    /** The type this belongs to */
    val containingType: TypeSettings[U]) extends PropertyOwner {

  private val prefs = Preferences.userRoot().node(s"/qq/skilledit/fieldsettings/${containingType.typ.name}/${field.name}");  
  
  /* defaults */
  private val (_hide, _inParent) = field.t.asInstanceOf[internal.fieldTypes.FieldType[_]] match {
    case u: fieldTypes.UserType[T]                   ⇒ (false, false)
    case c: fieldTypes.SingleBaseTypeContainer[_, T] ⇒ (false, false)
    case m: fieldTypes.MapType[_, _]                 ⇒ (false, false)
    case s: fieldTypes.StringType                    ⇒ (false, false)
    case fieldTypes.ConstantI8(_)                    ⇒ (true, true)
    case fieldTypes.ConstantI16(_)                   ⇒ (true, true)
    case fieldTypes.ConstantI32(_)                   ⇒ (true, true)
    case fieldTypes.ConstantI64(_)                   ⇒ (true, true)
    case fieldTypes.ConstantV64(_)                   ⇒ (true, true)
    case fieldTypes.BoolType                         ⇒ (false, true)
    case _: fieldTypes.AnnotationType                ⇒ (false, false)
    case fieldTypes.F32                              ⇒ (false, true)
    case fieldTypes.F64                              ⇒ (false, true)
    case fieldTypes.I8                               ⇒ (false, true)
    case fieldTypes.I16                              ⇒ (false, true)
    case fieldTypes.I32                              ⇒ (false, true)
    case fieldTypes.I64                              ⇒ (false, true)
    case fieldTypes.V64                              ⇒ (false, true)
  }

  /** User preference: hide this field */
  var prefHide: Property[Boolean] = new Property(this, "Always hide", prefs.getBoolean("hide",  _hide))
  /** User preference: hide if zero/empty/null */
  var prefHideNull: Property[Boolean] = new Property(this, "Hide empty/null values", prefs.getBoolean("hideEmpty",  true))
  /** User preference: show inside parent node */
  var prefShowInParent: Property[Boolean] = new Property(this, "Show inside parent", prefs.getBoolean("showInParent",  _inParent))
  /** User preference: keep edge direction stable */
  var prefFixedEdgeDirection: Property[Boolean] = new Property(this, "Keep edge direction stable", prefs.getBoolean("stableDirection",  false))
  /** User preference: edge direction (is changed by programme unless prefFixedEdgeDirection)*/
  var prefEdgeDirection: Property[Vector] = new Property(this, "Edge direction", Vector.parse(prefs.get("idealDirection","(0,0)"))) {
    restrictions += Restriction(_.abs <= 1, "length must not exceed one")
  }

  prefHide.onChange.strong += (prefs.putBoolean("hide", _))
  prefHideNull.onChange.strong += (prefs.putBoolean("hideEmpty", _))
  prefShowInParent.onChange.strong += (prefs.putBoolean("showInParent", _))
  prefFixedEdgeDirection.onChange.strong += {x =>
    prefs.putBoolean("stableDirection", x)
    if (x) prefs.put("idealDirection", prefEdgeDirection().toString)}
  prefEdgeDirection.onChange.strong += (x => if (prefFixedEdgeDirection()) prefs.put("idealDirection", x.toString))
  
  
  /** true if the value of this field in object o is null, empty collection, zero, or empty string */
  private def hasNullValueIn(o: api.SkillObject): Boolean = {
    field.t.asInstanceOf[internal.fieldTypes.FieldType[_]] match {
      case u: fieldTypes.UserType[T]                   ⇒ o.get(field) == null
      case c: fieldTypes.SingleBaseTypeContainer[_, T] ⇒ o.get(field).asInstanceOf[Iterable[T]].size == 0
      case m: fieldTypes.MapType[_, _]                 ⇒ o.get(field).asInstanceOf[mutable.HashMap[_, _]].size == 0
      case s: fieldTypes.StringType                    ⇒ o.get(field).asInstanceOf[String] == ""
      case fieldTypes.ConstantI8(_)                    ⇒ true
      case fieldTypes.ConstantI16(_)                   ⇒ true
      case fieldTypes.ConstantI32(_)                   ⇒ true
      case fieldTypes.ConstantI64(_)                   ⇒ true
      case fieldTypes.ConstantV64(_)                   ⇒ true
      case fieldTypes.BoolType                         ⇒ !o.get(field).asInstanceOf[Boolean]
      case _: fieldTypes.AnnotationType                ⇒ o.get(field) == null
      case fieldTypes.F32                              ⇒ o.get(field).asInstanceOf[Float] == 0.0f
      case fieldTypes.F64                              ⇒ o.get(field).asInstanceOf[Double] == 0.0
      case fieldTypes.I8                               ⇒ o.get(field).asInstanceOf[Byte] == 0
      case fieldTypes.I16                              ⇒ o.get(field).asInstanceOf[Short] == 0
      case fieldTypes.I32                              ⇒ o.get(field).asInstanceOf[Int] == 0
      case fieldTypes.I64                              ⇒ o.get(field).asInstanceOf[Long] == 0l
      case fieldTypes.V64                              ⇒ o.get(field).asInstanceOf[Long] == 0l
    }
  }

  /** returns true whether and how this field should be shown in object o*/
  def visibilityIn(o: api.SkillObject): FieldVisibility = {
    if (prefHide() /* user says hide */
      || (hasNullValueIn(o) && prefHideNull())) { /* or it;s null/empty…*/
      HideVisibility
    } else if (prefShowInParent()) {
      ShowInParentVisibility
    } else {
      ShowAsNodeVisibility
    }
  }
  
  
  // check whether it still exists after save
  var isDeleted = false
  def checkDeleted() : Unit = {
    def typeIsDeleted[T](τ: fieldTypes.FieldType[T]): Boolean = {
      τ match {
        case u: fieldTypes.UserType[_] => containingType.containingFile.typeSettings(u).isDeleted
        case c: fieldTypes.SingleBaseTypeContainer[_,_] => typeIsDeleted(c.groundType)
        case m: fieldTypes.MapType[k,v] => typeIsDeleted(m.keyType) || typeIsDeleted(m.valueType)
        case _ => false
      }
    }
    isDeleted = typeIsDeleted(field.t.asInstanceOf[fieldTypes.FieldType[_]])
  }

}