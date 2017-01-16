package qq.editor

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import de.ust.skill.common.scala.internal.fieldTypes;
import scala.collection.mutable;
import com.github.jpbetz.subspace._;

class FieldSettings[T, U <: api.SkillObject](
    /** The field this is about */
    val field: api.FieldDeclaration[T],
    /** The type this belongs to */
    val containingType: TypeSettings[U]) {

  /** User preference: hide this field */
  var prefHide: Boolean = false
  /** User preference: show this field */
  var prefShow: Boolean = false
  /** User preference: show also if null/zero (requires prefShow )*/
  var prefShowNull: Boolean = false
  /** User preference: show inside parent node */
  var prefShowInParent: Boolean = false
  /** User preference: keep edge direction stable */
  var prefFixedEdgeDirection: Boolean = false
  /** User preference: edge direction (is changed by programme unless prefFixedEdgeDirection)*/
  var prefEdgeDirection: Vector2 = Vector2(0.0f, 0.0f)

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
      case fieldTypes.F32                              ⇒ o.get(field).asInstanceOf[Float] == 0.0
      case fieldTypes.F64                              ⇒ o.get(field).asInstanceOf[Double] == 0.0
      case fieldTypes.I8                               ⇒ o.get(field).asInstanceOf[Byte] == 0
      case fieldTypes.I16                              ⇒ o.get(field).asInstanceOf[Short] == 0
      case fieldTypes.I32                              ⇒ o.get(field).asInstanceOf[Int] == 0
      case fieldTypes.I64                              ⇒ o.get(field).asInstanceOf[Long] == 0
      case fieldTypes.V64                              ⇒ o.get(field).asInstanceOf[Long] == 0
    }
  }

  /** returns true whether and how this field should be shown in object o*/
  def visibilityIn(o: api.SkillObject): FieldVisibility = {
    if (prefHide /* user says hide */
      || (hasNullValueIn(o) && !prefShowNull)) { /* or it;s null/empty…*/
      HideVisibility
    } else if (prefShow) { /* user says show */
      if (prefShowInParent) {
        ShowInParentVisibility
      } else {
        ShowAsNodeVisibility
      }
    } else if (!(containingType.typ.fields.contains(field))) { /* default 1: inherit from supertype */
      containingType.parentTypeSettings.fields(field).visibilityIn(o)
    } else { /* default 2: decide ourselves */
      field.t.asInstanceOf[internal.fieldTypes.FieldType[_]] match {
        case u: fieldTypes.UserType[T]                   ⇒ ShowAsNodeVisibility
        case c: fieldTypes.SingleBaseTypeContainer[_, T] ⇒ ShowAsNodeVisibility
        case m: fieldTypes.MapType[_, _]                 ⇒ ShowAsNodeVisibility
        case s: fieldTypes.StringType                    ⇒ ShowAsNodeVisibility
        case fieldTypes.ConstantI8(_)                    ⇒ ShowInParentVisibility
        case fieldTypes.ConstantI16(_)                   ⇒ ShowInParentVisibility
        case fieldTypes.ConstantI32(_)                   ⇒ ShowInParentVisibility
        case fieldTypes.ConstantI64(_)                   ⇒ ShowInParentVisibility
        case fieldTypes.ConstantV64(_)                   ⇒ ShowInParentVisibility
        case fieldTypes.BoolType                         ⇒ ShowInParentVisibility
        case _: fieldTypes.AnnotationType                ⇒ ShowAsNodeVisibility
        case fieldTypes.F32                              ⇒ ShowInParentVisibility
        case fieldTypes.F64                              ⇒ ShowInParentVisibility
        case fieldTypes.I8                               ⇒ ShowInParentVisibility
        case fieldTypes.I16                              ⇒ ShowInParentVisibility
        case fieldTypes.I32                              ⇒ ShowInParentVisibility
        case fieldTypes.I64                              ⇒ ShowInParentVisibility
        case fieldTypes.V64                              ⇒ ShowInParentVisibility
      }
    }
  }

}