package qq.editor.types

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;

/** show the fields of one type and edit their profile settings. */
class TypeEdit(val page: qq.editor.types.TypePage,
               val skillType: api.Access[_]) extends qq.util.VScrollPane {

  /** displays one field. the field properties can be shown and hidden */
  class Field(val page: qq.editor.types.TypePage,
              val skillType: api.Access[_],
              val field: api.FieldDeclaration[_])
      extends qq.util.ExpandableNode({
        var typename = qq.util.Swing.HBox(
          new FieldTypeControl(page, field.t),
          new swing.Label(" " + field.name),
          swing.Swing.HGlue)
        var f2 = field.asInstanceOf[internal.FieldDeclaration[_, _]]
        if (f2.restrictions.size == 0) {
          typename
        } else {
          qq.util.Swing.VBox(
            qq.util.Swing.HBox(
              new swing.Label(f2.restrictions.map { x ⇒
                /* non-null has no nice toString */
                if (x.isInstanceOf[internal.restrictions.NonNull[_]]) {
                  "@NonNull"
                } else {
                  "@" + x.toString
                }
              }.mkString(", ")),
              swing.Swing.HGlue),
            typename)
        }
      },
        new FieldSettingsEdit(page.file, skillType, field)) {
  }

  private val inner = new swing.BoxPanel(swing.Orientation.Vertical) {
    def addFieldsOfType(τ: api.Access[_]): Unit = {
      if (page.file.parentType.contains(τ)) {
        addFieldsOfType(page.file.parentType(τ))
      }
      contents += new qq.util.ExpandableNode(
        qq.util.Swing.HBox(
          new swing.Label("" + τ.fields.length + " fields from "),
          new TypeNameControl(page, τ),
          swing.Swing.HGlue)) {

        if (τ.fields.length > 0) {
          subPart = new swing.BoxPanel(swing.Orientation.Vertical) {
            contents ++= τ.fields.map(x ⇒ new Field(page, skillType, x))
          }
          if (τ == skillType) expand
        }
      }
    }
    addFieldsOfType(skillType)
  }

  contents = inner
}