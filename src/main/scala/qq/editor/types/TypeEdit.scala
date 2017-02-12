package qq.editor.types

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;

/** show the fields of one type and edit their profile settings. */
class TypeEdit(val page: qq.editor.types.TypePage,
               val skillType: api.Access[_ <: api.SkillObject]) extends qq.util.VScrollPane {

  /** displays one field. the field properties can be shown and hidden */
  class Field(val page: qq.editor.types.TypePage,
              val skillType: api.Access[_ <: api.SkillObject],
              val field: api.FieldDeclaration[_])
      extends qq.util.ExpandableNode({
        /* constants need a = value part */
        val constantValuePart = field.t match {
          case internal.fieldTypes.ConstantI8(i)  ⇒ " = " + i
          case internal.fieldTypes.ConstantI16(i) ⇒ " = " + i
          case internal.fieldTypes.ConstantI32(i) ⇒ " = " + i
          case internal.fieldTypes.ConstantI64(i) ⇒ " = " + i
          case _                                  ⇒ "" /* non-constants don't */
        }
        /* main part is type and name; FieldTypeControl makes user types in the type clickable */
        var typeAndName = qq.util.Swing.HBox(
          new FieldTypeControl(page, field.t),
          new swing.Label(" " + field.name + constantValuePart),
          swing.Swing.HGlue)
        /* when there are field restrictions, make a vbox with restrictions followed by type and name */
        var f2 = field.asInstanceOf[internal.FieldDeclaration[_, _]]
        if (f2.restrictions.size == 0) {
          typeAndName
        } else {
          new swing.BoxPanel(swing.Orientation.Vertical) {
            contents ++= f2.restrictions.map { x ⇒
              qq.util.Swing.HBox(
                new swing.Label(
                  /* non-null has no nice toString */
                  if (x.isInstanceOf[internal.restrictions.NonNull[_]]) {
                    "@NonNull"
                  } else {
                    "@" + x.toString
                  }),
                swing.Swing.HGlue)
            }
            contents += typeAndName
          }
        }
      },
        new FieldSettingsEdit(page.file, skillType, field)) {
  }

  private val inner = new swing.BoxPanel(swing.Orientation.Vertical) {
    def addFieldsOfType(τ: api.Access[_ <: api.SkillObject]): Unit = {
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
    for (
      rs ← empty.api.internal.FileParser.typeRestrictions.get(skillType.asInstanceOf[de.ust.skill.common.scala.internal.StoragePool[_,_]]);
      r ← rs
    ) {
      contents += qq.util.Swing.HBox(
        new swing.Label("@" + r.toString()) { foreground = java.awt.Color.red },
        swing.Swing.HGlue)
    }
    page.file.parentType.get(skillType) match {
      case Some(parentType) ⇒
        contents += qq.util.Swing.HBox(
          new TypeNameControl(page, skillType),
          new swing.Label(" : "),
          new TypeNameControl(page, parentType),
          new swing.Label(" {"),
          swing.Swing.HGlue)
      case None ⇒
        contents += qq.util.Swing.HBox(
          new TypeNameControl(page, skillType),
          new swing.Label(" {"),
          swing.Swing.HGlue)
    }
    addFieldsOfType(skillType)
    contents += qq.util.Swing.HBox(
      new swing.Label("}"),
      swing.Swing.HGlue)

  }

  contents = inner
}