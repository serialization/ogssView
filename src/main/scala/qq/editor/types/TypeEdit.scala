package qq.editor.types

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import qq.util.Swing.VBox
import qq.util.Swing.HBox
import swing.Swing.HGlue
import swing.Swing.RigidBox
import scala.swing.Dimension

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
        val ftc = new FieldTypeControl(page, field.t)
        val ftcw = ftc.preferredSize.width
        var typeAndName = if (ftcw > 40) {
        VBox(
            HBox( ftc,HGlue),
          HBox(RigidBox(new Dimension(40,0)),
              new swing.Label(" " + field.name + constantValuePart),
          HGlue)
)          
        } else {
        HBox(
          ftc,
          RigidBox(new Dimension(40 - ftcw, 0)),
          new swing.Label(" " + field.name + constantValuePart),
          HGlue)
        }
        /* when there are field restrictions, make a vbox with restrictions followed by type and name */
        var f2 = field.asInstanceOf[internal.FieldDeclaration[_, _]]
        if (f2.restrictions.size == 0) {
          typeAndName
        } else {
          new swing.BoxPanel(swing.Orientation.Vertical) {
            contents ++= f2.restrictions.map { x ⇒
              HBox(
                new swing.Label(
                  /* non-null has no nice toString */
                  if (x.isInstanceOf[internal.restrictions.NonNull[_]]) {
                    "@NonNull"
                  } else {
                    "@" + x.toString
                  }),
                HGlue)
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
        HBox(
          new swing.Label("" + τ.fields.length + " fields from "),
          new TypeNameControl(page, τ),
          HGlue)) {

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
      contents += HBox(
        new swing.Label("@" + r.toString()) { foreground = java.awt.Color.red },
        HGlue)
    }
    page.file.parentType.get(skillType) match {
      case Some(parentType) ⇒
        contents += HBox(
          new TypeNameControl(page, skillType),
          new swing.Label(" : "),
          new TypeNameControl(page, parentType),
          new swing.Label(" {"),
          HGlue)
      case None ⇒
        contents += HBox(
          new TypeNameControl(page, skillType),
          new swing.Label(" {"),
          HGlue)
    }
    addFieldsOfType(skillType)
    contents += HBox(
      new swing.Label("}"),
      HGlue)

  }

  contents = inner
}