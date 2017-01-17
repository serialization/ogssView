package qq.editor.types

import de.ust.skill.common.scala.api;

/** show the fields of one type and edit their profile settings. */
class TypeEdit(val page: qq.editor.types.TypePage,
               val skillType: api.Access[_]) extends qq.util.VScrollPane {

  /** displays one field. the field properties can be shown and hidden */
  class Field(val page: qq.editor.types.TypePage,
              val skillType: api.Access[_],
              val field: api.FieldDeclaration[_])
      extends swing.BoxPanel(swing.Orientation.Vertical) {

    val erbtn = new qq.util.PlainButton(swing.Action("") {}) {
      this.preferredSize = new java.awt.Dimension(15, 15)
    }

    contents += qq.util.Swing.HBox(
      erbtn,
      new FieldTypeControl(page, field.t),
      new swing.Label(" " + field.name),
      swing.Swing.HGlue)

    val fieldProperties = qq.util.Swing.HBox(
      swing.Swing.RigidBox(new java.awt.Dimension(15, 0)),
      new FieldSettingsEdit(page.file, skillType, field))
    fieldProperties.visible = false

    var reduce: swing.Action = null
    val expand: swing.Action = swing.Action("expand") {
      erbtn.action = reduce
      erbtn.text = ""
      erbtn.icon = javax.swing.UIManager.getIcon("Tree.expandedIcon")
      fieldProperties.visible = true
    }
    reduce = swing.Action("reduce") {
      erbtn.action = expand
      erbtn.text = ""
      erbtn.icon = javax.swing.UIManager.getIcon("Tree.collapsedIcon")
      fieldProperties.visible = false
    }
    erbtn.action = expand
    erbtn.text = ""
    erbtn.icon = javax.swing.UIManager.getIcon("Tree.collapsedIcon")
    contents += fieldProperties
  }

  private val inner = new swing.BoxPanel(swing.Orientation.Vertical) {
    def addFieldsOfType(τ: api.Access[_]): Unit = {
      if (page.file.parentType.contains(τ)) {
        addFieldsOfType(page.file.parentType(τ))
      }
      contents += new swing.BoxPanel(swing.Orientation.Vertical) {

        val erbtn = new qq.util.PlainButton(swing.Action("") {}) {
          this.preferredSize = new java.awt.Dimension(15, 15)
          this.focusable = false
        }
        contents += new swing.BoxPanel(swing.Orientation.Horizontal) {
          contents += erbtn
          contents += new swing.Label("" + τ.fields.length + " fields from ")
          contents += new TypeNameControl(page, τ)
          contents += swing.Swing.HGlue
        }
        if (τ.fields.length > 0) {
          val lowerPart = new swing.BoxPanel(swing.Orientation.Horizontal) {
            visible = τ == skillType
            contents += swing.Swing.RigidBox(new java.awt.Dimension(15, 0))
            contents += new swing.BoxPanel(swing.Orientation.Vertical) {
              contents ++= τ.fields.map(x ⇒ new Field(page, skillType, x))
            }
            contents += swing.Swing.HGlue
          }
          contents += lowerPart
          var reduce: swing.Action = null
          val expand: swing.Action = swing.Action("expand") {
            erbtn.action = reduce
            erbtn.text = ""
            erbtn.icon = javax.swing.UIManager.getIcon("Tree.expandedIcon")
            lowerPart.visible = true
          }
          reduce = swing.Action("reduce") {
            erbtn.action = expand
            erbtn.text = ""
            erbtn.icon = javax.swing.UIManager.getIcon("Tree.collapsedIcon")
            lowerPart.visible = false
          }
          if (τ == skillType) expand() else reduce()
        }
      }
    }
    addFieldsOfType(skillType)
  }

  contents = inner
}