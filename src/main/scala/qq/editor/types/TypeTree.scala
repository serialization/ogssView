package qq.editor.types

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;

class TypeTree(val page: qq.editor.types.TypePage)
    extends swing.BoxPanel(swing.Orientation.Vertical) {
  class TypeTreeNode(τ: api.Access[_])
      extends swing.BoxPanel(swing.Orientation.Vertical) {
    val erbtn = new qq.util.PlainButton(swing.Action("") {}) {
      this.preferredSize = new java.awt.Dimension(15, 15)
      this.focusable = false
    }
    contents += new swing.BoxPanel(swing.Orientation.Horizontal) {
      contents += erbtn
      contents += new TypeNameControl(page, τ)
      contents += swing.Swing.HGlue
      contents += new swing.Label(" " + τ.asInstanceOf[internal.StoragePool[_, _]].staticInstances.size + " (" + τ.size + ")") {
        tooltip = "objects in this pool (including subpools)"
      }

    }
    if (page.file.childTypes.contains(τ)) {
      val lowerPart = new swing.BoxPanel(swing.Orientation.Horizontal) {
        visible = false
        contents += swing.Swing.RigidBox(new java.awt.Dimension(15, 0))
        contents += new swing.BoxPanel(swing.Orientation.Vertical) {
          contents ++= page.file.childTypes(τ).map(new TypeTreeNode(_))
        }
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
      erbtn.action = expand
      erbtn.text = ""
      erbtn.icon = javax.swing.UIManager.getIcon("Tree.collapsedIcon")
      erbtn.focusable = true
    }
  }
  contents ++= page.file.rootTypes.map(new TypeTreeNode(_))
  contents += swing.Swing.VGlue
}