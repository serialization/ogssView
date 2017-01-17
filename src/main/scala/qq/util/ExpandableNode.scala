package qq.util

/**
 * A node that can be expanded and collapsed: useful for building trees
 */
class ExpandableNode(val node: swing.Component)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  def this(node0: swing.Component, subPart0: swing.Component, expanded: Boolean = false) = {
    this(node0)
    subPart = subPart0
    if (expanded) expand
  }

  private var subPart_ : swing.Component = null

  def subPart: swing.Component = subPart_
  def subPart_=(x: swing.Component): Unit = {
    subPart_ = x
    subBox.contents.clear     
    subBox.contents += spacer
    if (x != null) {
      subBox.contents += subPart
    } else {
      collapse
      erbtn.action = nullAction
    }
  }

  def expand(): Unit = {
    erbtn.action = if (subPart != null) collapseAction else nullAction
    erbtn.text = ""
    subBox.visible = subPart != null

  }
  def collapse(): Unit = {
    erbtn.action = if (subPart != null) expandAction else nullAction
    erbtn.text = ""
    subBox.visible = false
  }
  val expandAction: swing.Action = new swing.Action("expand") {
    icon = javax.swing.UIManager.getIcon("Tree.collapsedIcon")
    override def apply(): Unit = {
      expand
    }
  }
  val collapseAction: swing.Action = new swing.Action("collapse") {
    icon = javax.swing.UIManager.getIcon("Tree.expandedIcon")
    override def apply(): Unit = {
      expand
    }
  }
  private val nullAction = swing.Action("") {}

  private val erbtn = new qq.util.PlainButton() {
    this.preferredSize = new java.awt.Dimension(15, 15)
    this.focusable = false
  }

  contents += Swing.HBox(erbtn, node)
  val spacer = swing.Swing.RigidBox(new java.awt.Dimension(15, 0))
  val subBox = Swing.HBox(spacer)

}
