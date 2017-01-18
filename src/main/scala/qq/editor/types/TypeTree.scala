package qq.editor.types

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import scala.collection.mutable;

class TypeTree(val page: qq.editor.types.TypePage)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  val nodes: mutable.Map[api.Access[_], TypeTreeNode] = new mutable.HashMap()

  class TypeTreeNode(τ: api.Access[_])
      extends qq.util.ExpandableNode(
        qq.util.Swing.HBox(
          new TypeNameControl(page, τ),
          swing.Swing.HGlue,
          new swing.Label(" " + τ.asInstanceOf[internal.StoragePool[_, _]].staticInstances.size + " (" + τ.size + ")") {
            tooltip = "objects in this pool (including subpools)"
          })) {

    nodes(τ) = this

    if (page.file.childTypes.contains(τ)) {
      subPart = new swing.BoxPanel(swing.Orientation.Vertical) {
        contents ++= page.file.childTypes(τ).map(new TypeTreeNode(_))
      }
    }
  }
  val typeTree = new swing.BoxPanel(swing.Orientation.Vertical) {
    contents ++= page.file.rootTypes.map(new TypeTreeNode(_))
    contents += swing.Swing.VGlue
  }
  val scrollContainer = new qq.util.VScrollPane() {
    contents = typeTree
  }

  def select(τ: api.Access[_]): Unit = {
    /* expand all parents */
    for (x <- page.file.superTypes(τ)) nodes(x).expand
    
    val node = nodes(τ).node.peer
    val nodePos = javax.swing.SwingUtilities.convertRectangle(node.getParent, node.getBounds, typeTree.peer)
    typeTree.peer.scrollRectToVisible(nodePos)
  }

  contents += scrollContainer
}