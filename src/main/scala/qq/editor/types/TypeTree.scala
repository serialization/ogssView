package qq.editor.types

import de.ust.skill.common.scala.api;
import de.ust.skill.common.scala.internal;
import scala.collection.mutable;
import qq.util.Swing.HBoxT


class TypeTree(val page: qq.editor.types.TypePage)
    extends swing.BoxPanel(swing.Orientation.Vertical) {

  background = java.awt.SystemColor.text
  
  /** node control by type for unfold/highlight */
  val nodes: mutable.Map[api.Access[_], TypeTreeNode] = new mutable.HashMap()

  class TypeTreeNode(τ: api.Access[_ <: api.SkillObject])
      extends qq.util.ExpandableNode(
        HBoxT(
          new TypeNameControl(page, τ),
          swing.Swing.HGlue,
          new qq.util.PlainLabel(" " + τ.asInstanceOf[internal.StoragePool[_, _]].staticInstances.size + " (" + τ.size + ")") {
            tooltip = "objects in this pool (including subpools)"
          }), true) {

    nodes(τ) = this

    if (page.file.childTypes.contains(τ)) {
      subPart = new swing.BoxPanel(swing.Orientation.Vertical) {
        background = java.awt.SystemColor.text
        contents ++= page.file.childTypes(τ).map(new TypeTreeNode(_))
      }
    }
  }
  val typeTree = new swing.BoxPanel(swing.Orientation.Vertical) {
    /* toSeq keeps the order stable; there's probably some concurrency going on in the
     * map of sets */
    background = java.awt.SystemColor.text
    contents ++= page.file.rootTypes.toSeq.map(new TypeTreeNode(_))
    contents += swing.Swing.VGlue
  }
  val scrollContainer = new qq.util.VScrollPane() {
    contents = typeTree
  }

  var selected: TypeTreeNode = null
  def select(τ: api.Access[_ <: api.SkillObject]): Unit = {
    if (selected != null) {
      selected.node.background = java.awt.SystemColor.text
    }
    
    /* expand all parents */
    for (x <- page.file.superTypes(τ)) nodes(x).expand
    
    val node = nodes(τ).node.peer
    val nodePos = javax.swing.SwingUtilities.convertRectangle(node.getParent, node.getBounds, typeTree.peer)
    typeTree.peer.scrollRectToVisible(nodePos)
    selected = nodes(τ)
      selected.node.background = java.awt.SystemColor.textHighlight
    
  }

  contents += scrollContainer
}