package qq.graph

import qq.util.HtmlEscape
import de.ust.skill.common.scala.api
import java.awt.SystemColor
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

/** common Ui elements for the graph nodes*/
object UIElements {
  
  def value[T](g: Graph, v: T) = {
    val asString = if (v == null) "(null)" else v.toString()
    val shortString = if (asString.length() > 21) {
      asString.take(10) + "…" + asString.takeRight(10)
    } else {
      asString
    }
    new swing.Label(HtmlEscape(shortString)) {
      border = swing.Swing.LineBorder(SystemColor.textText)
      tooltip = HtmlEscape(asString) 
    }
  }
  def nil(g: Graph) = new swing.Label("⊥")
  def expandable(g: Graph, node: AbstractNode, text: String) = {
    val button = new qq.util.PlainButton(
      new swing.Action(text) {
        override def apply() = {
          g.viewer.expandCollapse(node)
        }
      })

    if (g.viewer.expandedNodes.contains(node)) {
      button.border = swing.Swing.LineBorder(SystemColor.textText, if (g.viewer.root == node) 2 else 1)
    }

    button
  }
  def skillObject(g: Graph, node: AbstractNode, o: api.SkillObject) = {
    val base = expandable(g, node, g.viewer.page.file.idOfObj(o))
    base.peer.setComponentPopupMenu(qq.editor.objects.ObjectContextMenu(o, g.viewer.page).peer)
    base
  }
  def list[E, C[E] <: Buffer[E]](g: Graph, node: AbstractNode, o: api.SkillObject, f: api.FieldDeclaration[C[E]]) = {
    import de.ust.skill.common.scala.internal.fieldTypes._
   
    val text = (if (f.t.isInstanceOf[ListType[_]]) "list of " else "array of ") + o.get(f).size
    val base = expandable(g, node, text)
    /* TODO context menu */
    base
    
  }
  def set[E, C[E] <: HashSet[E]](g: Graph, node: AbstractNode, o: api.SkillObject, f: api.FieldDeclaration[C[E]]) = {
    val base = expandable(g, node, "set of "+o.get(f).size)
    /* TODO context menu */
    base
    
  }
  def map[K, V, C[K,V] <: HashMap[K,V]](g: Graph, node: AbstractNode, o: api.SkillObject, f: api.FieldDeclaration[C[K,V]]) = {
    import qq.util.FlattenedMap
    import de.ust.skill.common.scala.internal.fieldTypes._
   
    val base = expandable(g, node, "map of "+FlattenedMap.size(o.get(f),f.t.asInstanceOf[MapType[K,V]]))
    /* TODO context menu */
    base
    
  }

}