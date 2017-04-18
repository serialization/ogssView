package qq.graph

import qq.util.HtmlEscape
import de.ust.skill.common.scala.api
import java.awt.SystemColor
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import swing.Swing.LineBorder
import swing.Swing.CompoundBorder
import swing.Swing.EmptyBorder

/** common Ui elements for the graph nodes*/
object UIElements {

  def valueShortString[T](v: T) = {
    val asString = if (v == null) "⊥" else v.toString()
    val n = qq.editor.Main.preferences.graphMaxStringLength()
    if (asString.length() > n) {
      asString.take((n - 1) / 2) + "…" + asString.takeRight(n / 2)
    } else {
      asString
    }
  }
  def value[T](g: Graph, n: AbstractNode, v: T) = {
    val asString = if (v == null) "⊥" else v.toString()
    new qq.util.PlainLabel(HtmlEscape(n.name(g))) {
      tooltip = HtmlEscape(asString)
    }
  }
  def nil(g: Graph) = new swing.Label("⊥")
  /** border width of node in graph (root bold is set in preferences) */
  private def borderWidth(g: Graph, n: AbstractNode) = {
    if (g.viewer.root == n && g.properties.rootBold()) 2 else 1
  }
  def skillObject(g: Graph, node: AbstractNode, o: api.SkillObject) = {
    val button = new qq.util.PlainButton(
      new swing.Action(node.name(g)) {
        override def apply() = {
          g.viewer.expandCollapse(node)
        }
      })
    button.peer.setComponentPopupMenu(qq.editor.objects.ObjectContextMenu(o, g.viewer.page).peer)
    if (g.viewer.expandedNodes.contains(node)) {
      val τ = g.file.s(o.getTypeName)
      val innerFields = for (f ← τ.allFields.toSeq if !g.file.fieldPreferences(f).isDeleted && g.file.fieldPreferences(f).visibilityIn(o).showInParent) yield {
        val text = valueShortString(o.get(f))
        qq.util.Swing.HBoxT(new qq.util.PlainLabel(f.name + " = " + text), swing.Swing.HGlue)
      }
      if (innerFields.size == 0) {
        button.border = CompoundBorder(
          LineBorder(SystemColor.textText, borderWidth(g, node)),
          EmptyBorder(0, 2, 0, 2))
        button
      } else {
        val head = qq.util.Swing.HBoxT(button, swing.Swing.HGlue)
        head.border = CompoundBorder(
          swing.Swing.MatteBorder(0, 0, borderWidth(g, node), 0, SystemColor.textText),
          EmptyBorder(0, 2, 0, 2))
        val whole = qq.util.Swing.VBoxT((head +: innerFields): _*)
        whole.border = LineBorder(SystemColor.textText, borderWidth(g, node))
        whole
      }
    } else {
      button
    }
  }
  private def container(g: Graph, node: AbstractNode, o: api.SkillObject, f: api.FieldDeclaration[_], small: Boolean) = {

    val lm = new swing.Action("list members") {
      override def apply() {
        val page = qq.editor.Main.newObjectTab()
        page.find(g.file.idOfObj(o)+" "+f.name + " ?member")
        page.show()
        }
    }
    
    val button = new qq.util.PlainButton(
      new swing.Action(node.name(g)) {
        override def apply() = {
          if (small) {
          g.viewer.expandCollapse(node)
          } else {
            lm()
          }
        }
      }) {
      peer.setComponentPopupMenu(new swing.PopupMenu(){contents += new swing.MenuItem(lm)}.peer)
    }

    button.border = CompoundBorder(
      LineBorder(java.awt.SystemColor.textText),
      CompoundBorder(
        LineBorder(java.awt.SystemColor.text),
        CompoundBorder(
          LineBorder(java.awt.SystemColor.textText),
          EmptyBorder(0, 2, 0, 2))))
    button
  }

  def list[E, C[E] <: Buffer[E]](g: Graph, node: AbstractNode, o: api.SkillObject, f: api.FieldDeclaration[C[E]]) = {
    val base = container(g, node, o, f, o.get(f).size <= g.viewer.page.settings.graphCollectionSmall())
    base
  }
  def set[E, C[E] <: HashSet[E]](g: Graph, node: AbstractNode, o: api.SkillObject, f: api.FieldDeclaration[C[E]]) = {
    val base = container(g, node, o, f, o.get(f).size <= g.viewer.page.settings.graphCollectionSmall())
    base
  }
  def map[K, V, C[K, V] <: HashMap[K, V]](g: Graph, node: AbstractNode, o: api.SkillObject, f: api.FieldDeclaration[C[K, V]]) = {
    import qq.util.FlattenedMap.size
    val base = container(g, node, o, f, size(o.get(f), f.t.asInstanceOf[de.ust.skill.common.scala.internal.fieldTypes.MapType[K,V]]) <= g.viewer.page.settings.graphCollectionSmall())
    base
  }
}