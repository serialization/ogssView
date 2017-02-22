package qq.editor.objects

import de.ust.skill.common.scala.api
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

/** Show obj and neighbourhood as graph */
class ObjectGraph[O <: api.SkillObject](
  val page: ObjectPage,
  val obj: O)
    extends swing.Panel() {
  peer.setLayout(null)
  peer.setMinimumSize(new swing.Dimension(50, 50))
  peer.setPreferredSize(new swing.Dimension(50, 50))
  peer.setSize(new swing.Dimension(50, 50))

  private var graph = new qq.graph.Graph(page.file, this, page.settings.graphLayout)

  private val root = new qq.graph.SkillObjectNode(obj)
  private val visibleNodes = new HashSet[qq.graph.AbstractNode]
  private val expandedNodes = new HashSet[qq.graph.AbstractNode]
  private val pathsToNode = new HashMap[qq.graph.AbstractNode, Set[Seq[api.FieldDeclaration[_]]]]

  private def expandPrefs = page.file.typeSettings(page.file.s(obj.getTypeName)).expanded
  def expandCollapse(n: qq.graph.AbstractNode) {
    if (expandedNodes.contains(n)) {
      expandPrefs.retain(!pathsToNode(n).contains(_))
    } else {
      val spl = pathsToNode(n).map(_.size).min
      expandPrefs ++= pathsToNode(n).filter(_.size == spl)
      println(expandPrefs)
    }
    updateLayout
    repaint
  }

  def updateLayout: Unit = {
    visibleNodes.clear()
    expandedNodes.clear()
    pathsToNode.clear()
    visibleNodes += root
    expandedNodes += root
    pathsToNode(root) = Set(Seq())
    // follow paths to expanded nodes
    for (path ← expandPrefs) {
      def expandPath(o: api.SkillObject,
                     pathToO: Seq[api.FieldDeclaration[_]],
                     pathToDo: Seq[api.FieldDeclaration[_]]): Boolean = {
        if (o != null) {
          if (pathToDo.size == 0) {
            val node = new qq.graph.SkillObjectNode(o)
            visibleNodes += node
            expandedNodes += node
              pathsToNode.getOrElseUpdate(node, Set())
            pathsToNode(node) += pathToO
            true
          } else {
            val field = pathToDo.head
            val p = o.get(field).asInstanceOf[api.SkillObject]
            if (expandPath(p, pathToO :+ field, pathToDo.tail)) {
              val node = new qq.graph.SkillObjectNode(o)
              visibleNodes += node
              pathsToNode.getOrElseUpdate(node, Set())
              pathsToNode(node) += pathToO
              true
            } else {
              false
            }
          }
        } else {
          false
        }
      }
      expandPath(obj, Seq(), path)
    }
    // go to fields of expanded nodes

    graph = new qq.graph.Graph(page.file, this, page.settings.graphLayout)

    for (node ← visibleNodes) graph.addNode(node)
    for (node ← expandedNodes) {
      node.getOutEdge(page.file).foreach { e ⇒
        graph.addEdge(e)
        e match {
          case e: qq.graph.SkillFieldEdge[_] ⇒
            val to = e.getTo
            pathsToNode.getOrElseUpdate(to, Set())
               pathsToNode(to) ++= pathsToNode(node).map(_ :+ e.field)
          case _ ⇒ ()
        }
      }
    }

    // fill in all edges
    for (node ← graph.nodes.keys) {
      for (edge ← node.getOutEdge(page.file)) {
        if (graph.nodes.contains(edge.getTo)) graph.addEdge(edge)
      }
    }

    // clamp origin to centre
    graph.nodes(root).clampedAt = Some(qq.util.Vector(size) / 2)
    graph.placeNodes(size)
    peer.removeAll()
    graph.nodes.values.foreach { x ⇒
      x.edgesOut.values.foreach { y ⇒ peer.add(y.uiElement.peer) }
      peer.add(x.uiElement.peer)
    }
  }

  override def paintComponent(g: swing.Graphics2D) {

    // Start by erasing this Canvas
    g.setColor(javax.swing.UIManager.getColor("Panel.background"))
    g.clearRect(0, 0, bounds.width, bounds.height)
    g.fillRect(0, 0, bounds.width, bounds.height)
    g.setColor(javax.swing.UIManager.getColor("Label.foreground"))

    for (c ← graph.nodes.values) {
      c.uiElement.peer.setSize(c.uiElement.preferredSize)
      c.uiElement.peer.setLocation(c.left, c.top)
      c.uiElement.repaint()
      g.drawString((c.energy / c.degree).toString(), c.pos.x.toInt, c.pos.y.toInt - 10)
      for (e ← c.edgesOut.values) {
        e.draw(g)
        e.updateToolTop
        e.uiElement.peer.setSize(e.uiElement.preferredSize)
        val x = (c.pos + e.to.pos) / 2
        e.uiElement.peer.setLocation(x.x.toInt, x.y.toInt)
        e.uiElement.repaint()
      }
    }
    for (i ← 0.until(graph.energyOfStep.size)) {
      g.drawString("x", 10 + i, 10 + 10 * graph.energyOfStep(i) / graph.nodes.size)
    }
    for (i ← 0.until(graph.stepOfStep.size)) {
      g.drawString("o", 10 + i, 10 + graph.stepOfStep(i))
    }

  }

  listenTo(this)
  reactions += {
    case e: swing.event.UIElementResized ⇒
      println("res " + this.bounds)
      updateLayout
      repaint

  }
  // don't updateLayout here, do it outside after added to container, when size is known
}
