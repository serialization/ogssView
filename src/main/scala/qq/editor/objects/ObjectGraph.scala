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
  background = java.awt.SystemColor.text

  private var graph = new qq.graph.Graph(page.file, this, page.settings.graphLayout)

  val root = new qq.graph.SkillObjectNode(obj)
  val visibleNodes = new HashSet[qq.graph.AbstractNode]
  val expandedNodes = new HashSet[qq.graph.AbstractNode]
  private val pathsToNode = new HashMap[qq.graph.AbstractNode, Set[Seq[api.FieldDeclaration[_]]]]

  // TODO save at the type the field belongs to
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
    val t0 = System.nanoTime()
    visibleNodes.clear()
    expandedNodes.clear()
    pathsToNode.clear()
    visibleNodes += root
    expandedNodes += root
    pathsToNode(root) = Set(Seq())
    val t1 = System.nanoTime()
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
            val node = new qq.graph.SkillObjectNode(o)
            val toNext = node.edgeForField(graph.file, field)
            toNext match {
              case Some(edge) ⇒
                edge.getTo match {
                  case qq.graph.SkillObjectNode(p) ⇒
                    if (expandPath(p, pathToO :+ field, pathToDo.tail)) {
                      visibleNodes += node
                      pathsToNode.getOrElseUpdate(node, Set())
                      pathsToNode(node) += pathToO
                      true
                    } else {
                      false
                    }
                  case next ⇒
                    // last step can lead to a collections (size == 1)
                    visibleNodes += node
                    pathsToNode.getOrElseUpdate(node, Set())
                    pathsToNode(node) += pathToO
                    visibleNodes += next
                    expandedNodes += next
                    pathsToNode.getOrElseUpdate(next, Set())
                    pathsToNode(next) += pathToO :+ field
                    true

                }
              case None ⇒ false // path leads nowhere (null or hidden or wrong type)
            }
          }
        } else {
          false
        }
      }
      expandPath(obj, Seq(), path)
    }
    // go to fields of expanded nodes
    val t2 = System.nanoTime()
    graph = new qq.graph.Graph(page.file, this, page.settings.graphLayout)

    if (page.settings.graphLayout.rootAtCentre()) {
      // clamp root to centre
      graph.addNode(root)
      graph.nodes(root).clampedAt = Some(qq.util.Vector(size) / 2)
      graph.nodes(root).move(0.0f)
    }
    for (node ← visibleNodes) graph.addNode(node)
    for (node ← expandedNodes) {
      node.getOutEdge(page.file).foreach { e ⇒
        graph.addEdge(e)
        e match {
          case e: qq.graph.SkillFieldEdge[_] ⇒
            val to = e.getTo
            pathsToNode.getOrElseUpdate(to, Set())
            pathsToNode(to) ++= pathsToNode(node).map(_ :+ e.field)
          case _ ⇒ println("huh?" + e)
        }
      }
    }

    // fill in all edges
    for (node ← graph.nodes.keys) {
      for (edge ← node.getOutEdge(page.file)) {
        if (graph.nodes.contains(edge.getTo)) graph.addEdge(edge)
      }
    }

    val t3 = System.nanoTime()
    graph.placeNodes(size)
    val t4 = System.nanoTime()
    peer.removeAll()
    graph.nodes.values.foreach { x ⇒
      x.edgesOut.values.foreach { y ⇒ 
        peer.add(y.uiElementAtFrom.peer)
        peer.add(y.uiElementAtTo.peer)
        }
      peer.add(x.uiElement.peer)
    }
    val t5 = System.nanoTime()
    println(s"${graph.nodes.size} nodes ${graph.nodes.values.map(_.edgesOut.size).sum} edges follow paths: ${t2 - t1} ns, place: ${(t4 - t3)/1E6} ms, total ${(t5 - t0)/1E6} ms E=${graph.energyOfStep.last} stepsize=${graph.stepOfStep.last}")

  }

  override def paintComponent(g: swing.Graphics2D) {
    super.paintComponent(g)

    // Start by erasing this Canvas
    g.setColor(java.awt.SystemColor.text)
    g.clearRect(0, 0, bounds.width, bounds.height)
    g.fillRect(0, 0, bounds.width, bounds.height)
    g.setColor(java.awt.SystemColor.textText)

    for (c ← graph.nodes.values) {
      c.uiElement.peer.setSize(c.uiElement.preferredSize)
      c.uiElement.peer.setLocation(c.left, c.top)
      c.uiElement.peer.revalidate()
      //g.drawString((c.energy / c.degree).toString(), c.pos.x.toInt, c.pos.y.toInt - 10)
      for (e ← c.edgesOut.values) {
        e.draw(g)
        e.updateToolTop
      }
    }
   // for (i ← 0.until(graph.energyOfStep.size)) {
   //   g.drawString("x", 10 + i, 10 + 10 * graph.energyOfStep(i) / graph.nodes.size)
   // }
   // for (i ← 0.until(graph.stepOfStep.size)) {
   //   g.drawString("o", 10 + i, 10 + graph.stepOfStep(i))
   // }
  }

  listenTo(this)
  reactions += {
    case e: swing.event.UIElementResized ⇒
      updateLayout
      repaint

  }  
  
  // simple solution to dealing with edits: rebuilt when any object we show changes 
  val onEdit = (ed: qq.editor.Edit[_]) => { if (visibleNodes.contains(new qq.graph.SkillObjectNode(ed.obj.asInstanceOf[api.SkillObject]))) {
    updateLayout
    repaint
  }}
  
  page.file.onEdit.weak += onEdit

  // don't updateLayout when created, it is done in resize, when the size is known

}
