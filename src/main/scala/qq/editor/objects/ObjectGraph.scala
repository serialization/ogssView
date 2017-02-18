package qq.editor.objects

import de.ust.skill.common.scala.api

class ObjectGraph[O <: api.SkillObject](
  val page: ObjectPage,
  val obj: O)
    extends swing.Panel() {
  peer.setLayout(null)
  peer.setMinimumSize(new swing.Dimension(50, 50))
  peer.setPreferredSize(new swing.Dimension(50, 50))
  peer.setSize(new swing.Dimension(50, 50))

  private val graph = new qq.graph.Graph(page.file, page, page.settings.graphLayout)

  private val origin = new qq.graph.SkillObjectNode(obj)
  graph.addNode(origin)
  origin.getOutEdge(page.file).seq.foreach(graph.addEdge(_))
  val foo = graph.nodes.values.seq.toSeq.seq
  foo.foreach(x=>x.data.getOutEdge(page.file).foreach(graph.addEdge(_)))

  private def updateLayout: Unit = {
    graph.placeNodes(size)
    peer.removeAll()
    graph.nodes.values.foreach(x ⇒ peer.add(x.uiElement.peer))
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
      val xx = c.pos + c.force
      g.drawLine(c.pos.x.toInt, c.pos.y.toInt, xx.x.toInt, xx.y.toInt)
      for (e ← c.edgesOut.values) {
        e.draw(g)
      }
    }
    for (i ← 0.until(graph.energyOfStep.size)) {
      g.drawString("x", 10 + i, 10 + 10 * graph.energyOfStep(i)/graph.nodes.size)
    }
  }

  listenTo(this)
  reactions += {
    case e: swing.event.UIElementResized ⇒
      updateLayout
      revalidate
  }
  updateLayout
  
}