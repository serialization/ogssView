package qq.graph

import scala.collection.mutable.HashSet
import qq.util.Vector

/**
 * a edge in the graph. There is at most one edge between two nodes. All
 *  edges that should connect those two nodes in the underlying data
 *  are added to the data and reverseData collections
 */
class Edge(
    val graph: Graph,
    val from: Node,
    val to: Node,
    data0: AbstractEdge) {

  /** all abstract edges represented by this drawn link */
  val data: HashSet[AbstractEdge] = HashSet(data0)
  /** abstract edges represented by a edge in the opposite direction of this one */
  val reverseData: HashSet[AbstractEdge] = HashSet()

  def r: Vector = to.pos - from.pos
  /** calculate the direction stabilising force due to this edge and add it to from and to */
  def calculateForce(): Unit = {
    // average of all edges?
    
    val d = Vector.avg(data.toSeq.map(_.idealDirection(graph.file)) ++ reverseData.toSeq.map(-_.idealDirection(graph.file)))
    val F = (d - r * (d * r) / (r * r)) * graph.properties.c4()
    if (F.isFinite()) {
      val ff = F * F
      from.force -= F
      to.force += F
      from.energy += ff
      to.energy += ff
    }
  }

  /** print a block of strings */
  private def putsstr(g: swing.Graphics2D, top: Float, right: Float, ss: Seq[String]) = {
    val h = g.getFontMetrics.getHeight
    var y = top + h
    for (s ← ss) {
      g.drawString(s, right - g.getFontMetrics.stringWidth(s), y)
      y += h
    }
  }
  private def putssbr(g: swing.Graphics2D, bottom: Float, right: Float, ss: Seq[String]) = {
    val h = g.getFontMetrics.getHeight
    var y = bottom - g.getFontMetrics.getDescent
    for (s ← ss) {
      g.drawString(s, right - g.getFontMetrics.stringWidth(s), y)
      y -= h
    }
  }
  private def putsstl(g: swing.Graphics2D, top: Float, left: Float, ss: Seq[String]) = {
    val h = g.getFontMetrics.getHeight
    var y = top + h
    for (s ← ss) {
      g.drawString(s, left, y)
      y += h
    }
  }
  private def putssbl(g: swing.Graphics2D, bottom: Float, left: Float, ss: Seq[String]) = {
    val h = g.getFontMetrics.getHeight
    var y = bottom - g.getFontMetrics.getDescent
    for (s ← ss) {
      g.drawString(s, left, y)
      y -= h
    }
  }

  /**
   * draw this edge, add decoration and textLabels from data and reverse data if feasible,
   * otherwise provide interactive means to get to them
   */
  def draw(g: swing.Graphics2D) {
    val t = g.getTransform
    val fromLabels = reverseData.iterator.map(_.textLabel).toSeq
    val toLabels = data.iterator.map(_.textLabel).toSeq
    if (from == to) {
      g.translate(from.pos.x.toInt + from.width / 2, from.pos.y.toInt)
      g.drawOval(-10, -20, 40, 40)
      putsstl(g, 30, 0, fromLabels)
      putssbl(g, 30, 0, toLabels)
    } else {
      val f = from.pos + from.toBorder(to.pos - from.pos)
      val t = to.pos + to.toBorder(from.pos - to.pos)
      g.translate(f.x.toInt, f.y.toInt)
      val d = t - f
      val φ = d.direction
      if (φ.abs <= math.Pi / 2) {
        // write in direction of edge
        g.rotate(φ)
        putsstr(g, 0, d.abs, toLabels)
        putsstl(g, 0, 0, fromLabels)
        g.drawLine(0, 0, d.abs.toInt, 0)
      } else {
        // write in opposite direction
        g.rotate(φ + math.Pi)
        putsstr(g, 0, 0, fromLabels)
        putsstl(g, 0, -d.abs, toLabels)
        g.drawLine(0, 0, -d.abs.toInt, 0)

      }
    }
    g.setTransform(t)
  }

}