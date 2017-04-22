package qq.graph

import scala.collection.mutable.HashSet
import qq.util.Vector

/**
 * a edge in the graph as drawn. There is at most one edge between two nodes. All
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

  val uiElementAtFrom = new swing.Label("  ")
  val uiElementAtTo = new swing.Label("  ")
  def updateToolTip: Unit = {
    // two tool tips with all edges neatly on top of each other
    uiElementAtFrom.tooltip = s"""<html><table><tr>
      <td valign="middle">${from.data.name(graph)}</td>
      <td valign="middle"><center>${
      (data.map(x ⇒ x.textLabel(graph.file) + " " + x.toDecoration.symbol) ++
        reverseData.map(x ⇒ x.toDecoration.reverseSymbol + " " + x.textLabel(graph.file))).mkString("<br>")
    }</center></td>
      <td valign="middle">${to.data.name(graph)}</td></tr></html>"""
    uiElementAtTo.tooltip = s"""<html><table><tr>
      <td valign="middle">${to.data.name(graph)}</td>
      <td valign="middle"><center>${
      (reverseData.map(x ⇒ x.textLabel(graph.file) + " " + x.toDecoration.symbol) ++
        data.map(x ⇒ x.toDecoration.reverseSymbol + " " + x.textLabel(graph.file))).mkString("<br>")
    }</center></td>
      <td valign="middle">${from.data.name(graph)}</td></tr></html>"""
    uiElementAtFrom.peer.setSize(uiElementAtFrom.preferredSize)
    uiElementAtTo.peer.setSize(uiElementAtTo.preferredSize)
    // invisible labels that show the tool tips at the endpoints of the edges
    val fromCenter = from.pos + from.toBorder(to.pos - from.pos)
    val toCenter = to.pos + to.toBorder(from.pos - to.pos)
    uiElementAtFrom.peer.setLocation(
      fromCenter.x.toInt - uiElementAtFrom.preferredSize.width / 2,
      fromCenter.y.toInt - uiElementAtFrom.preferredSize.height / 2)
    uiElementAtTo.peer.setLocation(
      toCenter.x.toInt - uiElementAtTo.preferredSize.width / 2,
      toCenter.y.toInt - uiElementAtTo.preferredSize.height / 2)

  }

  def r: Vector = to.pos - from.pos
  /** calculate the direction stabilising force due to this edge and add it to from and to */
  def calculateForce(): Unit = {
    def Fp(p: Vector) = (p - r * (p * r) / (r * r)) * graph.preferences.c4()
    def apply(F: Vector) = if (F.isFinite()) {
      val ff = F * F
      from.force -= F
      to.force += F
      from.energy += ff
      to.energy += ff
    }
    // when many edges for the same field meet at one node we make the force
    // weaker so that the nodes at the other end don't get pushed together too much.
    // thank data and reverseDate that's a lot of cases…

    // e and f represent the same field
    def sameField(e: AbstractEdge, f: AbstractEdge) = {
      e.isInstanceOf[SkillFieldEdge[_]] &&
        f.isInstanceOf[SkillFieldEdge[_]] &&
        e.asInstanceOf[SkillFieldEdge[_]].field ==
        f.asInstanceOf[SkillFieldEdge[_]].field
    }
    /* number of edges from source that represent the same field as e */
    def sameSourceEdgeCount(source: Node, e: AbstractEdge) = {
      if (graph.preferences.scaleDirectionWhenConflict()) {
        source.edgesOut.values.count(_.data.exists(sameField(e, _))) +
          source.edgesIn.values.count(_.reverseData.exists(sameField(e, _)))
      } else {
        1
      }
    }
    /* number of edges to target that represent the same field as e */
    def sameTargetEdgeCount(target: Node, e: AbstractEdge) = {
      if (graph.preferences.scaleDirectionWhenConflict()) {
        target.edgesOut.values.count(_.reverseData.exists(sameField(e, _))) +
          target.edgesIn.values.count(_.data.exists(sameField(e, _)))
      } else {
        1
      }
    }

    for (e ← data) {
      val scale = if (e.isInstanceOf[SkillFieldEdge[_]]) {
        sameSourceEdgeCount(from, e) + sameTargetEdgeCount(to, e) - 1
      } else {
        1
      }
      apply(Fp(e.idealDirection(graph.file)) / scale)
    }
    for (e ← reverseData) {
      val scale = if (e.isInstanceOf[SkillFieldEdge[_]]) {
        sameSourceEdgeCount(to, e) + sameTargetEdgeCount(from, e) - 1
      } else {
        1
      }
      apply(-Fp(e.idealDirection(graph.file)) / scale)
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
    val fromLabels = reverseData.iterator.map(_.textLabel(graph.file)).toSeq
    val fromDecorations = reverseData.iterator.map(_.toDecoration).toSeq.distinct
    val toLabels = data.iterator.map(_.textLabel(graph.file)).toSeq
    val toDecorations = data.iterator.map(_.toDecoration).toSeq.distinct
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
      val fromBelow = from.intersectsTopOrBottom(d) ^ (d.y > 0)
      val toBelow = to.intersectsTopOrBottom(d) ^ (d.y < 0)
      val φ = d.direction
      if (φ.abs <= math.Pi / 2) {
        // write in direction of edge
        g.rotate(φ)
        var width = d.abs.toInt
        for (d ← fromDecorations) {
          d.draw(g)
          width -= d.width
          g.translate(d.width, 0)
        }
        g.translate(width, 0)
        g.rotate(math.Pi)
        for (d ← toDecorations) {
          d.draw(g)
          width -= d.width
          g.translate(d.width, 0)
        }
        g.rotate(math.Pi)
        if (toBelow) putssbr(g, 0, 0, toLabels) else putsstr(g, 0, 0, toLabels)
        if (fromBelow) putssbl(g, 0, -width, fromLabels) else putsstl(g, 0, -width, fromLabels)
        g.drawLine(0, 0, -width, 0)
      } else {
        // write in opposite direction
        g.rotate(φ)
        var width = d.abs.toInt
        for (d ← fromDecorations) {
          d.draw(g)
          width -= d.width
          g.translate(d.width, 0)
        }
        g.translate(width, 0)
        g.rotate(math.Pi)
        for (d ← toDecorations) {
          d.draw(g)
          width -= d.width
          g.translate(d.width, 0)
        }
        if (toBelow) putssbl(g, 0, 0, toLabels) else putsstl(g, 0, 0, toLabels)
        if (fromBelow) putssbr(g, 0, width, fromLabels) else putsstr(g, 0, width, fromLabels)
        g.drawLine(0, 0, width, 0)

      }
    }
    g.setTransform(t)
  }

  // TODO decorations and stack labels of parallel edges in postscript
  def toPs(): String = {
    val fromLabels = reverseData.iterator.map(_.textLabel(graph.file)).toSeq
    val fromDecorations = reverseData.iterator.map(_.toDecoration).toSeq.distinct
    val toLabels = data.iterator.map(_.textLabel(graph.file)).toSeq
    val toDecorations = data.iterator.map(_.toDecoration).toSeq.distinct

    " matrix currentmatrix\n" +
      (if (from == to) {
        " " + (from.pos.x.toInt + from.width / 2) + " top " + from.pos.y.toInt + " sub translate\n" +
          "                           0.0 -10.0 moveto\n" +
          "   5.0 -20.0  30.0 -20.0  30.0   0.0 curveto\n" +
          "  30.0  20.0   5.0  20.0   0.0  10.0 curveto\n" +
          " stroke\n" +
          fromLabels.map("  33.0 0.0 moveto (" + _ + ") show\n").mkString("") +
          toLabels.map("  33.0 0.0 moveto (" + _ + ") show\n").mkString("")
      } else {
        val f = from.pos + from.toBorder(to.pos - from.pos)
        val t = to.pos + to.toBorder(from.pos - to.pos)
        val d = t - f
        val fromBelow = from.intersectsTopOrBottom(d) ^ (d.y < 0)
        val toBelow = to.intersectsTopOrBottom(d) ^ (d.y > 0)
        val φ = -d.direction
        " " + f.x + " top " + f.y + " sub translate\n" +
          (if (φ.abs <= math.Pi / 2) {
            // write in direction of edge
            " " + φ.toDegrees + " rotate\n" +
              toLabels.map(" " + d.abs + " 0 moveto (" + _ + ") " + (if (toBelow) "showtr" else "showbr") + "\n").mkString("") +
              fromLabels.map(" 0 0 moveto (" + _ + ") " + (if (fromBelow) "showtl" else "showbl") + "\n").mkString("") +
              " 0 0 moveto " + d.abs + " 0 lineto stroke\n"
          } else {
            // write in opposite direction
            " " + (φ + math.Pi).toDegrees + " rotate\n" +
              toLabels.map(" " + (-d.abs) + " 0 moveto (" + _ + ") " + (if (toBelow) "showtl" else "showbl") + "\n").mkString("") +
              fromLabels.map(" 0 0 moveto (" + _ + ") " + (if (fromBelow) "showtr" else "showbr") + "\n").mkString("") +
              " 0 0 moveto " + (-d.abs) + " 0 lineto stroke\n"
          })
      }) +
      " setmatrix "

  }

}