package qq.graph

import qq.util.Vector
import scala.collection.mutable.HashSet

/** a node in a laid out graph */
class Node(val graph: Graph,
           /** the thing that is represented by this node */
           val data: AbstractNode) {

  val edgesOut: HashSet[Edge] = HashSet()
  val edgesIn: HashSet[Edge] = HashSet()
  def degree = edgesOut.size + edgesIn.size
  def connectedTo(r: Node) = edgesOut.exists(_.to == r) || edgesIn.exists(_.from == r)

  var pos: Vector = new Vector(0f, 0f)
  def width: Int = 0
  def height: Int = 0
  def left: Int = (pos.x - width / 2).toInt
  def top: Int = (pos.y - height / 2).toInt
  var rigidSubGraph: Option[RigidSubGraph] = None

  var energy: Float = 0
  var force: Vector = new Vector(0f, 0f)

  def resetAccumulators: Unit = {
    energy = 0
    force = new Vector(0f, 0f)
  }
  def move(maxDist: Float): Unit = {
    if (!rigidSubGraph.isEmpty) pos += force.min(maxDist)
  }
  /** x.toborder(r) = r * k for some k and x.pos+x.toBorder(r) is on the border of the rectangulat node x */
  def toBorder(r: Vector): Vector = {
    if (r.x == 0 && r.y == 0 || width == 0 || height == 0) return new Vector(0f, 0f)
    if (r.y * width > r.x * height) { // w/h > x/y
      // pos+xr intersects top or bottom
      val h = math.signum(r.y) * height / 2
      new Vector(r.x / r.y * h, h) // scale with h/y because y component of new vector must be h
    } else {
      // pos+xr intersects left or right
      val w = math.signum(r.x) * width / 2
      new Vector(w, r.y / r.x * w)
    }
  }
  /**
   * calculate forces between this and r. will only be called once for each pair,
   *  therefore the accumulators of r are also written. overlapRemoval is a number
   *  0 .. 1 that specified how strongly the distance is corrected for removing
   *  overlap of the node's bounding boxes
   */
  def calculateForce(r: Node, overlapRemoval: Float) = {
    if (r == this) {
      // frame forces here
    } else {
      val p = graph.properties
      val deg = (degree + r.degree - 1)
      val l = if (deg <= 5) p.c2() else p.c2() * math.sqrt(deg / 5)
      val δ = (r.pos - pos).max(p.ε())
      val δ2 = {val δ2 = δ - (toBorder(δ) + r.toBorder(-δ)) * overlapRemoval
        if (δ2 * δ < 0) δ.max(p.ε()) else δ2 }
      val F = if (connectedTo(r)) {
        δ.norm  * p.c1() * math.log(δ2.abs / l)
      } else {
        δ.norm * p.c3() / δ2 / δ2
      }

      val F = u *

    }
  }

}