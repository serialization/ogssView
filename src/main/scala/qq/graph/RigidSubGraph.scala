package qq.graph

import scala.collection.mutable.HashSet
import qq.util.Vector

/** a set of nodes the relative position of which is fixed */
class RigidSubGraph(
    val graph: Graph) {
  
  val nodes: HashSet[Node] = HashSet()
  
  def force: Vector = nodes.foldLeft(new Vector(0f, 0f)){case (l, r) => l + r.force} / nodes.size
  
  def move(maxDist: Float) = {
    val d = force.min(maxDist)
    nodes.foreach (_.pos += d)
  }
  
}