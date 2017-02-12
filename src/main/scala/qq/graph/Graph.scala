package qq.graph

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import de.ust.skill.common.scala.api;

/** a graph showing objects from file in page */
class Graph(
    val file: qq.editor.File,
    val page: qq.editor.Page,
    val properties: LayoutProperties
    ) {
  
  /** nodes in the graph (indexed by the thing they represent);
   *  edges can be found inside the nodes */
  val nodes: HashMap[AbstractNode, Node] = HashMap()
  
  def addNode(x: AbstractNode) = {
    if (!nodes.contains(x)) {
      nodes(x) = new Node(this, x)
    }
  }
  
  /** components of the graph that should keep their layout */
  val rigidSubgraphs: HashSet[RigidSubGraph] = HashSet()
  
  def energy: Float = nodes.values.map(_.energy).sum
  
  def resetAccumulators: Unit = nodes.values.foreach(_.resetAccumulators)
  def calculateForce(overlapAvoidance: Float): Unit = {
    val nvis = nodes.values.toIndexedSeq
    for (i <- 0 until nvis.size;
         j <- i until nvis.size) {
      nvis(i).calculateForce(nvis(j), overlapAvoidance)
    }
  }
  def move(maxDist: Float): Unit = {
    nodes.values.foreach(_.move(maxDist))
    rigidSubgraphs.foreach(_.move(maxDist))
  }
  /** freeze current layout, i.e. add all nodes to one rigid sub-graph
   *  and TODO tell file about it */
  def freeze: Unit = {
    val r = new RigidSubGraph(this)
    r.nodes ++= nodes.values
    nodes.values.foreach(_.rigidSubGraph = Some(r))
    rigidSubgraphs.clear()
    rigidSubgraphs += r
  }
  
  
}