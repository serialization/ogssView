package qq.graph

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import de.ust.skill.common.scala.api;

/** a graph showing objects from file in page */
class Graph(
    val file: qq.editor.File,
    val page: qq.editor.Page,
    val properties: LayoutSettings) {

  /**
   * nodes in the graph (indexed by the thing they represent).7
   *  edges are found inside the nodes
   */
  val nodes: HashMap[AbstractNode, Node] = HashMap()

  def addNode(x: AbstractNode): Unit = {
    if (!nodes.contains(x)) {
      nodes(x) = new Node(this, x)
    }
  }
  def addEdge(x: AbstractEdge): Unit = {
    val f = x.getFrom
    val t = x.getTo
    val F = nodes.getOrElseUpdate(f, new Node(this, f))
    val T = nodes.getOrElseUpdate(t, new Node(this, t))
    // add to existing edge if one exists
    if (F.edgesOut.contains(T)) {
      F.edgesOut(T).data += x
    } else if (T.edgesOut.contains(F)) {
      T.edgesOut(F).reverseData += x
    } else {
      val X = new Edge(this, F, T, x)
      F.edgesOut(T) = X
      T.edgesIn(F) = X
    }
  }

  /** components of the graph that should keep their layout */
  val rigidSubgraphs: HashSet[RigidSubGraph] = HashSet()

  def energy: Float = nodes.values.map(_.energy).sum

  def resetAccumulators: Unit = nodes.values.foreach(_.resetAccumulators)
  def calculateForce(overlapAvoidance: Float, size: java.awt.Dimension): Unit = {
    val nvis = nodes.values.toIndexedSeq
    for (
      i ← 0 until nvis.size;
      j ← i until nvis.size
    ) {
      nvis(i).calculateForce(nvis(j), overlapAvoidance, size)
    }
  }
  def move(maxDist: Float): Unit = {
    nodes.values.foreach(_.move(maxDist))
    rigidSubgraphs.foreach(_.move(maxDist))
  }
  /**
   * freeze current layout, i.e. add all nodes to one rigid sub-graph
   *  and TODO tell file about it
   */
  def freeze: Unit = {
    val r = new RigidSubGraph(this)
    r.nodes ++= nodes.values
    nodes.values.foreach(_.rigidSubGraph = Some(r))
    rigidSubgraphs.clear()
    rigidSubgraphs += r
  }

  val energyOfStep = new ArrayBuffer[Float]()
  /**
   * place nodes in a rectangle of the given size
   */
  def placeNodes(size: java.awt.Dimension): Unit = {
    energyOfStep.clear()
    var stepsize: Float = (size.width max size.height) / 10
    var energyPreviousStep = Float.PositiveInfinity
    var stepsWithProgress = 0

    for (step ← 0.until(150)) {
      resetAccumulators
      calculateForce(((step - 50).toFloat / 50).max(0).min(1), size)
      move(stepsize)
      energyOfStep += energy
      if (energy <= energyPreviousStep) {
        stepsWithProgress += 1
        if (stepsWithProgress >= 5) {
          stepsize /= 0.95f
          stepsWithProgress = 0
        }
      } else {
        stepsize *= 0.95f
        stepsWithProgress = 0
      }
      energyPreviousStep = energy
    }

  }

}