package qq.graph

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import de.ust.skill.common.scala.api;
import qq.util.Vector

/** a graph showing objects from file in page */
class Graph(
    val file: qq.editor.File,
    val viewer: qq.editor.objects.ObjectGraph[_],
    val properties: LayoutSettings) {

  /**
   * nodes in the graph (indexed by the thing they represent).
   *  edges are found inside the nodes
   */
  val nodes: HashMap[AbstractNode, Node] = HashMap()

  def addNode(x: AbstractNode): Unit = {
    if (!nodes.contains(x)) {
      val n = new Node(this, x)
      nodes(x) = n
    }
  }
  /**
   * add edge x to the graph; add nodes for end points if necessary, append to existing edge is posible. If nodes
   *  are added, initialise their positions
   */
  def addEdge(x: AbstractEdge): Unit = {
    val f = x.getFrom
    val t = x.getTo
    var addedF = false
    var addedT = false
    val F = if (nodes.contains(f)) nodes(f) else {
      addedF = true;
      val F = new Node(this, f);
      nodes(f) = F;
      F
    }
    val T = if (nodes.contains(t)) nodes(t) else {
      addedT = true;
      val T = new Node(this, t);
      nodes(t) = T;
      T
    }
    if (addedF && !addedT) {
      F.pos = T.pos - x.idealDirection(file) * properties.c2()
    } else if (addedT) {
      T.pos = F.pos + x.idealDirection(file) * properties.c2()
    }
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
  private def calculateForce(overlapAvoidance: Float, size: java.awt.Dimension, stepSize: Float): Unit = {
    val nvis = nodes.values.toIndexedSeq
    for (i ← 0 until nvis.size) {
      for (j ← i until nvis.size) {
        nvis(i).calculateForce(nvis(j), overlapAvoidance, size)
      }
      for (e ← nvis(i).edgesOut.values) {
        e.calculateForce()
      }
    }
  }
  def move(stepSize: Float): Unit = {
      nodes.values.foreach(_.move(stepSize))
    //  rigidSubgraphs.foreach(_.move(maxDist))
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
  val stepOfStep = new ArrayBuffer[Float]()
  /**
   * place nodes in a rectangle of the given size
   */
  def placeNodes(size: java.awt.Dimension): Unit = {
    /* centre and scale current placement *down* to fit into size if necessary */
    val xmin = nodes.values.map(_.pos.x).min
    val xmax = nodes.values.map(_.pos.x).max
    val ymin = nodes.values.map(_.pos.y).min
    val ymax = nodes.values.map(_.pos.y).max
    if (xmin < 0 || ymin < 0 || xmax > size.width || ymax > size.height) {
      val x0 = (xmin + xmax) / 2
      val y0 = (ymin + ymax) / 2
      val ax = if (xmax - xmin > size.width) size.width.toFloat / (xmax - xmin) else 1.0f
      val ay = if (ymax - ymin > size.height) size.height.toFloat / (ymax - ymin) else 1.0f
      nodes.values.foreach { x ⇒
        x.pos = new Vector(
          ax * (x.pos.x - x0) + size.width / 2,
          ay * (x.pos.y - y0) + size.height / 2)
      }
    }
    energyOfStep.clear()
    stepOfStep.clear()
    var stepsize: Float = 10
    var energyPreviousStep = Float.PositiveInfinity
    var stepsWithProgress = 0

    val initialIterations = properties.initialIterations()
    val phaseInIterations = properties.phaseInIterations()
    for (step ← 0.until(properties.iterations)) {
      resetAccumulators
      calculateForce(((step - initialIterations).toFloat / phaseInIterations).max(0).min(1), size, stepsize)
      move(stepsize)
      // TODO this is not the energy of Hu
      energyOfStep += energy
      stepOfStep += stepsize
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
    if (!cluttered) {
      updateIdealEdgeDirections
    }
  }
  def cluttered: Boolean = energy / nodes.size > properties.cluttered()
  private def updateIdealEdgeDirections = {
    val drawnEdges = for (n ← nodes.values; e ← n.edgesOut.values) yield e
    val abstractEdgesDirections = drawnEdges.toSeq.flatMap(e ⇒ e.data.iterator.map(x ⇒ (x, e.r)) ++ e.reverseData.iterator.map(x ⇒ (x, -e.r)))
    val dirByAbsEdge = abstractEdgesDirections
      .filter(_._1.isInstanceOf[SkillFieldEdge[_]])
      .groupBy(_._1.asInstanceOf[SkillFieldEdge[_]].field)
      .mapValues(x ⇒ Vector.avg(x.map(_._2.norm)))
    for ((f, x) ← dirByAbsEdge if (x.isFinite())) {
      val d = file.fieldSettings(f).prefEdgeDirection
      val d0 = d()
      if (d0.abs < 1.0) {
        val d1 = d0 * d0.abs + x * (1 - d0.abs)
        println(s"${f.name} $d1 $d0 $x ${d1.abs}")
        d := d1.min(0.95f)
      }
    }
  }
}