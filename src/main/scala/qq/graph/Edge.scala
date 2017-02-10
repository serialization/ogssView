package qq.graph

import scala.collection.mutable.HashSet

/** a edge in the graph. There is at most one egde between two nodes. All
 *  edges that should connect those two nodes in the underlying data
 *  are added to the data and reverseData collections */
class Edge(
    val graph: Graph,
    val from: Node,
    val to: Node,
    data0: AbstractEdge) {
  
  /** all abstract edges represented by this drawn link */
  val data: HashSet[AbstractEdge] = HashSet(data0)
  /** abstract edges represented by a edge in the opposite direction of this one */
  val reverseData: HashSet[AbstractEdge] = HashSet()
  
}