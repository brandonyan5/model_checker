package src

/**
 * Trait for a node in a graph.
 * @tparam T - the type of its contents
 */
trait INode[T] {

  /**
   * Gets the contents of the node.
   * @return the contents of the node
   */
  def getContents(): T

  /**
   * Finds all nodes connected to this node by outgoing edges.
   * @return a List[Node[T]] of all nodes that the current node connects to
   */
  def getNexts(): List[INode[T]]

  /**
   * Adds an outgoing edge from this node to another node.
   * @param toNode the node to which to add an edge
   */
  def addEdge(toNode: INode[T]): Unit
}