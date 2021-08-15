package src

/**
 * Trait for an abstracted graph
 *
 * @tparam T the type of the nodes' contents
 */
trait IGraph[T] {

  /**
   * Adds a node to the graph with specified contents.
   * @param contents - the contents inside the new node
   * @return the Node[T] added to the graph
   */
  def addNode(contents: T): INode[T]

  /**
   * Adds a directed edge that connects two nodes.
   * @param fromNode - the origin node of the edge
   * @param toNode - the destination node of the edge
   */
  def addEdge(fromNode: INode[T], toNode: INode[T]): Unit

  /**
   * prints the graph by showing which nodes connect to other nodes.
   */
  def show(): Unit
}