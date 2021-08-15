package sol

import src.{IGraph, INode}

/**
 * Class for the adjacency list representation of IGraph.
 *
 * @tparam T the type of the nodes' contents
 */
class AdjacencyList[T] extends IGraph[T] {
  /**
   * a list that stores the actual nodes
   */
  var nodes = List[ALNode]()

  override def addNode(contents: T): INode[T] = {
    val newNode = new ALNode(contents, List())
    nodes = newNode :: nodes
    newNode
  }

  override def addEdge(fromNode: INode[T], toNode: INode[T]): Unit = {
    fromNode.addEdge(toNode)
  }

  override def show(): Unit = {
    for (node <- nodes) {
      println(node.contents + " gets to " + node.getsTo.toString)
    }
    for (node <- nodes) {
      println(node)
    }
  }

  /**
   * Inner class for a node in an AdjacencyList graph.
   *
   * @param contents the contents of the node
   * @param getsTo   the list of nodes which the current node outbounds to
   */
  class ALNode(val contents: T, var getsTo: List[INode[T]]) extends INode[T] {

    override def getContents(): T = contents

    override def getNexts(): List[INode[T]] = getsTo

    override def addEdge(toNode: INode[T]): Unit = {
      getsTo = toNode :: getsTo
    }

    override def toString: String = {
      this.contents.toString
    }
  }
}
