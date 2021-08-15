package sol

import src.{IGraph, INode}

/**
 * Adjacency matrix implementation of an IGraph.
 *
 * @param maxSize the maximum number of nodes in the graph
 * @tparam T the type of the nodes contents
 */
class AdjacencyMatrix[T](val maxSize: Integer) extends IGraph[T] {
  if (maxSize < 0) {
    throw new IllegalArgumentException("cannot have a negative matrix size")
  }

  /**
   * field that keeps track of the current size of the matrix
   */
  var currentSize: Int = 0

  /**
   * field that is an array, stores the actual nodes corresponding to their
   * index
   */
  val indexArray: Array[INode[T]] = new Array[INode[T]](maxSize)

  /**
   * 2D array that stores whether an edge occurs between two nodes inside
   * its cells
   */
  val theMatrix: Array[Array[Boolean]] = Array.fill
    [Array[Boolean]](maxSize)(Array.fill
      [Boolean](maxSize)(false))

  override def addNode(contents: T): INode[T] = {
    if (currentSize <= maxSize) {
      currentSize = currentSize + 1
      val newNode = new MNode(contents, currentSize - 1)
      indexArray(currentSize - 1) = newNode
      newNode
    } else {
      throw new IllegalStateException("cannot add more nodes than maxSize")
    }
  }

  override def addEdge(fromNode: INode[T], toNode: INode[T]): Unit = {
    fromNode.addEdge(toNode)
  }

  override def show(): Unit = {
    for (i <- 0 until currentSize) {
      for (j <- 0 until currentSize) {
        if (theMatrix(i)(j)) {
          println(indexArray(i).getContents() + " gets to "
            + indexArray(j).toString)
        }
      }
    }
    for (x <- indexArray) {
      println(x)
    }
  }

  /**
   * Class for a node in an AdjacencyMatrix graph.
   *
   * @param contents - the node's contents
   * @param index    - the index in the matrix that corresponds to this node
   */
  class MNode(val contents: T, val index: Int) extends INode[T] {
    override def getContents(): T = this.contents

    override def getNexts(): List[INode[T]] = {
      var returnList: List[INode[T]] = List()
      for (j <- 0 until currentSize) {
        if (theMatrix(index)(j)) {
          returnList = indexArray(j) :: returnList
        }
      }
      returnList
    }

    override def addEdge(toNode: INode[T]): Unit = {
      theMatrix(this.index)(indexArray.indexOf(toNode)) = true
    }

    override def toString: String = this.contents.toString
  }
}
