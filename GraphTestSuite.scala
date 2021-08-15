package sol

import tester.Tester;

/**
 * Tests for graph and node classes.
 */
class GraphTestSuite {
  /**
   * method that prints out the matrix
   *
   * @param adjacencyMatrix - the matrix that we want to print out
   */
  def printMatrix(adjacencyMatrix: AdjacencyMatrix[String]): Unit = {
    for (i <- adjacencyMatrix.theMatrix.indices) {
      for (j <- adjacencyMatrix.theMatrix.indices) {
        if (adjacencyMatrix.theMatrix(i)(j)) {
          print("T" + "  ")
        } else {
          print("F" + "  ")
        }
      }
      println()
    }
  }

  //first generic 5-node test
  def testMatrix1(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[String](5)
    val node1 = adjMatrix.addNode("a")
    val node2 = adjMatrix.addNode("b")
    val node3 = adjMatrix.addNode("c")
    val node4 = adjMatrix.addNode("d")
    val node5 = adjMatrix.addNode("e")

    val edge1 = adjMatrix.addEdge(node5, node2)
    val edge2 = adjMatrix.addEdge(node2, node1)
    val edge3 = adjMatrix.addEdge(node2, node3)
    val edge4 = adjMatrix.addEdge(node3, node4)
    val edge5 = node1.addEdge(node3)

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node2.getContents(), "b")
    t.checkExpect(node3.getContents(), "c")
    t.checkExpect(node4.getContents(), "d")
    t.checkExpect(node5.getContents(), "e")

    t.checkExpect(node5.getNexts(), List(node2))
    t.checkExpect(node2.getNexts(), List(node3, node1))
    t.checkExpect(node1.getNexts(), List(node3))
    t.checkExpect(node3.getNexts(), List(node4))

    adjMatrix.show()
    printMatrix(adjMatrix)
    System.out.println(adjMatrix.indexArray.mkString("Array(", ", ", ")"))
  }

  //cycle, one node by itself, bidirectional edge
  def testMatrix2(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[String](5)
    val node1 = adjMatrix.addNode("a")
    val node2 = adjMatrix.addNode("b")
    val node3 = adjMatrix.addNode("c")
    val node4 = adjMatrix.addNode("by itself")
    val node5 = adjMatrix.addNode("e")

    val edge1 = adjMatrix.addEdge(node5, node2)
    val edge2 = adjMatrix.addEdge(node2, node1)
    val edge3 = node1.addEdge(node3)
    val edge4 = adjMatrix.addEdge(node3, node1)
    val edge5 = node3.addEdge(node2)

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node2.getContents(), "b")
    t.checkExpect(node3.getContents(), "c")
    t.checkExpect(node4.getContents(), "by itself")
    t.checkExpect(node5.getContents(), "e")

    t.checkExpect(node5.getNexts(), List(node2))
    t.checkExpect(node2.getNexts(), List(node1))
    t.checkExpect(node1.getNexts(), List(node3))
    t.checkExpect(node3.getNexts(), List(node2, node1))
    t.checkExpect(node4.getNexts(), List())

    adjMatrix.show()
    printMatrix(adjMatrix)
    System.out.println(adjMatrix.indexArray.mkString("Array(", ", ", ")"))
  }

  //a just points to b and nothing else
  def testMatrix3(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[String](2)
    val node1 = adjMatrix.addNode("a")
    val node2 = adjMatrix.addNode("b")

    val edge1 = adjMatrix.addEdge(node2, node1)

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node2.getContents(), "b")

    t.checkExpect(node2.getNexts(), List(node1))
    t.checkExpect(node1.getNexts(), List())

    adjMatrix.show()
    printMatrix(adjMatrix)
    System.out.println(adjMatrix.indexArray.mkString("Array(", ", ", ")"))
  }

  //a and b just floating by themselves
  def testMatrix4(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[String](2)
    val node1 = adjMatrix.addNode("a")
    val node2 = adjMatrix.addNode("b")

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node2.getContents(), "b")

    t.checkExpect(node2.getNexts(), List())
    t.checkExpect(node1.getNexts(), List())

    adjMatrix.show()
    printMatrix(adjMatrix)
    System.out.println(adjMatrix.indexArray.mkString("Array(", ", ", ")"))
  }

  //one node graph with edge to itself
  def testMatrix5(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[String](2)
    val node1 = adjMatrix.addNode("a")

    val edge1 = adjMatrix.addEdge(node1, node1)

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node1.getNexts(), List(node1))

    adjMatrix.show()
    printMatrix(adjMatrix)
    System.out.println(adjMatrix.indexArray.mkString("Array(", ", ", ")"))
  }

  //just node by itself, no connections or anything
  def testMatrix6(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[String](1)
    val node1 = adjMatrix.addNode("a")

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node1.getNexts(), List())

    adjMatrix.show()
    printMatrix(adjMatrix)
    System.out.println(adjMatrix.indexArray.mkString("Array(", ", ", ")"))
  }

  //empty graph
  def testMatrix7(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[String](0)
    adjMatrix.show()
    printMatrix(adjMatrix)
    System.out.println(adjMatrix.indexArray.mkString("Array(", ", ", ")"))
  }

  //first generic 5-node test
  def testList1(t: Tester): Unit = {
    println()
    println()
    println()

    val adjList = new AdjacencyList[String]
    val node1 = adjList.addNode("a")
    val node2 = adjList.addNode("b")
    val node3 = adjList.addNode("c")
    val node4 = adjList.addNode("d")
    val node5 = adjList.addNode("e")

    val edge1 = adjList.addEdge(node5, node2)
    val edge2 = adjList.addEdge(node2, node1)
    val edge3 = adjList.addEdge(node2, node3)
    val edge4 = adjList.addEdge(node3, node4)
    val edge5 = node1.addEdge(node3)

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node2.getContents(), "b")
    t.checkExpect(node3.getContents(), "c")
    t.checkExpect(node4.getContents(), "d")
    t.checkExpect(node5.getContents(), "e")

    t.checkExpect(node5.getNexts(), List(node2))
    t.checkExpect(node2.getNexts(), List(node3, node1))
    t.checkExpect(node1.getNexts(), List(node3))
    t.checkExpect(node3.getNexts(), List(node4))

    adjList.show()
    System.out.println(adjList.nodes)
  }

  //cycle, one node by itself, bidirectional edge
  def testList2(t: Tester): Unit = {
    println()
    println()
    println()

    val adjList = new AdjacencyList[String]
    val node1 = adjList.addNode("a")
    val node2 = adjList.addNode("b")
    val node3 = adjList.addNode("c")
    val node4 = adjList.addNode("by itself")
    val node5 = adjList.addNode("e")

    val edge1 = adjList.addEdge(node5, node2)
    val edge2 = adjList.addEdge(node2, node1)
    val edge3 = node1.addEdge(node3)
    val edge4 = adjList.addEdge(node3, node1)
    val edge5 = node3.addEdge(node2)

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node2.getContents(), "b")
    t.checkExpect(node3.getContents(), "c")
    t.checkExpect(node4.getContents(), "by itself")
    t.checkExpect(node5.getContents(), "e")

    t.checkExpect(node5.getNexts(), List(node2))
    t.checkExpect(node2.getNexts(), List(node1))
    t.checkExpect(node1.getNexts(), List(node3))
    t.checkExpect(node3.getNexts(), List(node2, node1))
    t.checkExpect(node4.getNexts(), List())

    adjList.show()
    System.out.println(adjList.nodes)
  }

  //a just points to b and nothing else
  def testList3(t: Tester): Unit = {
    println()
    println()
    println()

    val adjList = new AdjacencyList[String]
    val node1 = adjList.addNode("a")
    val node2 = adjList.addNode("b")

    val edge1 = adjList.addEdge(node2, node1)

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node2.getContents(), "b")

    t.checkExpect(node2.getNexts(), List(node1))
    t.checkExpect(node1.getNexts(), List())

    adjList.show()
    System.out.println(adjList.nodes)
  }

  //a and b just floating by themselves
  def testList4(t: Tester): Unit = {
    println()
    println()
    println()

    val adjList = new AdjacencyList[String]
    val node1 = adjList.addNode("a")
    val node2 = adjList.addNode("b")

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node2.getContents(), "b")

    t.checkExpect(node2.getNexts(), List())
    t.checkExpect(node1.getNexts(), List())

    adjList.show()
    System.out.println(adjList.nodes)
  }

  //one node graph with edge to itself
  def testList5(t: Tester): Unit = {
    println()
    println()
    println()

    val adjList = new AdjacencyList[String]
    val node1 = adjList.addNode("a")

    val edge1 = adjList.addEdge(node1, node1)

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node1.getNexts(), List(node1))

    adjList.show()
    System.out.println(adjList.nodes)
  }

  //just node by itself, no connections or anything
  def testList6(t: Tester): Unit = {
    println()
    println()
    println()

    val adjList = new AdjacencyList[String]
    val node1 = adjList.addNode("a")

    t.checkExpect(node1.getContents(), "a")
    t.checkExpect(node1.getNexts(), List())

    adjList.show()
    System.out.println(adjList.nodes)
  }

  //empty graph
  def testList7(t: Tester): Unit = {
    println()
    println()
    println()

    val adjList = new AdjacencyList[String]
    adjList.show()
    System.out.println(adjList.nodes)
  }
}

object GraphTestSuite extends App {
  Tester.run(new GraphTestSuite())
}
