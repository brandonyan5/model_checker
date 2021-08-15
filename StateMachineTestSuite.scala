package sol

import src.State
import tester.Tester

/**
 * Tests for methods on a StateMachine.
 */
class StateMachineTestSuite {
  /**
   * checkState function to see if hope maps to red
   *
   * @param theState - the state we're checking
   * @return - whether the state is true or false based on the function
   */
  def checkState(theState: State): Boolean = {
    theState.data("hope") == "red"
  }

  /**
   * checkState function to see if hope maps to green
   *
   * @param theState - the state we're checking
   * @return - whether the state is true or false based on the function
   */
  def checkStateGreen(theState: State): Boolean = {
    theState.data("hope") == "green"
  }

  def testState1(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    machine.initFromCSV("traffic-states.csv",
      "traffic-transitions.csv")

    val testState = new State("1", Map(("hope" -> "red"),
      ("waterman" -> "green")))
    t.checkExpect(machine.checkAlways(testState, checkState),
      Option(new State("7", Map(("hope" -> "green"),
        ("waterman" -> "red")))))
    t.checkExpect(machine.checkNever(testState, checkState),
      Option(new State("1", Map(("hope" -> "red"),
        ("waterman" -> "green")))))
    t.checkExpect(machine.checkEventually(testState, checkState),
      true)
  }

  def testState2(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    machine.initFromCSV("traffic-states.csv",
      "traffic-transitions.csv")

    val testState = new State("1", Map("hope" -> "red", "waterman" -> "green"))
    t.checkExpect(machine.checkAlways(testState, checkState),
      Option(new State("7", Map(("hope" -> "green"), ("waterman" -> "red")))))
    t.checkExpect(machine.checkNever(testState, checkState),
      Option(new State("1", Map(("hope" -> "red"), ("waterman" -> "green")))))
    t.checkExpect(machine.checkEventually(testState, checkState), true)
  }

  def testState3(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    machine.initFromCSV("test3and4-states.csv", "test3and4-transitions.csv")

    val testState = new State("1", Map("hope" -> "green"))
    t.checkExpect(machine.checkAlways(testState, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(testState, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(testState, checkState), true)
  }

  def testState4(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    machine.initFromCSV("test3and4-states.csv", "test3and4-transitions.csv")

    val testState = new State("1", Map("hope" -> "green"))
    t.checkExpect(machine.checkAlways(testState, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(testState, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(testState, checkState), true)
  }

  def testState5(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "green"))
    val exceptionState = new State("4", Map("hope" -> "black"))
    val exceptionTransState = new State("7", Map("hope" -> "purple"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node2)
    machine.addTransition(node2, node4)

    t.checkExpect(machine.stateMap("3").getContents()
      == new State("3", Map("hope" -> "red")), true)
    t.checkException(
      new IllegalStateException("can't add a state with an existing id"),
      machine, "addState", exceptionState)
    t.checkException(
      new IllegalStateException
      ("transition must be between 2 existing states"),
      machine, "addTransition", exceptionTransState, node1)
    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), false)
  }

  def testState6(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "green"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node2)
    machine.addTransition(node2, node4)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), false)
  }

  def testState7(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "green"))
    val node5 = new State("5", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node5)
    machine.addTransition(node4, node2)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("5", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), false)
  }

  def testState8(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "green"))
    val node5 = new State("5", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node5)
    machine.addTransition(node4, node2)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("5", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), false)
  }

  def testState9(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "red"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "green"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node2)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("2", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState10(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "red"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "green"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node2)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("2", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState11(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "green"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node2)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState12(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "green"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node2)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState13(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "red"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "green"))
    val node5 = new State("5", Map("hope" -> "green"))
    val node6 = new State("6", Map("hope" -> "green"))
    val node7 = new State("7", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addState(node6)
    machine.addState(node7)
    machine.addTransition(node2, node1)
    machine.addTransition(node3, node2)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node5)
    machine.addTransition(node5, node6)
    machine.addTransition(node6, node4)
    machine.addTransition(node5, node7)

    t.checkExpect(machine.checkAlways(node3, checkState),
      Option(new State("3", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node3, checkState),
      Option(new State("1", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node3, checkState), false)
  }

  def testState14(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "red"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "green"))
    val node5 = new State("5", Map("hope" -> "green"))
    val node6 = new State("6", Map("hope" -> "green"))
    val node7 = new State("7", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addState(node6)
    machine.addState(node7)
    machine.addTransition(node2, node1)
    machine.addTransition(node3, node2)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node5)
    machine.addTransition(node5, node6)
    machine.addTransition(node6, node4)
    machine.addTransition(node5, node7)

    t.checkExpect(machine.checkAlways(node3, checkState),
      Option(new State("3", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node3, checkState),
      Option(new State("1", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node3, checkState), false)
  }

  def testState15(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "green"))
    val node5 = new State("5", Map("hope" -> "green"))
    val node6 = new State("6", Map("hope" -> "red"))
    val node7 = new State("7", Map("hope" -> "green"))
    val node8 = new State("8", Map("hope" -> "green"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addState(node6)
    machine.addState(node7)
    machine.addState(node8)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node3, node5)
    machine.addTransition(node1, node6)
    machine.addTransition(node6, node7)
    machine.addTransition(node6, node8)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState16(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "green"))
    val node5 = new State("5", Map("hope" -> "green"))
    val node6 = new State("6", Map("hope" -> "red"))
    val node7 = new State("7", Map("hope" -> "green"))
    val node8 = new State("8", Map("hope" -> "green"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addState(node6)
    machine.addState(node7)
    machine.addState(node8)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node3, node5)
    machine.addTransition(node1, node6)
    machine.addTransition(node6, node7)
    machine.addTransition(node6, node8)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState17(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "red"))
    val node5 = new State("5", Map("hope" -> "green"))
    val node6 = new State("6", Map("hope" -> "green"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addState(node6)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node1, node4)
    machine.addTransition(node4, node5)
    machine.addTransition(node4, node6)
    machine.addTransition(node2, node5)
    machine.addTransition(node5, node2)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), false)
  }

  def testState18(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "red"))
    val node5 = new State("5", Map("hope" -> "green"))
    val node6 = new State("6", Map("hope" -> "green"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addState(node6)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node1, node4)
    machine.addTransition(node4, node5)
    machine.addTransition(node4, node6)
    machine.addTransition(node2, node5)
    machine.addTransition(node5, node2)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), false)
  }

  def testState19(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "red"))
    val node2 = new State("2", Map("hope" -> "red"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node1)

    t.checkExpect(machine.checkAlways(node1, checkState), None)
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("1", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState20(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "red"))
    val node2 = new State("2", Map("hope" -> "red"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node1)

    t.checkExpect(machine.checkAlways(node1, checkState), None)
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("1", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState23(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "red"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "green"))
    val node5 = new State("5", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node5)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("2", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("1", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState24(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "red"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "green"))
    val node5 = new State("5", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node5)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("2", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("1", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState25(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node1, node3)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("4", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState26(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node1, node3)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node4)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("4", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState31(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "green"))
    val node5 = new State("5", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node2, node4)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node5)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("5", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState32(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "green"))
    val node5 = new State("5", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node2, node4)
    machine.addTransition(node3, node4)
    machine.addTransition(node4, node5)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("5", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState33(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    machine.initFromCSV("circle-states.csv", "circle-transitions.csv")
    val testState = new State("1",
      Map(("hope" -> "green"), ("waterman" -> "green")))

    t.checkExpect(machine.checkAlways(testState, checkState),
      Option(new State("1", Map("hope" -> "green", "waterman" -> "green"))))
    t.checkExpect(machine.checkNever(testState, checkState),
      Option(new State("4", Map("hope" -> "red", "waterman" -> "yellow"))))
    t.checkExpect(machine.checkEventually(testState, checkState), true)
  }

  def testState34(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    machine.initFromCSV("circle-states.csv", "circle-transitions.csv")
    val testState = new State("1",
      Map(("hope" -> "green"), ("waterman" -> "green")))

    t.checkExpect(machine.checkAlways(testState, checkState),
      Option(new State("1", Map("hope" -> "green", "waterman" -> "green"))))
    t.checkExpect(machine.checkNever(testState, checkState),
      Option(new State("4", Map("hope" -> "red", "waterman" -> "yellow"))))
    t.checkExpect(machine.checkEventually(testState, checkState), true)
  }

  def testState35(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node1)
    machine.addTransition(node1, node3)
    machine.addTransition(node3, node1)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node2)

    t.checkExpect(machine.checkAlways(node3, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), false)
  }

  def testState36(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node1)
    machine.addTransition(node1, node3)
    machine.addTransition(node3, node1)
    machine.addTransition(node2, node3)
    machine.addTransition(node3, node2)

    t.checkExpect(machine.checkAlways(node3, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("3", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), false)
  }

  def testState37(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "red"))
    val node2 = new State("2", Map("hope" -> "red"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "red"))
    val node5 = new State("5", Map("hope" -> "green"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node2, node4)
    machine.addTransition(node4, node5)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("5", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkStateGreen),
      Option(new State("5", Map("hope" -> "green"))))
    t.checkExpect(machine.checkEventually(node1, checkStateGreen), false)
    t.checkExpect(machine.checkEventually(node4, checkStateGreen), true)
  }

  def testState38(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "red"))
    val node2 = new State("2", Map("hope" -> "red"))
    val node3 = new State("3", Map("hope" -> "red"))
    val node4 = new State("4", Map("hope" -> "red"))
    val node5 = new State("5", Map("hope" -> "green"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addState(node5)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node2, node4)
    machine.addTransition(node4, node5)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("5", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkStateGreen),
      Option(new State("5", Map("hope" -> "green"))))
    t.checkExpect(machine.checkEventually(node1, checkStateGreen), false)
    t.checkExpect(machine.checkEventually(node4, checkStateGreen), true)
  }

  def testState39(t: Tester): Unit = {
    val adjMatrix = new AdjacencyMatrix[State](50)
    val machine = new StateMachine(adjMatrix)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node1, node3)
    machine.addTransition(node3, node4)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("4", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }

  def testState40(t: Tester): Unit = {
    val adjList = new AdjacencyList[State]
    val machine = new StateMachine(adjList)
    val node1 = new State("1", Map("hope" -> "green"))
    val node2 = new State("2", Map("hope" -> "green"))
    val node3 = new State("3", Map("hope" -> "green"))
    val node4 = new State("4", Map("hope" -> "red"))
    machine.addState(node1)
    machine.addState(node2)
    machine.addState(node3)
    machine.addState(node4)
    machine.addTransition(node1, node2)
    machine.addTransition(node2, node3)
    machine.addTransition(node1, node3)
    machine.addTransition(node3, node4)

    t.checkExpect(machine.checkAlways(node1, checkState),
      Option(new State("1", Map("hope" -> "green"))))
    t.checkExpect(machine.checkNever(node1, checkState),
      Option(new State("4", Map("hope" -> "red"))))
    t.checkExpect(machine.checkEventually(node1, checkState), true)
  }
}

object StateMachineTestSuite extends App {
  Tester.run(new StateMachineTestSuite())
}


