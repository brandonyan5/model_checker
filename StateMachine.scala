package sol

import org.apache.commons.csv.CSVRecord
import src.{IGraph, INode, IStateMachine, State}

import scala.collection.mutable

/**
 * A class for a state machine.
 *
 * @param stateGraph - a specific graph implementation of type State
 */
class StateMachine(stateGraph: IGraph[State]) extends IStateMachine {
  /**
   * a hashmap that connects each state's id to its corresponding node
   */
  val stateMap: mutable.HashMap[String, INode[State]] = mutable.HashMap()

  /**
   * a hashmap that connects each state id to the corresponding state
   */
  val stateIDMap: mutable.Map[String, State]
  = collection.mutable.Map[String, State]()

  override def addState(state: State): Unit = {
    if (stateMap.contains(state.id)) {
      throw new IllegalStateException("can't add a state with an existing id")
    } else {
      stateMap.put(state.id, stateGraph.addNode(state))
    }
  }

  override def addTransition(fromState: State, toState: State): Unit = {
    if (stateMap.contains(fromState.id) && stateMap.contains(toState.id)) {
      stateGraph.addEdge(stateMap(fromState.id), stateMap(toState.id))
    } else {
      throw new
          IllegalStateException("transition must be between 2 existing states")
    }
  }

  /**
   * helper method for checkAlways and checkNever that combines all their
   * shared code, and takes in a boolean function based on whether checkNever
   * or checkAlways is called
   *
   * @param op         - function that negates the first if statement or not
   *                   based on whether checkAlways or checkNever calls this
   *                   method
   * @param startState - the starting state to begin the checkNever or
   *                   checkAlways traversal
   * @param checkState - the function that defines whether a state is true
   *                   or not
   * @return - an option that returns the violating state, or None if no state
   *         is violating
   */
  def checkAlwaysNeverHelper(op: Boolean => Boolean, startState: State,
                             checkState: State => Boolean): Option[State] = {
    var leftToCheck = List(stateMap(startState.id))
    var visited = List[INode[State]]()

    while (leftToCheck.nonEmpty) {
      val curr = leftToCheck.head
      leftToCheck = leftToCheck.tail
      if (op(checkState(curr.getContents()))) {
        return Option(curr.getContents())
      } else {
        visited = curr :: visited
        for (eachNode <- curr.getNexts()) {
          if (!visited.contains(eachNode)) {
            leftToCheck = eachNode :: leftToCheck
          }
        }
      }
    }
    None
  }

  override def checkAlways(startState: State,
                           checkState: State => Boolean): Option[State] = {
    checkAlwaysNeverHelper(x => !x, startState, checkState)
  }

  override def checkNever(startState: State,
                          checkState: State => Boolean): Option[State] = {
    checkAlwaysNeverHelper(x => x, startState, checkState)
  }

  /**
   * helper method for checkEventually that helps to recursively keep track
   * of a visited list
   *
   * @param startState - the starting state for checkEventually
   * @param checkState - a function that defines whether a given state is
   *                   true or not
   * @param visited    - a list that keeps track of which nodes have been
   *                   visited so far
   * @return - a boolean that is true if there is every possible path leads
   *         to a true state, and false if not
   */
  def checkEventuallyHelp(startState: State, checkState: State => Boolean,
                          visited: List[INode[State]]): Boolean = {
    val startingNode = stateMap(startState.id)
    val visited2 = visited.appended(startingNode)

    if (startingNode.getNexts() == List()
      || startingNode.getNexts().forall(x => visited2.contains(x))
      || checkState(startState)) {
      checkState(startState)
    } else {
      for (child <- startingNode.getNexts()) {
        if (!visited2.contains(child)
          && !checkEventuallyHelp(child.getContents(), checkState, visited2)) {
          return false
        } else if (visited2.contains(child)
          && !checkState(child.getContents())) {
          return false
        }
      }
      true
    }
  }

  override def checkEventually(startState: State,
                               checkState: State => Boolean): Boolean = {
    checkEventuallyHelp(startState, checkState, List())
  }

  override def show(): Unit = stateGraph.show()

  /**
   * method that takes in a CSVRecord from the states CSV file and then
   * parses it
   *
   * @param record - a certain row of data from the states CSV file
   */
  def parseState(record: CSVRecord): Unit = {
    val stateDataArray = record.get(1).split(";")
    var stateDataMap = Map[String, String]()

    for (keyValuePair <- stateDataArray) {
      val arrayWithKeyAndValue = keyValuePair.split("=")
      stateDataMap = stateDataMap +
        (arrayWithKeyAndValue(0) -> arrayWithKeyAndValue(1))
    }
    val newState = new State(record.get(0), stateDataMap)
    stateIDMap.put(record.get(0), newState)
    this.addState(newState)
  }

  /**
   * method that takes in a CSVRecord from the transitions CSV file and then
   * parses it
   *
   * @param record - a certain row of data from the transitions CSV file
   */
  def parseTransition(record: CSVRecord): Unit = {
    this.addTransition(stateIDMap(record.get(0)), stateIDMap(record.get(1)))
  }

  override def initFromCSV(statesCSV: String, transitionsCSV: String): Unit = {
    src.Parser.parseCSV(statesCSV, parseState)
    src.Parser.parseCSV(transitionsCSV, parseTransition)
  }
}

