package src

/**
 * An interface for a State Machine.
 */
trait IStateMachine {

  /**
   * Adds a state to the state graph.
   * @param state the state to add
   * @throws IllegalStateException if a state with the same id already exists
   *                               in the state graph
   */
  def addState(state: State): Unit

  /**
   * Adds a transition from one state to another, where both states were
   * already present in the state graph.
   * @param fromState the origin state of the transition
   * @param toState the target state of the transition
   */
  def addTransition(fromState: State, toState: State): Unit

  /**
   * Initializes the state graph from CSV files.
   * @param statesCSV - the filename of the CSV containing state information
   * @param transitionsCSV - the filename of the CSV containing transition information
   */
  def initFromCSV(statesCSV: String, transitionsCSV: String): Unit

  /**
   * Checks that every state reachable from a starting state satisfies a
   * given predicate, returning a counter example if one is found.
   * @param startState the state from which to start the search
   * @param checkState the given predicate/condition
   * @return Some[State] if a counter example (a state for which checkState
   *         returns false) is found, None otherwise
   */
  def checkAlways(startState: State, checkState: State => Boolean): Option[State]

  /**
   * Checks that no state reachable from a starting state satisfies a
   * given predicate, returning a counter example if one is found.
   * @param startState the state from which to start the search
   * @param checkState the given predicate/condition
   * @return Some[State] if a counter example (a state for which checkState
   *         returns true) is found, None otherwise
   */
  def checkNever(startState: State, checkState: State => Boolean): Option[State]

  /**
   * Checks that a condition/predicate is guaranteed to eventually be satisfied
   * by a state reachable from the start state.
   * @param startState the state from which to start the search
   * @param checkState the given predicate/condition
   * @return
   */
  def checkEventually(startState: State, checkState: State => Boolean): Boolean

  /**
   * Prints the state graph.
   */
  def show(): Unit
}
