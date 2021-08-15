package src

/**
 * Class that represents a state and contains data.
 * @param id identifier for a State
 * @param data a map of state data, from variable to value
 */
class State(val id: String, val data: Map[String, String]) {

  override def equals(that: Any): Boolean = that match {
    case that: State => this.id.equals(that.id)  && this.data.equals(that.data)
    case _ => false
  }

  override def toString = s"State($id)"
}