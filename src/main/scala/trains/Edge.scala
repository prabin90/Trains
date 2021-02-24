/**
 * edge implementation, contains the start and destination nodes,
 * weight and the next node
 */

package trains

class Edge(var origin: Town, var destination: Town, var weight: Int) {
  this.next = null
  var next: Edge = null

  def this(origin: Town, destination: Town) {
    this(origin, destination, Integer.MAX_VALUE)
  }

  def next(edge: Edge): Edge = {
    this.next = edge
    this
  }

  def getWeight: Double = weight

  override def toString: String = origin + "to" + destination
}
