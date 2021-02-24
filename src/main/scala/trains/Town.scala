/**
 * This is a node representation of a town.
 * It contains a name, a visited flag
 */

package trains

class Town(var name: String) {
  var visited = false

  def getName: String = this.name

  def getVisited: Boolean = this.visited

  override def equals(obj: Any): Boolean = {
    obj match{
      case obj:Town => obj.equals(this)
      case _ => false
    }
    val rhsTown = obj.asInstanceOf[Town]
    this.name == rhsTown.name
  }

  override def hashCode: Int = {
    if (this.name == null) return 0
    this.name.hashCode
  }

  override def toString: String = this.name.toString
}
