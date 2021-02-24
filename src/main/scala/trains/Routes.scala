/**
 * Graph implementation of the routing problem
 */

package trains
import trains._

import scala.collection.mutable
import scala.util.control.Breaks._
import scala.collection.mutable.HashMap


class Routes() {
  var routingTable = new mutable.HashMap[Town,Edge]()

  def getRoutingTable: mutable.HashMap[Town,Edge] = routingTable

  /**
   * takes list of towns in an order and find the distance traversing through each town
   * if a route is nt found will throw NoSuchRouteException
   * @param List of towns
   * @throws NoSuchRouteException
   * @return distance between towns: integer
   */
  @throws[NoSuchRouteException]
  def findRouteDistanceBetweenTowns(towns: Seq[Town]): Int = {
    if (towns.size < 2) return 0
    var distance = 0
    var depth = 0
    var index = 0
    while (index < towns.size-1) {
      if (this.routingTable.contains(towns(index))) {
        var route = this.routingTable.get(towns(index)) match{
          case Some(x) => x
          case None => null
        }
        breakable {
          while (route != null) {
            if (route.destination.name.equals(towns(index + 1).name)) {
              distance += route.weight
              depth += 1
              break //todo: break is not supported
            }
            route = route.next
          }
        }
      }
      else throw new NoSuchRouteException
      index += 1
    }
    if (depth != towns.size - 1) throw new NoSuchRouteException
    distance
  }

  /**
   * find total no of routes between towns with stops <= limit
   * @param origin town
   * @param destination town
   * @param limit integer
   * @return no of routes having stops count <= limit
   */
  def findnumberOfRoutesBetweenTowns(origin: Town, destination: Town, limit: Int): Int = findRoutes(origin, destination, 0, limit)

  /**
   * helper function of findnumberOfRoutesBetweenTowns
   * @param origin
   * @param dest
   * @param depth
   * @param limit
   * @return
   */
  def findRoutes(origin: Town,dest: Town,depth: Int,limit: Int): Int = {
    var routes = 0
    var depth1 = depth
    if (this.routingTable.contains(origin) && this.routingTable.contains(dest)) {
      if (depth1 >= limit) return 0
      depth1+=1
      origin.visited = true
      var edge = this.routingTable.get(origin) match{
        case Some(x) => x
        case None => null
      }
      while (edge != null) {
          if (edge.destination.name.equals(dest.name)) {
            routes += 1
            depth1+=1
          }
          else if (!edge.destination.visited) {
            routes += findRoutes(edge.destination, dest, depth1, limit)
          }
        edge = edge.next
      }
    }
    else noRouteException()
    origin.visited = false
    routes
  }

  /**
   * find shortest path between two towns
   * @param origin
   * @param destination
   * @return shortest path
   */
  def shortestPathBetweenTowns(origin: Town, destination: Town): Int = findShortestPath(origin, destination, 0, 0)

  /**
   * helper function for shortestPathBetweenTowns
   * @param origin
   * @param dest
   * @param distance
   * @param shortestPath
   * @return
   */
  def findShortestPath(origin: Town, dest: Town, distance: Int, shortestPath: Int): Int = {
    var distance1 = distance
    var shortestPath1 = shortestPath
    if (this.routingTable.contains(origin) && this.routingTable.contains(dest)) {
      origin.visited = true
      var edge = this.routingTable.get(origin) match{
        case Some(x) => x
        case None => null
      }
      while (edge != null) {
        if (edge.destination.name.equals(dest.name) || !edge.destination.visited)
          distance1 += edge.weight
        if (edge.destination.name.equals(dest.name)) {
          if (shortestPath1 == 0 || distance1 < shortestPath1)
            shortestPath1 = distance1
          origin.visited = false
          return shortestPath1
        }
        else if (!edge.destination.visited) {
          shortestPath1 = findShortestPath(edge.destination, dest, distance1, shortestPath1)
          distance1 -= edge.weight
        }
        edge = edge.next
      }
    }
    else noRouteException()
    origin.visited = false
    shortestPath1
  }

  /**
   * find no of all routes between two towns within given max distance
   * @param origin
   * @param dest
   * @param maxDistance
   * @return
   */
  def numRoutesWithinDistance(origin: Town, dest: Town, maxDistance: Int): Int = findAllRoutesBetweenTowns(origin, dest, 0, maxDistance)

  def findAllRoutesBetweenTowns(origin: Town, destination: Town, weight: Int, maxDistance: Int):Int = {
    var weight1 = weight
    var routes = 0
    if (this.routingTable.contains(origin) && this.routingTable.contains(destination)) {
      var edge = this.routingTable.get(origin) match{
        case Some(x) => x
        case None => null
      }
      while (edge != null) {
        weight1 += edge.weight
        if (weight1 <= maxDistance) {
          if (edge.destination.name.equals(destination.name)) {
            routes += 1
            routes += findAllRoutesBetweenTowns(edge.destination, destination, weight1, maxDistance)
          }
          else {
            routes += findAllRoutesBetweenTowns(edge.destination, destination, weight1, maxDistance)
            weight1 -= edge.weight
          }
        }
        else weight1 -= edge.weight
        edge = edge.next
      }
    }
    else noRouteException()
    routes
  }

  def noRouteException(): Unit = {
    System.out.println("NO SUCH ROUTE")
  }
}

