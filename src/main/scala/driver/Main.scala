package driver

import trains.{Edge, Routes, Town}
import Utils.Utils._
object Main extends App {

  /**
   * input should be like
   * AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7
   * it should be a single line seperated with comma and spaces, each value represent an edge
   */
  val graphString = scala.io.StdIn.readLine()
  val createdGraph = createGraph(graphString)
  val graph:Routes = createdGraph._1
  val townList = createdGraph._2

  val a = townList.filter(x=>x.name.equals("A"))(0)
  val b = townList.filter(x=>x.name.equals("B"))(0)
  val c = townList.filter(x=>x.name.equals("C"))(0)
  val d = townList.filter(x=>x.name.equals("D"))(0)
  val e = townList.filter(x=>x.name.equals("E"))(0)
  try {
      //1. Distance of route A-B-C
      val routes1 = Seq[Town](a,b,c)
      println("Distance of route A-B-C: "+ graph.findRouteDistanceBetweenTowns(routes1).toString )
      //2. Distance of route A-D
      val routes2 = Seq[Town](a,d)
      println("Distance of route A-D : " + graph.findRouteDistanceBetweenTowns(routes2))
      //3. Distance of route A-D-C
      val routes3 = Seq[Town](a,d,c)
      println("Distance of route A-D-C : " + graph.findRouteDistanceBetweenTowns(routes3))
      //4. Distance of route A-E-B-C-D
      val routes4 = Seq[Town](a,e,b,c,d)
      println("Distance of route A-E-B-C-D  :" + graph.findRouteDistanceBetweenTowns(routes4))
      //5. Distance of route A-E-D
      val routes5 = Seq[Town](a,e,d)
      println("Distance of route A-E-D : " + graph.findRouteDistanceBetweenTowns(routes5))
  }
  catch{
    case e:Exception => println(e.getMessage)
  }
  //6. Number of trips starting at C,ending at C with 3 stops
  println("Number of trips starting at C,ending at C with 3 stops :" +
    graph.findnumberOfRoutesBetweenTowns(c, c, 3))
  //7. Number of trips starting at A,ending at C with 4 stops
  println("Number of trips starting at A,ending at C with 4 stops : " +
    graph.findnumberOfRoutesBetweenTowns(a, c, 4))
  //8.The length of the shortest path from A to C.
  println("The length of the shortest path from A to C :" + graph.shortestPathBetweenTowns(a, c))
  //9.The length of the shortest path from B to B.
  println("The length of the shortest path from B to B :" + graph.shortestPathBetweenTowns(b, b))
  //10.The number of different routes from C to C with a distance of less than 30
  println("The number of different routes from C to C with a distance of less than 30 :" + graph.numRoutesWithinDistance(c, c, 30))

}
