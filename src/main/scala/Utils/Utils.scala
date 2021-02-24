package Utils

import trains.{Edge, Routes, Town}


object Utils {

   def createGraph(graphString:String): (Routes,List[Town]) ={
     val graph = new Routes
     //val graphString="AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7"
     val edgeList = graphString.split("\\s+|,\\s*")
//     println("EdgeList are as follows")
//     edgeList.foreach(println)
     val vertices = edgeList.map(x => Seq(x(0),x(1))).flatten.toSet.toList
//     println("Vertices are as follows")
//     vertices.foreach(println)
     val listOfTown = vertices.map(x=>new Town(x.toString))
     listOfTown.foreach{
        x => {
          val newEdgeList = edgeList.filter(y=>y(0).toString.equals(x.name))
          var edges:Edge = null
          var edgeNext = edges
          for(i <- 0 to newEdgeList.size-1){
            val dest = listOfTown.filter(j=>j.name.equals(newEdgeList(i)(1).toString))(0)
            if(i==0){
              edges = new Edge(x,dest,newEdgeList(i)(2).toString.toInt)
              edgeNext=edges
            }
            else{
              edgeNext.next(new Edge(x,dest,newEdgeList(i)(2).toString.toInt))
              edgeNext = edgeNext.next
            }
          }
          graph.routingTable.put(x,edges)
        }
     }
     (graph,listOfTown)
   }

   def printGraph(graph:Routes): Unit ={
      val routetable:scala.collection.mutable.HashMap[Town,Edge] = graph.getRoutingTable
      routetable.foreach{
        case x->y => {
          var j =y
          println("town is :" + x.name.toString)
          while(j!=null){
            println("edge from "+ j.origin.name + "to " + j.destination.name + " is" + j.weight)
            j=j.next
          }
        }
        case _ => println("Not a graph route")
      }
    }
}
