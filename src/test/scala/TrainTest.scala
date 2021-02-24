import org.scalatest.FunSuite
import Utils.Utils._
import trains.{Routes, Town}

class TrainTest extends FunSuite {
  test("createGraph"){
    assert(createGraph("AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")._2.map(x=> x.name).sortWith(_<_)
      ==List("A","B","C","D","E"))
  }

  test("findRouteDistanceBetweenTowns"){
    val createdGraph = createGraph("AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")
    val graph:Routes = createdGraph._1
    val townList = createdGraph._2
    val a = townList.filter(x=>x.name.equals("A"))(0)
    val b = townList.filter(x=>x.name.equals("B"))(0)
    val c = townList.filter(x=>x.name.equals("C"))(0)
    val d = townList.filter(x=>x.name.equals("D"))(0)
    val e = townList.filter(x=>x.name.equals("E"))(0)
    val routes1 = Seq[Town](a,b,c)
    val routes3 = Seq[Town](a,d,c)
    assert(graph.findRouteDistanceBetweenTowns(routes1)==9)
    assert(graph.findRouteDistanceBetweenTowns(routes3)==13)
    assert(graph.findRouteDistanceBetweenTowns(Seq[Town](a,e,b,c,d))==22)
  }

  test("findnumberOfRoutesBetweenTowns"){
    val createdGraph = createGraph("AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")
    val graph:Routes = createdGraph._1
    val townList = createdGraph._2
    val a = townList.filter(x=>x.name.equals("A"))(0)
    val c = townList.filter(x=>x.name.equals("C"))(0)
    assert(graph.findnumberOfRoutesBetweenTowns(c, c, 3)==2)
    assert(graph.findnumberOfRoutesBetweenTowns(a, c, 4)==3)
  }
  test("shortestPathBetweenTowns"){
    val createdGraph = createGraph("AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")
    val graph:Routes = createdGraph._1
    val townList = createdGraph._2
    val a = townList.filter(x=>x.name.equals("A"))(0)
    val c = townList.filter(x=>x.name.equals("C"))(0)
    assert(graph.shortestPathBetweenTowns(a,c)==9)
  }
  test("numRoutesWithinDistance"){
    val createdGraph = createGraph("AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")
    val graph:Routes = createdGraph._1
    val townList = createdGraph._2
    val a = townList.filter(x=>x.name.equals("A"))(0)
    val c = townList.filter(x=>x.name.equals("C"))(0)
    assert(graph.numRoutesWithinDistance(c, c, 30)==7)
  }
}
