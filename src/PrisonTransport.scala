/**
 * https://www.hackerrank.com/challenges/prison-transport
 * Make a graph and find the number of connected groups
 **/
object PrisonTransport {

  def visit(graph: Array[List[Int]], visited: Array[Int], vertex: Int, id: Int): Array[Int] = {

    //for each edge from vertext, vist the edge if not visited.

    val edges = graph(vertex)
    edges.foreach(i => {
      if(visited(i) < 0){
        visited.update(i, id)
        visit(graph, visited, i, id)
      }
    })
    visited
//    edges.foldLeft(visited)((visitedTemp, i) => {
//      if(visitedTemp(i) < 0){
//        val newVisited = visitedTemp.updated(i, id)
//        visit(graph, newVisited, i, id)
//      }else{
//        visitedTemp
//      }
//    } )

  }

  def numberOfTrees(graph: Array[List[Int]]): List[Int] = {

    val vertices = graph.length
    val visited = (1 to vertices).map(i => -1).toArray

    val res = (0 to vertices-1).foreach(i => {
      if(visited(i) < 0){
        visited.update(i, i)
        visit(graph, visited, i, i)
      }
    })

//    println(res)

    val count = visited.groupBy(i => i)

//    println(count)
    count.toList.map(x => x._2.length)


  }

  def main(args: Array[String]): Unit = {

    val vertices = readInt()
    val edges = readInt()

    val graph:  Array[List[Int]] = (1 to vertices).map(i => List()).toArray

    (1 to edges).foreach(i => {

      val x = readLine().split(" ").map(x => x.toInt)
      val a = x(0) - 1
      val b = x(1) - 1

      val updatedA = graph(a)++List(b)
      val updatedB = graph(b)++List(a)

      graph.update(a, updatedA)
      graph.update(b, updatedB)


    })

//    println(graph(0))
//    println(graph(1))
//    println(graph(2))

    val count = numberOfTrees(graph)

    val cost = count.foldLeft(0)((cost, k2) =>
      cost + math.ceil(math.sqrt(k2)).toInt
    )

    println(cost)



  }


}
