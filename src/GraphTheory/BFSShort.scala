package GraphTheory

import scala.collection.mutable

/**
 * https://www.hackerrank.com/challenges/bfsshortreach
 */
object BFSShort {

  def bfs(list: Array[Array[Int]], start: Int): Array[Int] = {

  //for each vertex, init dist = -1
  //init Put start on the Queue

    val vertices = list.length
    val dist = (1 to vertices).map(i => -1).toArray
    dist.update(start, 0)
    val explored = (1 to vertices).map(i => false).toArray

    val queue = mutable.Queue[Int](start)


//    println(queue)

    while(queue.nonEmpty){

      val currentVertex = queue.dequeue()
      val currentVertexDist = dist(currentVertex)

      if(!explored(currentVertex)){
        explored.update(currentVertex, true)
        val edgesFromCurrent = list(currentVertex)
        edgesFromCurrent.foreach(reachableVertex => {
          if(dist(reachableVertex) == -1){
            dist.update(reachableVertex, 6 + currentVertexDist)
          }
          queue.enqueue(reachableVertex)
        })
      }
    }
    dist
  }

  def main(args: Array[String]) = {

    val testCount = readInt()

    (1 to testCount).foreach(i => {

      //read the vertex, edges
      val meta = readLine().split(" ").map(_.toInt)
      val (vertices, edges) = (meta(0), meta(1))

      //read the edges
      val adjList = (1 to vertices).map(i => Array(): Array[Int]).toArray //.toArray
      (1 to edges).foreach(edge => {
        val temp = readLine().split(" ").map(_.toInt)
        val (a, b) = (temp(0) - 1, temp(1) - 1)

        adjList.update(a, adjList(a) :+ b)
        adjList.update(b, adjList(b) :+ a)

      })
      val startLine = (readLine().split(" ").map(_.toInt))// - 1
      val start = startLine(0) - 1

//      adjList.foreach(println)
//      println(start)

      val dist = bfs(adjList, start)

      println(dist.toList.filter(x => x != 0).mkString(" "))
    })

  }

}
