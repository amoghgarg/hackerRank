package DynamicProgramming

import scala.collection.mutable

/**
  * https://www.hackerrank.com/challenges/maxsubarray
  */
object FarVertices {



  var adjList: Array[Array[Int]] = Array()
  var vertices = 0

  def distances(root: Int) = {

    val heap = mutable.Queue[Int]()
    heap.enqueue(root)
    val explored = (1 to vertices).map(i => -10).toArray
    explored.update(root, 0)

    while(heap.nonEmpty){

      val current = heap.dequeue
      val nextLevel = explored(current) + 1

      val neighbours = adjList(current)

      neighbours.filter(v => explored(v) < 0).map(v => {
        explored.update(v, nextLevel)
        heap.enqueue(v)
      })
    }

    explored
  }

  def main(args: Array[String]): Unit = {

    var explored: Array[Int] = Array()


    val meta = readLine().split(" ").map(_.toInt)
    vertices = meta(0)
    val maxDist = meta(1)

    adjList = (1 to vertices).map(i => Array(): Array[Int]).toArray

    (1 to vertices-1).foreach(ind => {

      val temp = readLine().split(" ").map(_.toInt)
      val (a, b) = (temp(0)-1, temp(1)-1)

      adjList.update(a, adjList(a) :+ b)
      adjList.update(b, adjList(b) :+ a)

    })

    val allDistances = (0 to vertices-1).map(distances(_))

//    allDistances.foreach( r => println(r._1.mkString(" ")))
//
//    val distanceMirrored = allDistances.map(row => {
//      val ind = row._2
//      val distances = row._1
//      distances.slice(0, ind+1)
//    })
//
//    distanceMirrored.foreach( r => println(r.mkString(" ")))
//
//
//    val distaneMirroredMax = distanceMirrored.map(row => row.max)
//
//
//
//    val okVertices = distaneMirroredMax.filter(d => d <= maxDist)
//
//    println(vertices - okVertices.length)

    val farCount = allDistances.map(row => row.filter(d => d > maxDist).size).toArray

    //while(the max in farCount is not zero){
    //    Get the indice with max,and remove this vertex. Subtracting the count for vertices which had contact with this vertex
    // }

    val ind = 0
    var finished = false

    while(!finished){

      val currentVertex = farCount.zipWithIndex.maxBy(_._1)._2
      val currentFarCount = farCount(currentVertex)

      if(currentFarCount == 0){
        finished = true
      }else{
        //removing the current vertex
        farCount.update(currentVertex, -1)

        (0 to vertices-1).foreach(v => {

          if( allDistances(v)(currentVertex) > maxDist){
            val farCountForV = farCount(v)
            farCount.update(v, farCountForV - 1)
          }


        })
      }


    }

    val vertexOk = farCount.filter(_ == 0).size

    println(vertices - vertexOk)


  }
}
