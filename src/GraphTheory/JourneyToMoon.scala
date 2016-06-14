package GraphTheory

/**
 * https://www.hackerrank.com/challenges/journey-to-the-moon
 */
object JourneyToMoon {

  var explored: Array[(Int, Int)] = Array()

  def dfsExplore(list: Array[Array[Int]], start: Int, root: Int): Unit = {

    explored.update(start, (start, root) )
    val reachableVertices = list(start)

    reachableVertices.foreach( edge => {

      if(explored(edge)._2 < 0){
        dfsExplore(list, edge, root)
      }

    })
  }

  def main(args: Array[String]) = {

    val meta = readLine().split(" ").map(_.toInt)
    val (vertices, edges) = (meta(0), meta(1))

    val adjList = (1 to vertices).map(i => Array(): Array[Int]).toArray
    (1 to edges).foreach(edge => {
      val temp = readLine().split(" ").map(_.toInt)
      val (a, b) = (temp(0) , temp(1) )

      adjList.update(a, adjList(a) :+ b)
      adjList.update(b, adjList(b) :+ a)

    })

    explored = (0 to vertices - 1).map(i => (i, -i - 1)).toArray

//    explored.foreach(println)

    explored.foreach( valInd => {
      if(valInd._2 < 0){
        dfsExplore(adjList, valInd._1, valInd._1)
      }
    })

//    explored.foreach(println)
    val sizes = explored.groupBy(x => x._2).map(x => x._2.length).toArray
    val sizesCount = sizes.groupBy(x => x).map(i => (i._1, i._2.length)).toArray

    var count = 0 : BigInt
    sizesCount.foreach(println)
    count = sizesCount.foldLeft(0: BigInt)((sum, pair) => {  //pair -> (size, count)
      sum + BigInt(pair._2)*BigInt(pair._2-1)*BigInt(pair._1)*BigInt(pair._1)/2
    })

    if(sizesCount.size > 1){
      (0 to sizesCount.size - 2).foreach(i => {
        (i + 1 to sizesCount.size - 1).foreach(j => {
          count = count + BigInt(sizesCount(i)._1) * BigInt(sizesCount(j)._1) * BigInt(sizesCount(i)._2) * BigInt(sizesCount(j)._2)
        })
      })
    }

    println(count)

  }

}
