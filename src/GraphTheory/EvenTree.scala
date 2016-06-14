package GraphTheory

/**
 * https://www.hackerrank.com/challenges/even-tree
 * Given a tree, remove all those edges which will render the number of vertices in the
 * remaining connected components to be even. Output the number of such edges.
 */
object EvenTree {

  var count = 0
  var counts: Array[(Int, Int)] = Array()
  var explored: Array[Boolean] = Array()

  def explore(adjList: Array[Array[Int]], start: Int):Unit = {

    explored.update(start, true)
    count = count + 1
    val startCount = count
    counts.update(start, (count, 0))
    //explore the each unexplored neighbour of the start vertex
    val adjVertices = adjList(start)
    adjVertices.foreach(vert => {
      if(!explored(vert)){
        explore(adjList, vert)
      }
    })
    count = count + 1
    counts.update(start, (startCount, count))
  }

  def isEdgeRemovable(a: Int, b: Int): Boolean = {

    val countA = counts(a)._2 - counts(a)._1 + 1
    val countB = counts(b)._2 - counts(b)._1 + 1
    val minCount = math.min(countA/2, countB/2)   // number of

    if(minCount % 2 == 0){
      true
    }else{
      false
    }

  }

  def main(arg: Array[String]) = {

    val meta = readLine().split(" ").map(_.toInt)
    val (verticeNum, edgeNum) = (meta(0), meta(1))

    if(verticeNum % 2 == 0){
      val edges = (1 to edgeNum).map(i => {
        val temp = readLine().split(" ").map(_.toInt)
        (temp(0), temp(1))
      }).toList

      //make the adjList
      val adjList = (1 to verticeNum).map( i => Array(): Array[Int]).toArray
      edges.foreach(edge => {
        val (a,b) = (edge._1 - 1, edge._2 - 1)
        adjList.update(a, adjList(a) :+ b)
        adjList.update(b, adjList(b) :+ a)
      })

      explored = (1 to verticeNum).map(i => false).toArray
      counts = (1 to verticeNum).map(i => {
        (0,0)
      }).toArray   //keeps the pre and post count of each vertex

      //find a vertec with only one edge.
      val rootVertex = adjList.zipWithIndex.filter( x => x._1.length == 1).head._2

      //start exploring from the above vertex
      explore(adjList, rootVertex)

      counts.foreach(println)

      val removableEdgesCount = edges.filter(edge => {
        val (a,b) = (edge._1 - 1, edge._2 - 1)
        isEdgeRemovable(a, b)
      }).length

      println(removableEdgesCount)
    }
    else{
      println(0)
    }
  }
}
