package Search

/**
  * https://www.hackerrank.com/challenges/cut-the-tree
  */
object CutTheTree {

  var adjList: Array[Array[Int]] = Array()

  var explored: Array[Boolean] = Array()
  var sizes: Array[Int] = Array()
  var totalSize = 0

  var globalCount = 0
  var globalMin = 0

  def explore(toExplore: Int): Unit = {

    if(!explored(toExplore)){

      explored.update(toExplore, true)
      globalCount = globalCount + sizes(toExplore)

      val neighbours = adjList(toExplore).filter(ver => !explored(ver))

      neighbours.map( ver => {
        val startValue = globalCount
        explore(ver)
        val endValue = globalCount
        val halfTree = endValue - startValue
        val otherhalfTree = totalSize - halfTree
        val diff = math.abs(otherhalfTree - halfTree)

//        println(s"vert: $toExplore half: $halfTree other: $otherhalfTree")

        globalMin = math.min(globalMin, diff)
      })

    }

  }

  def main(args: Array[String]) = {

    val vertices = readInt()

    sizes = readLine().split(" ").map(v =>{
      val temp = v.toInt
      totalSize = totalSize + temp
      temp
    })

    globalMin = totalSize

    adjList = (1 to vertices).map(i => Array(): Array[Int]).toArray


    (1 to vertices-1).foreach(i =>{

      val meta = readLine().split(" ").map(_.toInt)

      val (a, b) = (meta(0)-1, meta(1)-1)

      adjList.update(a, adjList(a) :+ b)
      adjList.update(b, adjList(b) :+ a)

    })

    explored = (1 to vertices).map(i => false).toArray

    explore(0)

    println(globalMin)


  }

}
