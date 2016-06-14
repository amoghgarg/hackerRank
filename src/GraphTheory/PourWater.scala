package GraphTheory

/**
  * Given three jugs of volume 4, 7 and 10L.
  * 4 and 7L jugs are filled completely with water.
  * At each step one jug can be emptied in another jug if the second jug can hold the water.
  *
  * Find if you can get 2L in any jug
  * And print out how.
  *
  *
  * Solution;:
  * Represent each state by a, b, c (the volume of water in each jug). An edge is to another possible state.
 */

object PourWater{

  val capacityA = 4
  val capacityB = 7
  val capacityC = 10
  val requiredWater = 2

  val exploredMap: scala.collection.mutable.Map[State, State] = scala.collection.mutable.Map()


  def getNeighbours(node: State):List[State] = {

    //a to b
    val n1 = if(node.b + node.a <= capacityB){
      Some(State(0, node.a+node.b, node.c))
    }else{
      Some(State(node.a+node.b - capacityB, capacityB, node.c))
    }
    //a to c
    val n2 = if(node.c + node.a <= capacityC){
      Some(State(0, node.b, node.c+node.a))
    }else{
      Some(State(node.c+node.a - capacityC, node.b, capacityC))
    }
    //b to a
    val n3 = if(node.b + node.a <= capacityA){
      Some(State(node.a+node.b, 0, node.c))
    }else{
      Some(State(capacityA, node.a+node.b - capacityA, node.c))
    }
    //b to c
    val n4 = if(node.b + node.c <= capacityC){
      Some(State(node.a, 0, node.b+node.c))
    }else{
      Some(State(node.a, node.b+node.c - capacityC, capacityC))
    }
    //c to a
    val n5 = if(node.c + node.a <= capacityA){
      Some(State(node.a+node.c, node.b, 0))
    }else{
      Some(State(capacityA, node.b, node.a+node.c - capacityA))
    }
    //c to b
    val n6 = if(node.b + node.c <= capacityB){
      Some(State(node.a, node.c+node.b, 0))
    }else{
      Some(State(node.a, capacityB, node.c+node.b - capacityB))
    }

    List(n1, n2, n3, n4, n5, n6).flatten

  }

  def runDFS(node: State, parent: State): Unit = {



    //Add to the hash
    //For the given start vertex
    //    For each of the neighbouring vertices of this vertex
    //        If the vertex is present in the map -> Do nothing
    //        Else -> Explore the vertex.


    //Adding to the hash
    exploredMap.+=((node, parent))
    exploredMap.foreach(println)
    println("=========================")


    //Finding the possible vertices
    val neighbours = getNeighbours(node)
    val notVisitedNeighbours = neighbours.filter(neighbour => {
      val x = exploredMap.get(neighbour)
      x.isEmpty
    })

    val shouldStop = notVisitedNeighbours.filter(neighbour => {
      neighbour.a == requiredWater || neighbour.b == requiredWater || neighbour.c == requiredWater
    })

    if(shouldStop.size > 0){
      exploredMap.+=((shouldStop.head, node))
    }else {
      notVisitedNeighbours.map(neighbourNode => {
        runDFS(neighbourNode, node)
      })
    }

  }


  case class State(a: Int, b:Int, c:Int)   //Amount of water in jugs A, B and C

  def main(args: Array[String]) = {

    val initState = State(4, 7, 0)
    runDFS(initState, initState)

    exploredMap.foreach(println)


  }


}


// Solution for (4,7, 0) in (4, 7, 10) capacity jugs
// 4,2,5 <- 0, 6, 5 <- 4, 6, 1 <- 3, 7, 1 <- 3, 0, 8 <- 0, 3, 8 <- 4,3,4, <- 0,7,4 <- 4,7,0
