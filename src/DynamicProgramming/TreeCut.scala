package DynamicProgramming

/**
  * https://www.hackerrank.com/challenges/cuttree
  */
object TreeCut {

  case class SubTree(
    val incomingVer : List[Int],
    val seenUptil: Int
  )

  var inTime = 0
  var times = Array(0)
  var explored = Array(false)


  def explore(adjList: Array[List[Int]], root: Int): Unit = {

    explored.update(root, true)
    times.update(root, inTime)
    inTime = inTime + 1
    val neighbours = adjList(root)
    neighbours.foreach(v => {
      if(!explored(v)){
        explore(adjList, v)
      }
    })
  }

  def main(args: Array[String]): Unit = {


    val meta = readLine().split(" ").map(_.toInt)

    val vertices = meta(0)
    val K = meta(1)

    val adjList = (1 to vertices).map(i => List(): List[Int]).toArray

    (1 to vertices-1).foreach(i => {

      val temp = readLine().split(" ").map(_.toInt)
      val (a, b) = (temp(0)-1, temp(1)-1)

      adjList.update(a, adjList(a) :+ b)
      adjList.update(b, adjList(b) :+ a)
    })

    (0 to vertices-1).foreach(ind => {
      adjList.update(ind, adjList(ind).sorted)
    })

    times = (0 to vertices-1).map(i => -10).toArray
    explored = (0 to vertices-1).map(i => false).toArray
    explore(adjList, 0)

    println(times.mkString(" "))

    var vertexOrder = times.zipWithIndex.map(_._2)


    var validSubTreeCnt = BigInt(0)

    vertexOrder.foreach(startVertex =>{

      val initNeighbourList = adjList(startVertex)
      val initSeenUptil = if(startVertex < initNeighbourList.head){
        -1
      }else if(startVertex > initNeighbourList.last){
        initNeighbourList.length
      }else{
        initNeighbourList.indexWhere(x => x > startVertex) - 1
      }

      var subTrees = List(SubTree(incomingVer = initNeighbourList, seenUptil = initSeenUptil))
      if(initNeighbourList.size <= K) validSubTreeCnt = validSubTreeCnt + 1

      while(subTrees.nonEmpty){

        val newList = subTrees.flatMap(subTree => {

          var incoming = subTree.incomingVer
          var lookFrom = subTree.seenUptil + 1


          if(incoming.length > 0) {
            var maxIncoming = incoming.last
            (lookFrom to incoming.length - 1).map(ind => {

              val vertexToAdd = incoming(ind)

              val newInComing = incoming.take(ind) ++ incoming.drop(ind + 1) ++ adjList(vertexToAdd).filter(x =>times(x) > times(maxIncoming))
              if (newInComing.size <= K) validSubTreeCnt = validSubTreeCnt + 1

              SubTree(newInComing, ind - 1)


            }).toList
          }else{
            List()
          }

        })


        subTrees = newList
      }
    })



  println(validSubTreeCnt + 1)      //1 for the empty array


  }



}
