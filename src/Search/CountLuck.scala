package Search

/**
  * https://www.hackerrank.com/challenges/count-luck
  */
object CountLuck {

  //empty = 0
  //tree = 1
  //cameFromLeft = 2
  //cameFromRight = 3
  //cameFromDown = 4
  //cameFromUp = 5
  //isStart = 6


  var matrix = Array.ofDim[Int](0,0)
  var rows = 0
  var cols = 0
  var endPoint = (0,0)
  var startPoint = (0,0)
  var multiNeighbours = Array.ofDim[Boolean](0,0)

  def backTrack() = {

    var point = endPoint

    var waves = 0

    while(point != startPoint) {

      if(multiNeighbours(point._1)(point._2)) waves = waves + 1

      matrix(point._1)(point._2) match {
        case 2 => point = (point._1-1, point._2)  // Came from left
        case 3 => point = (point._1+1, point._2)  // Came from right
        case 4 => point = (point._1, point._2+1)  // Came from down
        case 5 => point = (point._1, point._2-1)  // Came from up
      }

    }

    if(multiNeighbours(point._1)(point._2)) waves = waves + 1

    waves

  }

  def explore(root: (Int, Int), mark: Int): Unit = {

    val x = root._1
    val y = root._2

    matrix(x)(y) = mark

    var neighbours = 0

    if(x != endPoint._1 || y != endPoint._2){

      //Checking Right
      if(x+1 < rows && matrix(x+1)(y) == 0){
        explore((x+1, y), 2)    //cameFromLeft
        neighbours = neighbours + 1
      }
      if(x-1 > -1 && matrix(x-1)(y)==0 ) {
        //Checking left
        explore((x-1,y), 3)
        neighbours = neighbours + 1
      }
      if(y+1 < cols && matrix(x)(y+1) == 0){
        //Checking down
        explore((x,y+1), 5)
        neighbours = neighbours + 1
      }
      if(y-1 > -1 && matrix(x)(y-1) == 0){
        explore((x,y-1), 4)  //Checking up, came from down
        neighbours = neighbours + 1
      }

    }

    multiNeighbours(x)(y) = neighbours > 1




  }

  def main(args: Array[String]) = {

    val tests = readInt()

    (1 to tests).foreach(testNo => {

      val meta = readLine().split(" ").map(_.toInt)

      rows = meta(0)
      cols = meta(1)

      matrix = Array.ofDim[Int](rows, cols)
      multiNeighbours = Array.ofDim[Boolean](rows, cols)

//      matrix.foreach(l => println(l.mkString(" ")))
//      println("-------------")

      (0 to rows-1).foreach(r => {

        var c = -1

        val line = readLine().map(x => {
          c = c+1
          val num = x match {
            case 'X' => 1
            case '.' => 0
            case '*' => {
              endPoint = (r,c)
              0
            }
            case 'M' => {
              startPoint = (r,c)
              0
            }
          }
          matrix(r)(c) = num
        })
      })

//      matrix.foreach(l => println(l.mkString(" ")))
//      println("-------------")

      explore((startPoint._1, startPoint._2), 6)

//      matrix.foreach(l => println(l.mkString(" ")))
//      println("-------------")

      if (readInt() == backTrack()){
        println("Impressed")
      }else{
        println("Oops!")
      }



    })


  }

}
