package Search

/**
  * https://www.hackerrank.com/challenges/connected-cell-in-a-grid
  * Given m x n grid of ones and zeros. Find the largest connected region on ones.
  */
object ConnectedCell {

  var matrix: Array[Array[Int]] = Array()
  var explored: Array[Array[Boolean]] = Array()
  var markMatrix: Array[Array[Int]] = Array()


  var rows = 0
  var cols = 0

  def explore(x: Int, y: Int, mark: Int ): Unit ={

    explored(x)(y) = true
    markMatrix(x)(y) = mark

    //neighbours: (x-1, y) (x, y-1) ( x+1, y) (x, y+1)

    if(x-1 > -1 && !explored(x-1)(y) && matrix(x-1)(y) == 1) explore(x-1, y, mark)
    if(y-1 > -1 && !explored(x)(y-1) && matrix(x)(y-1) == 1) explore(x, y-1, mark)
    if(x+1 < rows && !explored(x+1)(y) && matrix(x+1)(y) == 1 ) explore(x+1, y, mark)
    if(y+1 < cols && !explored(x)(y+1) && matrix(x)(y+1) == 1) explore(x, y+1, mark)

    if(x-1 > -1 && y-1 > -1 && !explored(x-1)(y-1) && matrix(x-1)(y-1) == 1) explore(x-1, y-1, mark)
    if(x+1 < rows && y-1 > -1 && !explored(x+1)(y-1) && matrix(x+1)(y-1) == 1) explore(x+1, y-1, mark)
    if(x+1 < rows && y+1 < cols && !explored(x+1)(y+1) && matrix(x+1)(y+1) == 1 ) explore(x+1, y+1, mark)
    if(x-1 > -1 && y+1 < cols && !explored(x-1)(y+1) && matrix(x-1)(y+1) == 1) explore(x-1, y+1, mark)



  }


  def main(args: Array[String]) = {

    rows = readInt()
    cols = readInt()

    matrix = Array.ofDim[Int](rows, cols)
    markMatrix = Array.ofDim[Int](rows, cols)
    explored = Array.ofDim[Boolean](rows, cols)


    (0 to rows-1).foreach(rowInd => {

      var row = readLine().split(" ").map(_.toInt)
      matrix(rowInd) = row

    })

    matrix.foreach(r => {
      r.foreach(i => print(s"$i "))
      print("\n")
    })

    println("------------")

    (0 to rows-1).foreach(r => {
      (0 to cols-1).foreach(c => {

        if(!explored(r)(c) && matrix(r)(c) == 1 ){
          explore(r, c, (r)*cols + c + 1)
        }

      })
    })

    markMatrix.foreach(r => {
      r.foreach(i => print(s"$i "))
      print("\n")
    })

    val groupSizes = markMatrix.flatten.groupBy(k => k).filter(pair => pair._1 > 0).map(pair => pair._2.size)

    println(groupSizes.max)


  }

}
