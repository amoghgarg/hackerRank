package DynamicProgramming

/**
 * https://www.hackerrank.com/challenges/reverse-factorization
 * Input: N, count
 * the factors
 *
 * Output: the smallest possible steps to reach N
 *
 */
object ReverseFactorization {

  case class Path(
    list: List[Int],      //List of states till now -> 1, i ...
    index: Int
  )

  def makeNewPaths(paths: List[Path], factors: List[Int]) = {

    val newPaths = paths.flatMap(path => {

      val factorsToConsider = factors.splitAt(path.index)._2

      factorsToConsider.zipWithIndex.map( f => {

        Path(
          list = path.list ::: List(path.list.last * f._1),
          index = path.index + f._2
        )

      })
    })

    newPaths
    //val grouped = newPaths.groupBy( p => p.list.last
  }


  def main(args: Array[String]) = {

    val first = readLine().split(" ").map(_.toInt)
    val N = first(0)
    val count = first(1)

    val factorsUnsorted = readLine().split(" ").map(_.toInt).filter( p => N % p == 0)
    scala.util.Sorting.quickSort(factorsUnsorted)
    val factors = factorsUnsorted.toList

    val origPaths = factors.zipWithIndex.map( i => {
      Path(
        list = List(1, i._1),
        index = i._2
      )
    })

    var prevPaths = origPaths
    var done = false

    while(!done){
      var newPaths = makeNewPaths(prevPaths, factors)
      prevPaths = newPaths

      val worthIt = newPaths.filter( p => p.list.last == N)

      if(worthIt.size > 0) {

        println(worthIt(0).list.mkString(" "))
        done = true
      }

      val hopeRemains = newPaths.filter( p => p.list.last < N)

      if(hopeRemains.size == 0){
        println(-1)
        done = true
      }

    }
  }
}
