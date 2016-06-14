package DynamicProgramming


/**
 * https://www.hackerrank.com/challenges/dice-path
 * Can rotate only right or down.
 * Initial Dice: Top = 1,Front = 2, Left = 3
 *
 * Have to reach (M, N)  -> This means that have to take M downs and N rights
 * Each step means either right or left
 * Start with one path -> (1, 1), sum = 1. Start with another path.
 * For the list of the path
 *  Add a new path
 *    not allowed cases => when the thing is alread M or equal to N.
 *      At most we will have 2 * (M + N) paths.
 *
 *
 *
 */
object DicePath {

  case class Dice(
                   top: Int = 1,
                   front: Int = 2,
                   left: Int = 3
                   )

  case class Path(
                   x: Int,
                   y: Int,
                   sum: Int,
                   dice: Dice
                   )

  def rollDice(dice: Dice, direction: String): Dice = {

    direction match {
      case "down" => {
        Dice(
          top = 7 - dice.front,       //old back
          front = dice.top,           //old top
          left = dice.left            //old left
        )
      }
      case _ => {
        //right case
        Dice(
          top = dice.left,          //old left
          front = dice.front,       //old front
          left = 7 - dice.top       //old front
        )
      }
    }
  }


  def maxSumPath(M: Int, N: Int): Int = {

    val origPath = Path(x = 1, y = 1, sum = 1, dice = Dice())

    val endPaths = (1 to M+N-2).foldLeft(List(origPath))((paths, i) => {

      //Create roll down paths ->  M, x increases
      val newDownPaths = paths.filter(p => p.x < M).map(p => {
        val newDice = rollDice(p.dice, "down")
        Path(
          x = p.x + 1,
          y = p.y,
          sum = p.sum + newDice.top,
          dice = newDice
        )
      })

      //Create roll right paths, -> N, y increase
      val newRightPaths = paths.filter(p => p.y < N).map(p => {
        val newDice = rollDice(p.dice, "right")
        Path(
          x = p.x,
          y = p.y + 1,
          sum = p.sum + newDice.top,
          dice = newDice
        )
      })

      val allNewPaths = newRightPaths ::: newDownPaths
      val len = allNewPaths.size
      println(s"Number of paths: $len")

      val grpPaths = allNewPaths.groupBy(p => (p.x, p.y))   // For each of these groups, find the max, then remove the paths with sum < max -6, grp again with x,y,dice
      val filPaths = grpPaths.flatMap(grp => {
          val paths = grp._2
          val grpmax = paths.foldLeft(paths(0).sum)((max  , path) => math.max(max, path.sum))
          val worthItPaths = paths.filter(p => grpmax - p.sum < 7)  //contains the worhtit paths on the same posiion
//          worthItPaths.foreach(println)

          val diceGrped = worthItPaths.groupBy(p => p.dice)
          diceGrped.map(grp => {
            val list = grp._2
            list.foldLeft(list(0))((maxpath, path) => {
              if(maxpath.sum > path.sum) maxpath
              else path
            })
          }).toList

        }).toList
      val ln = filPaths.size
      println(s"Number of filtered paths: $ln")
      filPaths
    })
    //    println(endPaths)
    endPaths.foldLeft(endPaths(0).sum)((max, path) => math.max(max, path.sum))
  }


  def main(args: Array[String]): Unit = {

    val turns = readInt()

    val result = (1 to turns).foldLeft((0L, 0L))( (times,i) => {

      val t0 = System.currentTimeMillis()
      val numbers = readLine().split(" ").map(_.toInt)
      val readTime = times._1 + System.currentTimeMillis() - t0
      val M = numbers(0)
      val N = numbers(1)

      val t1 = System.currentTimeMillis()
      maxSumPath(M, N)
      val calcTime = times._2 + System.currentTimeMillis() - t1

      (readTime, calcTime)

    })

    println(result)

//    result.foreach(println(_))

  }


}
