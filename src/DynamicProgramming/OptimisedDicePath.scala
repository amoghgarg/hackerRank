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
 * This version will calculate only till the max M, N. And save the intermediate results.
 *
 */
object OptimisedDicePath {

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


  def maxSumPath(M: Int, N: Int) = {

    val origPath = Path(x = 1, y = 1, sum = 1, dice = Dice())
    val globalMapOrig = Map((1,1) -> Dice().top)

    val result = (1 to M+N-2).foldLeft(  (List(origPath),  globalMapOrig) )((results, i) => {

      val paths = results._1
      val optMapTillNow = results._2

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
//      println(s"Number of paths: $len")

      val positionGrped = allNewPaths.groupBy(p => (p.x, p.y))   // For each of these groups, find the max, then remove the paths with sum < max -6, grp again with x,y,dice
      val bestPaths = positionGrped.map(grp => {
          val paths = grp._2
          val grpmax = paths.foldLeft(paths(0).sum)((max  , path) => math.max(max, path.sum))   //This is best sum for this position, add it to the global map.
          val worthItPaths = paths.filter(p => grpmax - p.sum < 7)  //contains the worhtit paths on the same posiion (removing paths with sum < max -7)
//          worthItPaths.foreach(println)

          val diceGrped = worthItPaths.groupBy(p => p.dice)
          val bestDiceGrouped = diceGrped.map(grp => {
            val list = grp._2
            list.foldLeft(list(0))((maxpath, path) => {
              if(maxpath.sum > path.sum) maxpath
              else path
            })
          }).toList

          (bestDiceGrouped, (grp._1 -> grpmax))

          //Adding grpmax to the globalMap. Point = grp._1, sum = grpmax
//          vaoptMapTillNow + (grp._1 -> grpmax)


        }).toList
      val ln = bestPaths.size
//      println(s"Number of filtered paths: $ln")

      val newPaths = bestPaths.flatMap(list => list._1)
      val updatedMap = bestPaths.foldLeft(optMapTillNow)((map, pair) => map + pair._2)
      (newPaths, updatedMap)
    })
    //    println(endPaths)
//    endPaths.foldLeft(endPaths(0).sum)((max, path) => math.max(max, path.sum))
    result._2
  }


  def main(args: Array[String]): Unit = {

    val turns = readInt()
    val emptyList: List[(Int, Int)] = List()

    val (pairs, maxM, maxN) = (1 to turns).foldLeft((emptyList, 0, 0))((triplet, i) => {

      val numbers = readLine().split(" ").map(_.toInt)
      val M = numbers(0)
      val N = numbers(1)

      val maxM = math.max(triplet._2, M)
      val maxN = math.max(triplet._3, N)
      val newList = triplet._1 ::: List((M, N))

      (newList, maxM, maxN)
    })

    val sums = maxSumPath(maxM, maxN)

    pairs.map( pair => {
      println(sums(pair))
    })
  }
}
