package Greedy

/**
 * https://www.hackerrank.com/challenges/flowers
 * Input
 * N K
 * c1 c2 .... cN
 *
 * N = flowers, K = friends
 * ci = cost of the flower
 */

object Flowers {

  def main(args: Array[String]) = {

    val first = readLine().split(" ").map(_.toInt)
    val N = first(0)
    val K = first(1)
    val costs = readLine().split(" ").map(_.toInt)
    scala.util.Sorting.quickSort(costs)

    val total = costs.reverse.zipWithIndex.foldLeft(0)((cost, c) => {

      val thisCost = c._1 * (1 + (c._2/K))

      cost + thisCost

    })

    println(total)


  }

}
