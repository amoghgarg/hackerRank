package Greedy

/**
  * https://www.hackerrank.com/challenges/angry-children
  *
  * From given N numbers, choose K such that maxK - minK is the least
  */
object AngryChildren {

  def main(args: Array[String]) = {

    val N = readInt
    val K = readInt

    val numbers = (1 to N).map(i => readInt()).sorted

    var minDiff = numbers(N-1)

    (K-1 to N-1).foreach(i => {

      val tempDiff = numbers(i) - numbers( i - (K-1) )

      minDiff = math.min(tempDiff, minDiff)

    })

    println(minDiff)


  }

}
