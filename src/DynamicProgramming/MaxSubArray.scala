package DynamicProgramming

/**
  * https://www.hackerrank.com/challenges/maxsubarray
  */
object MaxSubArray {

  def main(args: Array[String]) = {

    val tests = readInt()

    (0 to tests-1).foreach(testNo => {

      val count = readInt
      val numbers = readLine.split(" ").map(_.toInt)

      var globalMax = 0
      var maxEndingHere = 0

      (0 to count-1).foreach(ind => {

        val contEndingHere = maxEndingHere + numbers(ind)
        maxEndingHere = math.max(numbers(ind), contEndingHere)

        globalMax = math.max(globalMax, maxEndingHere)

      })



      val positiveNumbers = numbers.filter(x => x > 0)

      val result2 = if(positiveNumbers.nonEmpty){
        positiveNumbers.sum
      }else{
        globalMax = numbers.max
        numbers.max
      }



      println(s"$globalMax $result2")

    })

  }

}
