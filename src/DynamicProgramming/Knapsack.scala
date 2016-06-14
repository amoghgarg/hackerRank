package DynamicProgramming

/**
  * https://www.hackerrank.com/challenges/unbounded-knapsack
  * Given a set of values and MAX value, find the maximum number(smaller or equal to MAX) which can be formed by sum of values from the set
  */
object Knapsack {

  def main(args: Array[String]) = {

    var testCount = readInt()

    (1 to testCount).foreach(i => {

      val meta = readLine().split(" ").map(_.toInt)
      val (count, maxSum) = (meta(0), meta(1))

      val numbers = readLine.split(" ").map(_.toInt).distinct

      val sumPossible = (0 to maxSum).map(i => i == 0).toArray

      (0 to maxSum-1).foreach(current => {

        if(sumPossible(current)){

          numbers.map(num => {
            val test = current + num
            if(test < maxSum + 1){
              sumPossible.update(test, true)
            }
          })
        }
      })

      println(maxSum - sumPossible.reverse.indexOf(true))



    })

  }
}
