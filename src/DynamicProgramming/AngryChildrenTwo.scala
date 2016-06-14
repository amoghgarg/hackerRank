package DynamicProgramming

/**
  * https://www.hackerrank.com/challenges/angry-children-2
  */
object AngryChildrenTwo {

  def main(args: Array[String]) = {

    val N = readInt()
    val K = readInt

    val numbers = (1 to N).map(i => BigInt(readInt)).sorted

    var minDiff: BigInt = 0;
    (1 to K).foreach(ind =>{
      minDiff = minDiff +  BigInt(( (1-K) + 2*(ind - 1) ))*numbers(ind - 1)
    })
    var prevDiff: BigInt = minDiff

    var middleSum: BigInt = 0
    (1 to K-1).foreach(ind => middleSum = middleSum + numbers(ind))



//    println(numbers.mkString(" "))

    println(s"startInd: 0")
    println(s"middleSum: $middleSum")
    println(s"prevDiff: $prevDiff")
    println("-------------")

    var minStartInd = 0;

    (1 to N - K).foreach(startInd => {

      val toAdd: BigInt = ( numbers(startInd-1) + numbers(startInd + K - 1) )*BigInt((K-1))
      val toRemove: BigInt = middleSum + middleSum

      prevDiff = prevDiff + toAdd - toRemove

      if( prevDiff < minDiff){
        minDiff = prevDiff
        minStartInd = startInd
      }

      middleSum = middleSum - numbers(startInd) + numbers(startInd + K - 1)

//      println(s"startInd: $startInd")
//      println(s"middleSum: $middleSum")
//      println(s"prevDiff: $prevDiff")
//      println("-------------")
    })
//
//    var sum = BigInt(0)
//    (1 to K).foreach(ind =>{
//      sum = sum + numbers(minStartInd + ind - 1) * BigInt( ( (1-K) + 2*(ind - 1) ) )
//    })

    println(minDiff)

  }

}
