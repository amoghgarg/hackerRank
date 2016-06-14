package DynamicProgramming

import scala.collection.mutable.HashMap
import scala.math.BigDecimal.RoundingMode

/**
  * https://www.hackerrank.com/challenges/vertical-sticks
  */
object Sticks {

  var factorials: Array[BigInt] = Array()

  val choose: HashMap[(Int, Int), BigInt] = HashMap()

  def getChoose(big: Int, small: Int) = {

    val mapValue = choose.get((big, small))

    if(mapValue.nonEmpty){
      mapValue.get
    }else{
      val result = factorials(big)/ (factorials(small)*factorials(big-small))
      choose += ((big, small) -> result)
      result
    }

  }


  def main(args: Array[String]) = {

    val tests = readInt()

    (1 to tests).foreach(testNo => {

      val count = readInt()

      factorials = Array.ofDim[BigInt](count+1)
      factorials.update(0,1)

      (1 to count).foreach(ind => {
        val prev = factorials(ind-1)
        factorials.update(ind, prev*ind)
      })

      val numbers = readLine().split(" ").map(_.toInt).sorted

      val smallCount = Array.ofDim[Int](count)

      var ind = 1
      while(ind < count){
        val smalls = if(numbers(ind) == numbers(ind-1) ){
          smallCount(ind-1)
        }else{
          ind
        }
        smallCount.update(ind, smalls)
        ind = ind + 1
      }

      var totalSum: BigInt = 0

      (0 to count-1).foreach(ind => {

        val ht = numbers(ind)

        val smalls = smallCount(ind)
        val bigs = count - smalls -1

        //Ray lengths possible are 1 to smalls. Count the number of each and add it to the total sum

        (1 to smalls+1).foreach(rayLen => {

          val chooseFrontSmalls = getChoose(smalls, rayLen-1) * factorials(rayLen - 1)

          //Can be put at position rayLen to count
          //If at position rayLen => only smalls can be put -> choose (rayLen - 1) from smalls  (rayLen is less than smalls)
          val otherPermutations = factorials(count - rayLen)
          val noBigSum = chooseFrontSmalls * otherPermutations

          //Now putting at position rayLen + 1 to count and arrange the other count - smalls - 2
//          val otherPositionsPermutation = factorials(count - rayLen - 1)    //Including one Big
//          val possiblePositions = count - rayLen

          val possiblePositions = factorials(count - rayLen)
          val chooseBig = bigs
          val permutations = chooseFrontSmalls * possiblePositions * bigs

          val sumToAdd = rayLen * (noBigSum + permutations)

          totalSum = totalSum + sumToAdd

        })


      })

      var result =  (BigDecimal(totalSum)) / (BigDecimal(factorials(count)))

      println(result.setScale(2, RoundingMode.HALF_DOWN))


    })


  }

}
