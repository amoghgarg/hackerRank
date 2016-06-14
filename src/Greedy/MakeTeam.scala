package Greedy

import scala.collection.mutable.ListBuffer

/**
  * https://www.hackerrank.com/challenges/team-formation
  */
object MakeTeam {

  def findMinCont(input: List[Int]) = {

    var result = input.size

    var currentLen = 1

    (1 to input.size-1).foreach( i =>{

      if(input(i) == input(i-1) + 1){
        currentLen = currentLen + 1
      }else{
        result = math.min(result, currentLen)
        currentLen = 1
      }

    })

    math.min(result, currentLen)

  }

  def main(args: Array[String]) = {

    val tests = readInt()

    (1 to tests).foreach(testNo => {
      val allNums = readLine().split(" ").map(i => i.toInt)
      val count = allNums(0)
      val nums = allNums.slice(1, allNums(0) + 1).sorted

      val freqArrayTemp = ListBuffer(): ListBuffer[(Int, Int)]

      var currentCount = 1
      var minCount = 1000000
      var maxCount = 0

      


      (1 to allNums(0)-1).foreach(ind => {
        if(nums(ind) == nums(ind-1)){
          currentCount = currentCount + 1
        }else{
          minCount = math.min(minCount, currentCount)
          freqArrayTemp.append((nums(ind-1), currentCount))
          currentCount = 1
        }
        maxCount = math.max(maxCount, currentCount)

      })
      freqArrayTemp.append((nums(count-1), currentCount))

      val freqArray = freqArrayTemp.toList



      var result = count

      (minCount to maxCount).foreach(freq => {

        val array = freqArray.filter( i => i._2 >= freq).map(x => x._1)
        val temp = findMinCont(array)

        //      println(array.mkString(" "))
        //      println(temp)
        //      println("---------")

        result = math.min(result, temp)

      })

      println(result)

    })




  }

}
