package Sorting

/**
  * Created by 0000101795 on 1/23/2016.
  */
object AlmostSorted {

  def main(args: Array[String]): Unit = {

    val len = readInt()

    val numbers = readLine().split(" ").map(_.toInt)

    //Checking for swapping thing
    var globalMins = 0
    var globalMaxs = 0
    var leftInd = Array[Int]()
    var rightInd = Array[Int]()

    (0 to len-1).foreach(ind => {

      if(ind == 0){
        if(numbers(1) < numbers(0)){
          globalMaxs = globalMaxs + 1
          leftInd = leftInd :+  0
        }
      }
      else if(ind == len-1){
        if(numbers(len-1) < numbers(len-2)) {
          globalMins = globalMins + 1
          rightInd = rightInd :+ ind
        }
      }else{
        val current = numbers(ind)
        val prev = numbers(ind-1)
        val next = numbers(ind+1)
        if(current > prev && current > next) {
          globalMaxs = globalMaxs + 1
          leftInd = leftInd :+ ind
        }else if(current < prev && current < next){
          globalMins = globalMins + 1
          rightInd = rightInd :+ ind
        }
      }


    })

    if(globalMaxs == 1 && globalMins == 1){
      //Can be only swap, or might be reverse or something else
      if( rightInd(0) < leftInd(0) + 3){
        //Check if swap solves
        if(doesSwapSolve(numbers, leftInd(0), rightInd(0))){
          println(s"yes\nswap ${leftInd(0)+1} ${rightInd(0)+1}")
        }else{
          println("no")
        }
      }else{
        //The lenght is longer than 3, case -> increasing sub, decreasing sub

        var neverDecreases = true
        var isOnlyDecreasig = true

        (leftInd(0)+1 to rightInd(0)).foreach(ind => {
          val current = numbers(ind)
          val prev = numbers(ind - 1)
          if (current > prev) {
            isOnlyDecreasig = false
          } else if (current < prev) {
            neverDecreases = false
          }
        })
        if(neverDecreases){
          //swapping solves
          if(doesSwapSolve(numbers, leftInd(0), rightInd(0))){
            println(s"yes\nswap ${leftInd(0)+1} ${rightInd(0)+1}")
          }else{
            println("no")
          }
        }else if(isOnlyDecreasig){
          println(s"yes\nreverse ${leftInd(0)+1} ${rightInd(0)+1}")
        }
      }
    }else  if(globalMaxs == 2 && globalMins == 2){
      //Check if swap solves
      if(doesSwapSolve(numbers, leftInd(0), rightInd(1))){
        println(s"yes\nswap ${leftInd(0)+1} ${rightInd(1)+1}")
      }else {
        println("no")
      }
    }
    else{
      println("no")
    }

  }

  def doesSwapSolve(numbers: Array[Int], left: Int, right: Int) ={
    //check if swapping left and right will solve
    val temp = numbers(left)
    numbers.update(left, numbers(right))
    numbers.update(right, temp)

    var isSorted = true
    (1 to numbers.length-1).foreach(ind => {
      if(numbers(ind) < numbers(ind-1)) isSorted = false
    })

    isSorted
  }

}
