package Greedy

/**
  * https://www.hackerrank.com/challenges/maximise-sum
  * Maximise the sub array sum Modulo M
  */
object MaximseSum {

  def main(args: Array[String]) = {

    val tests = readInt()

    (1 to tests).foreach(i =>{
      val meta = readLine().split(" ")

      val M = BigInt(meta(1))

      val numbers = readLine.split(" ").map(x => BigInt(x))

      var globalMax = BigInt(0)
      var prevMax = BigInt(0)  //max of ending at the prev number

      numbers.foreach(num => {

        val continuingPrev = ( prevMax + num ) % M
        val notContinuing = num % M

        prevMax = if(continuingPrev > notContinuing){
          continuingPrev
        } else{
          notContinuing
        }

        globalMax = if(globalMax > prevMax){
          globalMax
        }else{
          prevMax
        }

      })

      println(globalMax)
    })



  }

}
