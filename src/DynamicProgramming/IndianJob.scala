package DynamicProgramming

/**
  * https://www.hackerrank.com/challenges/the-indian-job
  */
object IndianJob {

  def main(args: Array[String]) = {

    val tests = readInt()

    (1 to tests).foreach(i => {

      val meta = readLine().split(" ")
      val G = meta(1).toInt  //the time when the guard returns.

      var totalSum = 0
      val times = readLine().split(" ").map(t => {
        val temp = t.toInt
        totalSum = totalSum + temp
        temp
      })


      val possible = Array.ofDim[Boolean](G+1, times.size)

      (0 to times.size-1).foreach(i => possible(0)(i) = true)

      (1 to G).foreach(reqTime => {

        //Using the first time
        val firstTime = times(0)
        if(reqTime == firstTime){
          (0 to times.size-1).foreach(i => possible(reqTime)(i) = true)
        }else{

          (1 to times.size-1).foreach(newTimeInd => {

            val newTime = times(newTimeInd)

            val isPossibleWithoutAdding = possible(reqTime)(newTimeInd-1)

            val isPossibleByAdding = if(reqTime - newTime >= 0){
              possible(reqTime-newTime)(newTimeInd-1)
            }else{
              false
            }

            possible(reqTime)(newTimeInd) = isPossibleByAdding || isPossibleWithoutAdding

          })

        }

      })

//      possible.foreach(x => {
//        x.foreach(c => print(c + " "))
//        print("\n")
//      })

      val possibilities = possible.map(x => x.reverse.head)

      val maxPossible = G - possibilities.reverse.indexOf(true)
      val otherTrack = totalSum - maxPossible

//      println(s"maxPossible: $maxPossible; otherTrack: $otherTrack; totalSum: $totalSum")

      if(maxPossible < G + 1 && otherTrack < G + 1){
        println("YES")
      }else{
        println("NO")
      }






    })


  }

}
