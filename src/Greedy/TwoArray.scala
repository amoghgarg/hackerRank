package Greedy

/**
  * https://www.hackerrank.com/challenges/two-arrays
  */
object TwoArray {

  def main(ar: Array[String]) = {

    val tests = readInt()

    (1 to tests).foreach(testNo => {
      val meta = readLine().split(" ").map(_.toInt)
      val K = meta(1)

      var A = readLine().split(" ").map(_.toInt).sorted
      var B = readLine().split(" ").map(_.toInt).sorted.reverse

      var allBig = true
      var ind = 0

      while(allBig && ind < A.length){

        if(A(ind)+B(ind) < K) allBig = false
        ind = ind + 1

      }

      if(allBig) println("YES")
      else println("NO")



    })


  }

}
