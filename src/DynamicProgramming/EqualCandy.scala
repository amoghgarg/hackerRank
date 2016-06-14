package DynamicProgramming

/**
  * https://www.hackerrank.com/challenges/equal
  */
object EqualCandy {

  def opsReq(candies: Array[Int], reduceTo: Int): Int ={

    candies.filter(x => x > reduceTo).map(x => {

      var diff = x - reduceTo


      val fives = diff / 5;
      diff = diff - 5 * fives


      val twos = diff / 2;
      diff = diff - 2 * twos

      fives + twos + diff
    } ).sum
  }


  def main(arg: Array[String]) = {

    val tests = readInt()

    (1 to tests).foreach(testNo => {

      val count = readInt

      var candies = readLine().split(" ").map(_.toInt)

      val min = candies.min
      var max = candies.max

      var res = opsReq(candies, min)

      var ind = min - 1
      while(ind >  min - 6 ){
        val temp = opsReq(candies, ind)
        res = math.min(temp, res)
        ind = ind - 1
      }

      println(res)


    })
  }

}
