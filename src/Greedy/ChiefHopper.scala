package Greedy

/**
  * https://www.hackerrank.com/challenges/chief-hopper
  */
object ChiefHopper {

  def main(a: Array[String]) = {

    val N = readInt

    val hts = readLine().split(" ").map(_.toInt).reverse

    var endEnergy = 0

    hts.foreach(ht => {

      val temp = endEnergy + ht

      endEnergy =  (temp/2) + (temp%2)

    })

    println(endEnergy)




  }

}
