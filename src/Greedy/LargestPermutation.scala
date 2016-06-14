package Greedy

/**
  * Created by 0000101795 on 2/1/2016.
  */
object
LargestPermutation {

  def main(a: Array[String]) = {

    val meta = readLine().split(" ").map(_.toInt)

    val len = meta(0)
    var swaps = meta(1)

    var numbers = readLine().split(" ").map(_.toInt).toArray

    var positions = Array.ofDim[Int](len)

    (0 to numbers.size-1).foreach(ind => {

      positions( numbers(ind)-1 ) = ind

    })

    var ind = 0

    while(swaps > 0 && ind < len){

      val shouldBe = len - ind

      val numberIs = numbers(ind)

      if(numberIs != shouldBe){
        swaps= swaps - 1
        //swap the positions of these numbers
        val posOfShould = positions(shouldBe-1)
        val posOfIs = ind

        positions(shouldBe-1) = posOfIs
        positions(numberIs-1) = posOfShould

        numbers(ind) = shouldBe
        numbers(posOfShould) = numberIs

      }
      ind = ind + 1
    }

    println(numbers.mkString(" "))

  }

}
