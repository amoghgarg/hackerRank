package Search

import scala.collection.mutable


/**
  * https://www.hackerrank.com/challenges/pairs
  */
object Pairs {

  def main(args: Array[String])= {

    val meta = readLine().split(" ")

    val K = meta(1).toInt

    val numbers = readLine().split(" ").map(_.toInt)

    val hash = mutable.HashMap.empty[Int, Boolean]

    numbers.foreach(num => {
      hash += (num -> false)
    })

    var pairsCount = 0

    numbers.foreach(num => {

      val needed = num + K

      if(hash.get(needed).nonEmpty) pairsCount = pairsCount + 1


    })

    println(pairsCount)

  }

}
