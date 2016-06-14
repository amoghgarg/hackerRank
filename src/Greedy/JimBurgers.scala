package Greedy

/**
  * https://www.hackerrank.com/challenges/jim-and-the-orders
  */
object JimBurgers {

  def main(args: Array[String]) = {

    val fans = readInt

    var data = (0 to fans-1).map(ind =>{

      val temp = readLine().split(" ").map(_.toInt)

      (ind, temp(0)+temp(1))

    }).sortBy(_._2)

    data.foreach(x => println(x._1 + 1) )


  }

}
