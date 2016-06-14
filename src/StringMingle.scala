/**
 * https://www.hackerrank.com/challenges/string-mingling
 */
object StringMingle {

  def main(args: Array[String]): Unit = {

    val first = readLine().toCharArray
    val second = readLine().toCharArray

    val zipeed = first.zip(second)

    val x = zipeed.map(item => {
      item._1.toString + item._2.toString
    })

    println(x.mkString(""))

  }

}
