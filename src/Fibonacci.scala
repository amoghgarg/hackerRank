/**
 * https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---fibonacci-numbers
 * Input Format
      One integer N.

 * Output Format
   One integer which is the N-th Fibonacci number.
 */
object Fibonacci {

  def main(args: Array[String]): Unit = {
    var count = readInt();

    //(prev than prev, prev) -> ()
    val res  = (1 until count).foldLeft((0, 1))((a, i) => {
      (a._2, a._1 + a._2)
    })

    println(res._1)

  }

}
