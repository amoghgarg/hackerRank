/**
 * https://www.hackerrank.com/challenges/pascals-triangle
 * For a given integer K, print the first K rows of Pascal's Triangle.
 * Print each row with values separated by spaces.
 * The value at nthrow and rth column of the triangle is equal to n! / (r! * (n-r)!) where
 * indexing start from 0. These values are the binomial coefficients.
 */
object PascalTriangle {

  def main(args: Array[String]): Unit = {
    var count = readInt();

    println(1);
    val arr = List(1,1)

    (2 until count+1).foldLeft(arr)((input, x) => {

      println(input.mkString(" "))

      val newA = input.scanLeft((0,0))( (res, now) => (now + res._2, now) )

      val x = newA.map(i => i._1).drop(1)

      x:::List(1)

    })


  }

}
