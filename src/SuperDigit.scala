/**
 * Find the sum of the digits of the input number.
 * input: x y (eg-> 132 2)
 * x -> the input number, y -> the number of times the number is repeated
 * output: sum of digits of x concatenated y time and subsequently summed until you get a single digit number,
 * 14 3 -> 141414 -> sum = 15 -> sum -> 6. So 6 is the expected output.
 * https://www.hackerrank.com/challenges/super-digit
 */
object SuperDigit {

  def getSum(in: String): String = {
    var totalSum = in.toCharArray.foldLeft(0)((sum, x) => {(sum + x.toInt - 48) % 9})
    return totalSum.toString
  }

  def getSuperDig(input: String): String = {
    if(input.length == 1) return input
    else{
      return  getSuperDig(getSum(input))
    }
  }

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    val input = readLine.trim.split(" ")

    val number = input(0)
    val multiplier = input(1).toInt
//    println(number)
//    println(multiplier)
//    println(getSum( number))
    val singleSum = getSum(number)
    val concatenatedSum : BigInt = (singleSum.toInt * multiplier ) % 9
    println(getSuperDig(concatenatedSum.toString))


  }
}
