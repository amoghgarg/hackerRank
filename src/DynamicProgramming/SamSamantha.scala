package DynamicProgramming

/**
  * https://www.hackerrank.com/challenges/sam-and-substrings
  */
object SamSamantha {


  def main(args: Array[String]) = {

    val numbers = readLine().map(x => BigInt(x.asDigit))

    val modulo: BigInt = 1000000007

    var prev: BigInt = 0
    var i: BigInt = 1  //numbers till now
    var last: BigInt = 0    //Sum of sequences containing the last number

    numbers.foreach(num => {

      last = (10*last + i*num)

      prev = (prev + last)

      i = i + 1

    })

    prev = prev % modulo
    if(prev < 0) prev = prev + modulo
    println(prev)

  }

}
