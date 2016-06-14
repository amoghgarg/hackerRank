package DynamicProgramming

/**
 * https://www.hackerrank.com/challenges/kundu-and-bubble-wrap
 * Input; Two numbers a, b
 * There a*b bubbles. Kundu picks one at random and pops it. He can pick popped bubbles too.
 * What is expected number of times he would pick a bubble.
 * OutPut; The expected number.
 *
 * Solution:  Expected number pick ups = Expected PickUps for first pop + Expected PickUps for second Pop + ...
 * Expected pick ups for (i+1)th pop (i have popped, a*b - i are remaining) -> Calculate this, would come out to a*b/(a*b -i)
 */
object BubbleWrap {

  def main(args: Array[String]) = {

    val numbers = readLine().split(" ").map(_.toInt)
    val N = numbers(1)*numbers(0)
    val X : Double = N

    val ans: Double = if(N < 500){
      (1 to N).foldLeft(0: Double)((sum, n) => {
        sum + X/n
      })
    }else{
      X*math.log(X)
    }

    println(ans)
  }

}
