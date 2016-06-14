package DynamicProgramming

/**
  * https://www.hackerrank.com/challenges/play-game
  * Two players are playing a game.
  * There is a stack of bricks with numbers written on each brick.
  * Each players takes turns to take 1,2 or 3 bricks from the top.
  * If player 1 starts,　print his possible max score.
  */
object Bricks {

  def main(args: Array[String]) = {

    val tests = readInt

    (1 to tests ).foreach(i => {

      val count = readInt()

      val numbersRead = readLine().split(" ").map(x => BigInt(x.toInt)) :+ BigInt(0)
      val numbers = numbersRead.reverse


      if(count < 4){
        println(numbers.sum)
      }else{

        val sums = (0 to count).map(i => BigInt(0)).toArray   //sum(i) = sum of last i numbers
        val max = (0 to count).map(i => BigInt(0)).toArray

        (1 to count).foreach(i => {

          sums(i) = sums(i-1) + numbers(i)

        })


//        numbers.foreach(x => print(s"$x "))
//        print("\n")
//        sums.foreach(x => print(s"$x "))
//        print("\n")
        max.update(1, sums(1))
        max.update(2, sums(2))
        max.update(3, sums(3))
//        max.foreach(x => print(s"$x "))
//        print("\n")

        (4 to count).foreach(i => {
          val takeOne = numbers(i) + sums(i-1) - max(i-1)
          val takeTwo = numbers(i) + numbers(i-1) + sums(i-2) - max(i-2)
          val takeThree = numbers(i) + numbers(i-1) + numbers(i-2) + sums(i-3) - max(i-3)

          val temp = if(takeOne > takeTwo) {
            takeOne
          }else{
            takeTwo
          }

          val temp2 = if(temp > takeThree) temp
                      else takeThree

          max.update(i, temp2)

        })

        println(max(count))

      }

    })

  }

}
