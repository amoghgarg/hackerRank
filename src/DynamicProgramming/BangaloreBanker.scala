package DynamicProgramming

/**
 * https://www.hackerrank.com/challenges/bangalore-bank
 * There is a famous old bank in Bangalore. It has just started the process of filling its database with bank account numbers of its clients. In order to put one account number to the database, an employee has to insert it from a piece of paper to a computer using a standard keyboard (without using number pad found on the right hand side of most keyboards). The weird thing is that every employee assigned to this task can type in using only 2 index fingers (one left hand index finger and one right hand index finger).

Below is the sample representation of number keys present in the keyboard.

1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 0

He can perform any one of the following steps:

He can move any one of his fingers to adjacent keys.
He can press the key just below any of his fingers. But he can press only one key at a time.
Each of the above steps takes 1 second. So moving a finger from key 3 to key 5 takes 2s, moving a finger from key 7 to key 2 takes 5s, and moving a finger from key 0 to key 8 takes 2s (Key 0 is the rightmost key). Similarly, pressing a single key takes 1 second.

Write a program that computes the minimal time needed to add account number of an employee to the database. Before the process, an employee can place his finger wherever he wants. All digits should be inserted in the given order.

Note

It is not necessary that left finger will always lie on the left side of right finger. They can also lie on the same key, and in opposite direction also.
Input
In the first line, there is a number n denoting the length of the bank account number.
In the second line, there are n digits separated by a single space denoting the bank account number.

Output
In one and only line, print the minimum time (in seconds) required to rewrite the bank account number according to the above rules.

Constraints
1 ? n ? 104

 Input #00
2
1 2
Output #00
2
 */
object BangaloreBanker {

  def main(args: Array[String]): Unit = {

    val length = readInt()
    if(length == 1) println(1)
    else if(length == 2) println(2)
    else {

      val digits = readLine().split(" ").map(i => {
        var temp = i.toInt
        if (temp == 0) 9
        else temp - 1
      })

      //DS: List ((leftFinger, RightFinger) -> Cost)
      //For each pair and current -> will two maps
      //Group the maps and choose the min for each map
      val origMap = ((digits(0), -1), 1)    //( (leftFinger = firstNumber, rightFinger = empty) , cost = 1  )
      val origList = List(origMap)

      val paths = (1 until length).foldLeft(origList)((costs, ind) => {

        val current = digits(ind)

        val newPaths = costs.flatMap(pair => {
          //(a, b) -> cost
          val leftFinger = pair._1._1
          val rightFinger = pair._1._2
          val currentCost = pair._2
          //Changing the left
          val leftPathCost = currentCost + math.abs(leftFinger - current) + 1

          val rightPathCost = if(rightFinger == -1){
            currentCost + 1
          }else{
            currentCost + math.abs(rightFinger - current) + 1
          }

          val leftPath = ((current, rightFinger), leftPathCost)
          val rightPath = ((leftFinger, current), rightPathCost)
          List(leftPath, rightPath)
        })

        val groupedPath = newPaths.groupBy(path => path._1)
        val filteredPaths = groupedPath.map(group => {

          val pathList = group._2
          val tempMin = pathList(0)._2
          val minCost = pathList.foldLeft(tempMin)((min, pathCost) => math.min(min, pathCost._2))

          (group._1, minCost)
        })

        filteredPaths.toList

      })

      val tempMin = paths(0)._2
      println(paths.foldLeft(tempMin)((min, x) => math.min(min, x._2)))

    }
  }
}
