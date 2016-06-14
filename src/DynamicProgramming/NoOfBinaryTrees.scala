package DynamicProgramming

/**
 * https://www.hackerrank.com/challenges/number-of-binary-search-tree
 * Input:
 *  T (No of testCases)
 *  N numbers -> For each N, find the number of binary search trees possible for N. (N is the number of nodes with values from 1 to N)
 */
object NoOfBinaryTrees {


  def countTrees(N: Int) = {
    //The return value will contain the number of the trees for each n from 1 to N

    val initCost: List[BigInt] = List(0, 1)

    if(N < 2){
      initCost
    }else{

      (2 to N).foldLeft(initCost)((totalCosts, n) => {

        val cost = (1 to n).foldLeft(0: BigInt)((tempCost, p) => {

          val left = p - 1
          val right = n - p
          val leftCost = if( totalCosts(left) == 0){
            1: BigInt
          }else{
            totalCosts(left)
          }
          val rightCost = if( totalCosts(right) == 0){
            1: BigInt
          }else{
            totalCosts(right)
          }

          tempCost + (leftCost * rightCost)

        })

        val modCost = cost
        totalCosts ++ List( modCost )

      })
    }
  }

  def main(args: Array[String]): Unit = {

    val testCases = readInt()

    val numbers = (1 to testCases).foldLeft(List(): List[Int])((list, i) => {
      val x = readInt
      list ::: List(x)
    })

    val max = numbers.foldLeft(0)((tempMax, p) => math.max(tempMax, p))

    val results = countTrees(max)
    println(results)

    numbers.map(i => {
      val ans = results(i) % 100000007
      println(ans)
    })

  }

}
