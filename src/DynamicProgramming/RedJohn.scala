package DynamicProgramming

/**
  * Search for read joh problem i dynamic programming at Hacker Rank
  */
object RedJohn {

  def main(args: Array[String]) = {

    val testCases = readInt()

    var max = 0
    val cases = (1 to testCases).map(i => {
      val num = readInt()
      max = math.max(num, max)
      num
    }).toArray

    val permutationCount = (0 to max).map(i => 1).toArray
    permutationCount.update(4, 2)

    (5 to max).foreach(i => {
      val prev = permutationCount(i-1)
      val prev4 = permutationCount(i-4)
      permutationCount.update(i, prev + prev4)
    })

    val visited = (0 to permutationCount(max)).map(i => false).toArray
    visited.update(0, true)
    visited.update(1, true)

    val countTill = math.sqrt(permutationCount(max)).toInt

    var currentPoint = 2

    while(currentPoint <= countTill){
      (2*currentPoint to (permutationCount(max), currentPoint)).foreach( i => {
        visited.update(i, true)
      })
      currentPoint = currentPoint + 1
      while(visited(currentPoint)){
        currentPoint = currentPoint + 1
      }
    }


    //Count the number of primes
    var currentPrimeCount = 0
    val primeCount = (0 to permutationCount(max)).map(i => {
      if(!visited(i)){
        currentPrimeCount = currentPrimeCount + 1
      }
      currentPrimeCount
    }).toList

//    permutationCount.zipWithIndex.map(x => println(x._2 + "->" + x._1 ))
//    println("-------------------")
//    primeCount.zipWithIndex.map(x => println(x._2 + "->" + x._1 ))

    (0 to testCases-1).foreach(i => {
      val permutations = permutationCount(cases(i))
      println( primeCount( permutations) )
    })
  }

}
