package DynamicProgramming

/**
 * https://www.hackerrank.com/challenges/coin-change
 */
object CoinChange {

  case class Path(sum: Int, coinIndex: Int)



  def main(args: Array[String]) = {

    val temp = readLine().split(" ").map(_.toInt)
    val reqSum = temp(0)

    val coins = readLine().split(" ").map(_.toInt).filter(i => i < reqSum + 1).sorted

    val permutes = Array.ofDim[BigInt](reqSum + 1, coins.size)
    val permutesCumulative = Array.ofDim[BigInt](reqSum + 1, coins.size)

    //Cost(n, lc) = (permutes to sum n with max = lc, permutes to sum n with max <=  lc)

    (1 to reqSum).foreach(sum =>{
      (0 to coins.size-1).foreach(coinInd => {

        val currentCoin = coins(coinInd)
        var temp = BigInt(0)
        if(sum - currentCoin > 0){
          temp = permutesCumulative(sum - currentCoin)(coinInd)
        }else if(sum == currentCoin){
          temp = 1
        }
        permutes(sum)(coinInd) = temp
        if(coinInd > 0){
          permutesCumulative(sum)(coinInd) = permutesCumulative(sum)(coinInd-1) + temp
        }else{
          permutesCumulative(sum)(coinInd) = temp
        }


      })
    })

//    permutes.foreach(x => {
//      x.foreach(c => print(c + " "))
//      print("\n")
//    })

    if(coins.size > 0){
      println(permutesCumulative(reqSum)(coins.size-1))
    }else{
      println(0)
    }

  }

}
