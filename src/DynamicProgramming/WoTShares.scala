package DynamicProgramming

/**
  * https://www.hackerrank.com/domains/algorithms/dynamic-programming
  */
object WoTShares {

  def optmise() = {

    val days = readInt()
    var profit = 0: BigInt

    val prizes = readLine().split(" ").map(x => BigInt(x))

    var currentSellingPrice = prizes(days-1)
    var sellingPriceInd = days - 1
    var currentSpend = 0: BigInt

    (days - 2 to (0, -1)).foreach( ind => {

      val thisPrize = prizes(ind)

      if(thisPrize < currentSellingPrice){
        //buy this share
        currentSpend = currentSpend + thisPrize
      }else{
        //sell the current holding and set new sellingPrice
        val numberOfShares = sellingPriceInd - ind - 1
        profit = profit + currentSellingPrice*numberOfShares - currentSpend

        currentSpend = 0
        currentSellingPrice = thisPrize
        sellingPriceInd = ind
      }

    })

    val numberOfShares = sellingPriceInd
    profit = profit + numberOfShares*currentSellingPrice - currentSpend


    println(profit)
  }

  def main(args: Array[String]) {

    val casesCount = readInt()

    (1 to casesCount).foreach( i => optmise())

  }


}
