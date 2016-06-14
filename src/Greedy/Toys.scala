package Greedy

/**
  * https://www.hackerrank.com/challenges/mark-and-toys
  */
object Toys {

  def main(args: Array[String]) {
    val meta = readLine().split(" ").map(_.toInt)
    var moneyRemaining = meta(1)
    val prices = readLine().split(" ").map(_.toInt).sorted

    var toyCount = 0;
    var ind = 0;


   while(moneyRemaining > -1 && ind < prices.length){

     if(prices(ind) <= moneyRemaining){
        toyCount = toyCount + 1
        moneyRemaining = moneyRemaining - prices(ind)

     }
     ind = ind + 1


   }

    println(toyCount)


  }
}
