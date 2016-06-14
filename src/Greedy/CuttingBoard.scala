//package Greedy
//
///**
//  * https://www.hackerrank.com/challenges/board-cutting
//  */
//object CuttingBoard {
//
//
//  def main(args: Array[String]) = {
//
//    val tests = readInt
//    (1 to tests).foreach(testNo => {
//
//      val meta = readLine().split(" ").map(_.toInt)
//
//      val (cols, rows) = (meta(0), meta(1))
//
//      val colCosts = readLine().split(" ").map(_.toInt).sorted.reverse
//      val rowCosts = readLine().split(" ").map(_.toInt).sorted.reverse
//
//      var verticalCuts = 0
//      var horizontalCuts = 0
//      var colPieces = 1
//      var rowPieces = 1       //A vertical cut increases vertical pieces, which increases horizontal multiplier
//
//      var totalCost = 0
//
//      var colCutCost = colCosts(verticalCuts) * rowPieces
//      var horCutCost = rowCosts(horizontalCuts) * colPieces
//
//      if(colCutCost > )
//
//
//    })
//
//  }
//
//}
