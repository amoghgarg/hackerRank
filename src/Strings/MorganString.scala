//package Strings
//
///**
//  * https://www.hackerrank.com/challenges/morgan-and-a-string/forum
//  */
//object MorganString {
//
//
//
//
//  def main(args: Array[String]) = {
//
//
//    val tests = readInt
//
//    (1 to tests).foreach(testNo => {
//
//      var stringA = readLine().toCharArray
//      var stringB = readLine().toCharArray
//      val lengthA = stringA.length
//      val lengthB = stringB.length
//
////      var minsA = Array.ofDim[(Char, Int)](lengthA)        //minsA(i) = (min val from i to end, distance of min from i)
////      var minsB = Array.ofDim[(Char, Int)](lengthB)
////
////      var prevMin = ('Z' + 1).toChar
////      var prevMinPos = lengthA
////
////      (lengthA-1 to (0,-1)).map(ind => {
////
////        val thisVal = stringA(ind)
////
////        if(thisVal <= prevMin){
////          prevMin = thisVal
////          prevMinPos = ind
////        }
////        minsA.update(ind, (prevMin, prevMinPos-ind))
////
////      })
////
////      println(stringA)
////      minsA.foreach(x => println(s"${x._1.toChar} ${x._2}"))
////
////      prevMin = ('Z' + 1).toChar
////      prevMinPos = lengthB
////
////      (lengthB-1 to (0,-1)).map(ind => {
////
////        val thisVal = stringB(ind)
////
////        if(thisVal <= prevMin){
////          prevMin = thisVal
////          prevMinPos = ind
////        }
////        minsB.update(ind, (prevMin, prevMinPos-ind))
////
////      })
////
////      println(stringA)
////      minsB.foreach(x => println(s"${x._1.toChar} ${x._2}"))
////
////
////      var result = new StringBuilder
////
////      while( stringA.length > 0 && stringB.length > 0 ){
////
////        val minA = minsA.head._1
////        val minAPos = minsA.head._2
////
////        val minB = minsB.head._1
////        val minBPos = minsB.head._2
////
////
////        //Three cases ; less, greater, equal
////
////        if(minA < minB) {
////          val stringToTake = stringA.slice(0, minAPos + 1)
////          result.append(stringToTake.mkString(""))
////          stringA = stringA.slice(minAPos + 1, lengthA)
////          minsA = minsA.slice(minAPos + 1, lengthA)
////        }else if(minA > minB) {
////          val stringToTake = stringB.slice(0, minBPos + 1)
////          result.append(stringToTake.mkString(""))
////          stringB = stringB.slice(minBPos + 1, lengthB)
////          minsB = minsB.slice(minBPos + 1, lengthB)
////        }else {
////          //mins are equal, take the one which has
////
////        }
////
////
////      }
//
////
//
//
//    })
//
//  }
//
//}
