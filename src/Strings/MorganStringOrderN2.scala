package Strings

/**
  * https://www.hackerrank.com/challenges/morgan-and-a-string/forum
  */
object MorganStringOrderN2 {

  def main(args: Array[String]) = {


    val tests = readInt

    (1 to tests).foreach(testNo => {

      var stringA = readLine().toCharArray
      var stringB = readLine().toCharArray
      val lengthA = stringA.length
      val lengthB = stringB.length

      stringA = stringA :+ 'Z'
      stringB = stringB :+ 'Z'

      var pointerA = 0
      var pointerB = 0

      val result = new StringBuilder

      while(pointerA < lengthA && pointerB < lengthB ){

        val charA = stringA(pointerA)
        val charB = stringB(pointerB)

        if(charA < charB) {
          //pick A
          result.append(charA.toString)
          pointerA = pointerA + 1
        }
        else if(charA > charB){
          result.append(charB.toString)
          pointerB = pointerB + 1
        }
        else{
          //CharA = charB
          //Find the pointers till when the stings are equal. The next thing should be the smaller things Choose the entire array uptil there

          var stillEqual = true
          var newPointerA = pointerA + 1
          var newPointerB = pointerB + 1

          while(stillEqual && newPointerA <= lengthA && newPointerB <= lengthB){

            val newCharA = stringA(newPointerA)
            val newCharB = stringB(newPointerB)

            stillEqual = newCharA == newCharB

            newPointerA = newPointerA + 1
            newPointerB = newPointerB + 1

          }

          //Stopped => chars at prev things are either not equal or one of the lenghts has overshot.

          val nextUnequalA = stringA(newPointerA-1)
          val nextUnequalB = stringB(newPointerB-1)

          if(nextUnequalA < nextUnequalB){
            result.append(charA)
            pointerA = pointerA + 1

          }else if(nextUnequalA > nextUnequalB){
            result.append(charB)
            pointerB = pointerB + 1
          }else{
            //unequal characters are equal -> atleast one the pointers is onthe  last.
            //Take the one which is on last Z, append the whole thing to

            if(newPointerA == lengthA+1){
//              val stringToPick = stringA.slice(pointerA, newPointerA)
//              result.append(stringToPick.mkString(""))

              result.append(charA)
              pointerA = pointerA + 1
            }else if(newPointerB == lengthB + 1){
//              val stringToPick = stringB.slice(pointerB, newPointerB)
//              result.append(stringToPick.mkString(""))
              result.append(charB)
              pointerB = pointerB + 1
            }
          }
        }
      }

      if(pointerA < lengthA){
        val stringToPick = stringA.slice(pointerA, lengthA)
        result.append(stringToPick.mkString(""))
      }else if(pointerB < lengthA){
        val stringToPick = stringB.slice(pointerB, lengthB)
        result.append(stringToPick.mkString(""))
      }

      println(result.toString())

    })

  }

}
