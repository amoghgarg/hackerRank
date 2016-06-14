package Sorting

/**
  * Given a string, find the next permutation which is larger then the given string lexicographicaly
  */
object NextPermutation {

  def getClosestTo(string: Array[Char], base: Char): (Char, Array[Char]) ={

    var result = 'z'
    var ind = 0
    var changeInd = 0

    string.foreach(c => {
      if(c > base && c < result ) {
        result = c
        changeInd = ind
      }
      ind = ind + 1
    })
    string.update(changeInd, base)
    (result, string.sorted)
  }

  def main(ar: Array[String]) = {

    (1 to readInt()).foreach(testno => {

      val orig = readLine().toCharArray

      val length = orig.length

      var decreasedInd = -1
      var foundFirst = false
      var ind = length-1

      while(!foundFirst && ind > 0){

        if(orig(ind) > orig(ind-1)){
          foundFirst = true
          decreasedInd = ind - 1
        }
        ind = ind - 1
      }

      if(decreasedInd > -1){

        val goodString = orig.slice(0, decreasedInd)
        val base = orig(decreasedInd)
        val shuffle = orig.slice(decreasedInd+1, length)

        val shuffled = getClosestTo(shuffle, base)

        val result = goodString.mkString("") + (shuffled._1) + shuffled._2.mkString("")
        println(result)

      }else{
        println("no answer")
      }





    })


  }

}
