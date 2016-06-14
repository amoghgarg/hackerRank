package Greedy

/**
  * https://www.hackerrank.com/challenges/missing-numbers
  */
object MissingNumbers {

  def main(args: Array[String]) = {

    readInt()

    //List A, the smaller list

    var min = 1000020  //Max possible number for this problem

    var freq = (0 to 100).map(i => 0).toArray


    val listA = readLine().split(" ").map(i => {
      val temp = i.toInt
      min = math.min(temp, min)
      temp
    })

    listA.foreach(i => {
      val ind = i - min
      freq(ind) = freq(ind) + 1
    })

    readInt

    readLine().split(" ").foreach( i => {

      val temp = i.toInt - min
      freq(temp) = freq(temp) - 1

    })

    (0 to 100).foreach(i => {

      if(freq(i) < 0){
        print(s"${i + min} ")
      }

    })

  }

}
