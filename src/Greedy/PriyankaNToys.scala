package Greedy

/**
  * https://www.hackerrank.com/challenges/priyanka-and-toys
  */
object PriyankaNToys {

  def main(a: Array[String])= {

      val N = readInt()

      val wts = readLine().split(" ").map(_.toInt).sorted

      var moneySpent = 1

      var currentWt = wts(0)

    (1 to N-1).foreach(ind => {

      if(wts(ind) > currentWt + 4){
        currentWt = wts(ind)
        moneySpent = moneySpent + 1
      }
    })

    println(moneySpent)


  }

}
