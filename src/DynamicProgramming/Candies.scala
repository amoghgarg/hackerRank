package DynamicProgramming

/**
  * https://www.hackerrank.com/challenges/candies
  *
  * 1 2 3 1 2 3
  * 1 2 3 1 2 3
  *
  */
object Candies {

  def candies(ratings: Array[Int]) = {

    var maxInd = 0
    var maxValue = 0

    (0 to ratings.size-1).foreach(i => {
      if(ratings(i) > maxValue){
        maxValue = ratings(i)
        maxInd = i
      }
    })

    var totalCandy = 0
    var currentCandy = 2

    (0 to maxInd).foreach(i => {

      totalCandy = totalCandy + currentCandy
      if(i + 1 < ratings.size){
        if(ratings(i+1) > ratings(i)) currentCandy = currentCandy + 1
        if(ratings(i+1) == ratings(i)) currentCandy = 1
      }

    })

    currentCandy = 2
    (ratings.size-1 to (maxInd+1, -1) ).foreach(i => {
      if(i < ratings.size) totalCandy = totalCandy + currentCandy

      if(i > 0){
        if(ratings(i-1)>ratings(i)) currentCandy = currentCandy + 1
        if(ratings(i-1)==ratings(i)) currentCandy = 1

      }

    })

    totalCandy

  }

  def main(args: Array[String]) = {

    val count = readInt()

    val ratings = (1 to count).map(i =>readInt).toArray

    //Finding the local mins
    var leftMins: Array[Int] = Array(-1)
    var rightMins: Array[Int] = Array()

    var contMinZone = false
    var contZoneStateInd = -100
    (0 to count - 1).foreach(ind => {

      if (ind == 0) {     //For the first element only.
        if (ratings(0) < ratings(1)) {
          leftMins = leftMins :+ 0
        } else if (ratings(0) == ratings(1)) {
          contMinZone = true
          contZoneStateInd = 0
        } else {
          leftMins = leftMins :+ -1
        }
      } else if (ind == count - 1) {
        val last = ratings(count - 1)
        val prev = ratings(count - 2)
        if (last < prev) {
          rightMins = rightMins :+ (count - 1)
        } else if (last > prev) {
          rightMins = rightMins :+ count
        } else {
          //last = prev, in the zone
          if(contMinZone){
            //The last part is a flat thing.
            rightMins = rightMins :+ contZoneStateInd
          }else{
            rightMins = rightMins :+ count-1
          }
        }
      } else {
        val current = ratings(ind)
        val prev = ratings(ind - 1)
        val next = ratings(ind + 1)
        if (current < prev && current < next) {
          leftMins = leftMins :+ ind
          if (contZoneStateInd != 0) rightMins = rightMins :+ ind
        } else if (current == next && current < prev) {
          contMinZone = true
          contZoneStateInd = ind
        } else if (current < next && contMinZone) {
          contMinZone = false
          if (contZoneStateInd != 0) rightMins = rightMins :+ contZoneStateInd
          leftMins = leftMins :+ ind
        }

      }
    })

    rightMins = rightMins :+ (count)

    println("leftMin")
    leftMins.foreach(x => print(x + " "))
    println("\nrightMin")
    rightMins.foreach(x => print(x + " "))
    println("\n-----------")

    val minCount = leftMins.size

    var result = 0

    (0 to minCount-1).foreach(i => {

      val neededArray = ratings.slice(rightMins(i)+1, leftMins(i))
      neededArray.foreach(i => print(i + " "))
      val temp = candies(neededArray)
      result = result + temp
      println(s"\ncandies: $temp\n-------------")

    })


    if(leftMins(0) > -1) result = result + leftMins(0) + 1
    if(rightMins(minCount-1) < count) result = result + (count-1-rightMins(minCount-1)) + 1

    (1 to minCount-1).foreach(i => {

      result = result + leftMins(i) - rightMins(i-1) + 1

    })

    println(s"Final: $result")

  }



}
