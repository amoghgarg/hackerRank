package Search

/**
  * Created by 0000101795 on 1/15/2016.
  */
object PlayingWithNumbers {

  def main(arg: Array[String]) = {

    val MAX = 4000;

    //ignore
    readInt()
    val numbers = readLine().split(" ").map(_.toInt)

    readInt()
    var queries = readLine().split(" ").map(_.toInt)

    var positiveFreq = (0 to MAX).map(i => (BigInt(0),BigInt(0),BigInt(0))).toArray      // (freq, cumulativeFreq, cumulativeSum)
    var negativeFreq = (0 to MAX).map(i => (BigInt(0),BigInt(0),BigInt(0))).toArray    // (freq, cumulativeFreq, cumulativeSum)

    var result = new StringBuilder

    numbers.foreach(num => {
      if(num < 0){
        val abs = -1*num
        val prevCount = negativeFreq(abs)
        negativeFreq.update(abs,(prevCount._1+1, prevCount._2, prevCount._3))
      }else{
        val prevCount = positiveFreq(num)
        positiveFreq.update(num,(prevCount._1+1, prevCount._2, prevCount._3))
      }
    })


    positiveFreq.update(0,(positiveFreq(0)._1, positiveFreq(0)._1, 0))
    (1 to MAX).foreach(i => {
      val prev = positiveFreq(i-1)
      val current = positiveFreq(i)

      val cumulativeFreq = prev._2 + current._1
      val cumulativeSum = prev._3 + current._1*i
      positiveFreq.update(i, (current._1, cumulativeFreq, cumulativeSum) )
    })

    negativeFreq.update(1,(negativeFreq(1)._1, negativeFreq(1)._1, negativeFreq(1)._1))
    (2 to MAX).foreach(i => {
      val prev = negativeFreq(i-1)
      val current = negativeFreq(i)

      val cumulativeFreq = prev._2 + current._1
      val cumulativeSum = prev._3 + current._1*i
      negativeFreq.update(i, (current._1, cumulativeFreq, cumulativeSum) )
    })

    var totalSum = negativeFreq(MAX)._3 + positiveFreq(MAX)._3
    var cumQuery = BigInt(0)        //Cumulative Query

    queries.foreach(q => {

      cumQuery = cumQuery + q

      if(cumQuery > 0){

        val abVal = cumQuery

        val positiveIncrease = positiveFreq(MAX)._2 * abVal  //Freq * val

        val sum = if(abVal < MAX){
          val freqOfNegToDecrease = negativeFreq(MAX)._2 - negativeFreq((abVal-1).toInt)._2
          val negativeDecrease = freqOfNegToDecrease * abVal
          val negToIncrease = (negativeFreq((abVal-1).toInt)._2 * abVal)   - 2 * negativeFreq((abVal-1).toInt)._3
          totalSum + positiveIncrease - negativeDecrease + negToIncrease

        }else{
          val negToIncrease = (negativeFreq(MAX)._2 * abVal)   - 2 * negativeFreq(MAX)._3
          totalSum + positiveIncrease + negToIncrease

        }

        result.append(sum.toString() + "\n")

      }else if(cumQuery < 0) {

        val abVal = -1*cumQuery

        val negativeIncrease = negativeFreq(MAX)._2 * abVal

        val sum = if(abVal < MAX) {
          val positiveDecrease = (positiveFreq(MAX)._2 - positiveFreq((abVal - 1).toInt)._2) * abVal

          val positiveToIncrease = positiveFreq((abVal - 1).toInt)._2 * abVal - 2 * positiveFreq((abVal - 1).toInt)._3

          totalSum + negativeIncrease - positiveDecrease + positiveToIncrease

        }else{
          val positiveToIncrease = positiveFreq(MAX)._2 * abVal - 2 * positiveFreq(MAX)._3
          totalSum + negativeIncrease  + positiveToIncrease
        }

        result.append(sum.toString()  + "\n")

      }else{
        result.append(totalSum.toString() + "\n")
      }


    })

    print(result.toString())


  }



}
