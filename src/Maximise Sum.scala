object Solution {

  def getMax(input: List[Int], m: Int): Int = {
    //println("inputArr")
   // println(input)
    //(sumTillNow, maxTillNow)
    val result = input.foldLeft((0,0))((data, value)=>{
      val sumTillNow = (data._1 + value) % m
      val maxTillNow = if(data._2 > sumTillNow){
        data._2
      }else{
        sumTillNow
      }
      //println(s"value: $value, sumTillNow: $sumTillNow, maxTillnow: $maxTillNow ")
      (sumTillNow, maxTillNow)
    })

    return result._2
  }

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    val n = readInt
    (1 to n).map(i => {
      val meta = readLine.split(" ").map(i => i.toInt)
      //println(meta)
      val m = meta(1)
      val arrT = readLine.split(" ").map(a => a.toInt).map(i => i%m).filter(i => i>0)
      //arr.foreach(print)
      val arr = arrT.toList
      println(arr)
      //println(arr.sum)

      //(max, index)
      val res = arrT.foldLeft((0, 0))((data, v) => {
        val i = data._2
        //println("--------Start------")
        val  tempMax = getMax(arr.slice(i, i + m).toList, m)
        //println("MaxFromaboveArrau")
        //println(tempMax)
        //println("-------End----------")
        if(data._1 > tempMax){
          (data._1, data._2 + 1)
        }else{
          (tempMax, data._2 + 1)
        }
      })

      println(res._1)
    })
  }
}
