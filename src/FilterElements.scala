import scala.collection.parallel.mutable
import scala.collection.parallel.mutable.ParHashMap

/**
 * https://www.hackerrank.com/challenges/filter-elements
 * filter out the even numbers
 */
object FilterElements {

  def main(args: Array[String]): Unit={

    val count = readInt();

    (1 to count).foreach(i => {
      val k = readLine().split(" ")(1).toInt

      //(key -> (count, pos))
      val map = new ParHashMap[Int, (Int, Int)]

      val numbers = readLine().split(" ").toList.map(a => a.toInt)

      numbers.foreach(i => {
        val (count, position)= map.get(i).getOrElse((0,0))

        if(count > 0){
          map.update(i, (count+1, position))
        }else{
          map.put(i, (1, map.size+1))
        }

      })

      val filterd = map.filter(kv => {
        kv._2._1 > k - 1
      })

      val z = filterd.toList.map(kv => kv._2._2 -> kv._1)

      val res = z.sortBy(it => it._1).map(it => it._2).mkString(" ")

      if(res.size < 1){
        println(-1)
      }else{
        println(res)
      }


    })

  }

}
