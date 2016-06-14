package DynamicProgramming

/**
  * https://www.hackerrank.com/challenges/string-reduction
  */
object StringReduction {

  case class Paths(
                  a: Int,
                  b: Int,
                  c: Int
                  )

  def getNonZeroMin(path: Paths): Int = {

    val list = List(path.a, path.b, path.c).filter(_ > 0)

    if(list.length > 0){
      list.min
    }else{
      0
    }

  }

  def newPath(path: Paths, current: Int): Paths ={

    val newPath = if(current == 0){
      //the current is a
      val newA = getNonZeroMin(path) + 1
      val newB = path.c
      val newC = path.b
      Paths(newA, newB, newC)
    }else if(current == 1){
      //current = b
      val newA = path.c
      val newB = getNonZeroMin(path) + 1
      val newC = path.a
      Paths(newA, newB, newC)
    }
    else{
      //current = c
      val newA = path.b
      val newB = path.a
      val newC = getNonZeroMin(path) + 1
      Paths(newA, newB, newC)
    }

    newPath
  }

  def main(args: Array[String]) = {

    val tests = readInt()

    (1 to tests).foreach(testNo => {

      val nums = readLine().toCharArray.map(x => x - 'a')  // a -> 0, b -> 1, c => 2
      println(nums.mkString(" "))

      val initPath = Paths(0, 0, 0)

      val finalPath = nums.foldLeft(initPath)((path, current) => {
        val ret = newPath(path, current)
        println(s"current: $current")
        println(ret)
        ret
      })

      println(finalPath)
      val min = getNonZeroMin(finalPath)
      println(min)


    })


  }

}
