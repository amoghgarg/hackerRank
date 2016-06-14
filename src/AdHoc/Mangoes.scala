package AdHoc

/**
 * https://www.hackerrank.com/challenges/mango
 *
 */
object Mangoes {

  def recurse(tryFrnd: Int, prevFrnd1: Int, prevFrnd2: Int, app: List[(BigInt, BigInt)], M: BigInt): Int = {
    //M = mangoes
    //N = total number of friends

//    println(s"$tryFrnd, $prevFrnd1, $prevFrnd2" )
    if(tryFrnd == prevFrnd1 && math.abs(prevFrnd1 - prevFrnd2) < 2){
      tryFrnd
    }else{
      val apps = app.map(t => t._1 + t._2*(tryFrnd-1))
      val appSortedAsc = apps.sorted.splitAt(tryFrnd)._1

      val requiredMangoes = appSortedAsc.sum
//      println(s"req:$requiredMangoes, mangoes:$M")

      val largePrevFrnd = math.max(prevFrnd1, prevFrnd2)
      val smallPrevFrnd = math.min(prevFrnd1, prevFrnd2)

      if(requiredMangoes > M){
        recurse((tryFrnd+smallPrevFrnd)/2, tryFrnd, smallPrevFrnd, app, M)
      }else{
        recurse((tryFrnd+largePrevFrnd)/2, tryFrnd, largePrevFrnd, app, M)
      }
    }
  }

  def main(args: Array[String]) = {

    val first = readLine().split(" ")
    val N = first(0).toInt   //number of friends
    val M = BigInt(first(1))  //number of mangoes

    val a = readLine().split(" ").map(t => BigInt(t))
    val h = readLine().split(" ").map(t => BigInt(t))

    val unsortedApp = a.zip(h).toList

    val ans = recurse(N, 0, N, unsortedApp, M)

    println(ans)


  }

}
