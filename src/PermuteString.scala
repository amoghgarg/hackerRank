/**
 * input; a string
 * output; all the permutations of the string
 * eg; input -> 'abc'
 * output -> abc, acb, bac, bca, cab, cba.
 */
object PermuteString {

  def permute(prefix: String, remain: String): Unit ={

    val charArry = remain.toCharArray;
    val charArryCopy = remain.toCharArray;

    if(remain.length == 1){
      println(prefix + remain)
    }else{
      charArry.zipWithIndex.map( ind => {
        var splitted = charArryCopy.splitAt(ind._2)
        var spliced = (splitted._1 ++ splitted._2.slice(1,splitted._2.length)).mkString("")
//        println(spliced)
        permute(prefix + ind._1, spliced.mkString(""))
      })
    }


  }

  def main(args: Array[String]): Unit = {
    val input = readLine.trim;

    permute("", input);


  }

}
