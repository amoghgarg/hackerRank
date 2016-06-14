object StringCompression {

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    val input = readLine.trim.toCharArray
    //println(input)
    var init: StringBuilder = new StringBuilder("")

    //(result, count, lastChar)
    val a = input.foldLeft((init, 0, input.head))((result, newChar ) => {
      result._2 match{
        case 0 => (result._1 + newChar, 1, newChar)
        case 1 => {
          if(newChar == result._3){
            (result._1, 2, newChar)
          }else{
            (result._1+newChar, 1, newChar)
          }
        }
        case _ => {
          if(newChar == result._3){
            (result._1, result._2 + 1, newChar)
          }
          else{
            ((result._1.append(result._2) + newChar, 1, newChar))
          }
        }
      }
    })
    if(a._2 > 1) println(a._1 + a._2.toString)
    else println(a._1)
//    println(a._1)
  }
}
