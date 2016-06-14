/**
 * Created by 0000101795 on 11/26/2015.
 */
object  ValidBSTPreOrderTraversal {

  def isValidSeq(arr: Array[Int]): Boolean = {

    if(arr.length > 1){
      val head = arr(0)

      val (left, right) = arr.slice(1, arr.length).span(i => i < head)

      if(right.filter(i => i < head).nonEmpty){
        false
      }
      else{
        isValidSeq(left) && isValidSeq(right)
      }
    }
    else{
      true
    }
  }

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    val times = readInt
    (1 to times).map(i => {
      val len = readInt()
      var arr = readLine.split(" ").map(i => i.toInt)

      isValidSeq(arr);

      if(isValidSeq(arr)){
        println("YES")
      }else{
        println("NO")
      }
    })
  }
}
