/**
 * https://www.hackerrank.com/challenges/remove-duplicates
 */
object removeDuplicates {

  def main (args: Array[String]): Unit = {

    val input = readLine().toCharArray

    var include = (1 to 26 ).map(_ => true)

    val res = input.filter( letter => {
      var ind = letter - 'a'
      if(include(ind)){
       include = include.updated(ind, false)
       true
      }else{
        false
      }
    })


    println(res.mkString(""))

  }

}
