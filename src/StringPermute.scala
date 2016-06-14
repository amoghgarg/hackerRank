/**
 * https://www.hackerrank.com/challenges/string-o-permute
 * permute every second letter
 */
object StringPermute {

  def main(args: Array[String]) {
    val count = readInt()

    (1 to count).foreach(_ => {

      val input = readLine()
      val out = new StringBuilder

      (1 to (input.length)/2).foreach(i =>{
        val x = 2*i;
        out.append(input(x-1))
        out.append(input(x-2))
      })

      println(out)
    })

  }

}
