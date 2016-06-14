package AdHoc

/**
 * https://www.hackerrank.com/challenges/rotate-string
 */
object RotateString {

  def main(args: Array[String]) = {

    val count = readInt()

    (1 to count).foreach(i => {

      val str = readLine

      (1 to str.length).foreach(ind => {

        val (left, right) = str.splitAt(ind)

        print(s"$right$left ")

      })

      println("")
    })

  }

}
