package DynamicProgramming

/**
 * https://www.hackerrank.com/challenges/password-cracker-fp
 * Check if a given string can be made by concatenation of words from a list of words.
 * Input: a. list of words. b. a string "P"
 * Output: If the string "P" can be made by concatenation of words from the list in any oders then print
 *          the words making P separated by space.
 *          Otherwise, print WRONG PASSWORD
 *
 *  For each letter
 *    For each path
 *      For each remaining word  -> Returns the list of the paths.
 *        If makes sense  -> Returns a list of the paths
 *          Make new paths
 *        Else
 *          return nothing
 */


case class Path(
  wordsTillNow: List[Int] = Nil,  //the words in the path till now
  currentPartWord: String = "",
  remainingPossibilities: List[Int] = Nil   //the words which can be still form the next word in this path.
  )

object StringCompositionChecker {


  def belongs(word: String, partWord: String): Boolean = {

    if(partWord.length > word.length){
      false
    }else if(word.substring(0, partWord.size) == partWord){
      true
    }else{
      false
    }
  }

  def main(args: Array[String]) = {

    val count = readInt();  // number of time the things has to be repeated.

    (1 to count).foreach{  i =>
      val wordCount = readInt()
      val words = readLine().split(" ").toList
      val password = readLine().toCharArray

      val allWordsInd = (0 to wordCount-1).map(i => i).toList
      val firstPath = List(Path(remainingPossibilities = allWordsInd))

      val res = password.foldLeft(firstPath)((paths, letter) => {
        val newPaths = paths.map(path => {

          val wordsTillnow = path.wordsTillNow

          val remainingWords = path.remainingPossibilities
          val currentWord = path.currentPartWord + letter

          val possiblePaths = remainingWords.map(wordInd => {
            if(belongs(words(wordInd), currentWord)){
              //check if the currentword is the words(wordInd) is completely equal to word
              if(currentWord == words(wordInd)){
                //return a new path with all the remaining words now
                Some(Path(wordsTillnow ++ List(wordInd), "", allWordsInd))
              }else{
                //return a path with the reduced paths.
                val remWordsFiltered = remainingWords.filter(ind => belongs(words(ind), currentWord))
                Some(Path(wordsTillnow, currentWord, remWordsFiltered))
              }
            }
            else{
              None
            }
          })
//          println(possiblePaths.flatten)
          possiblePaths.flatMap(p => p)

        })
        val newPathsFlattened = newPaths.flatten
        //println(newPathsFlattened.size)
        //println(newPathsFlattened)

        //// Experimention: Select only one from the paths which end at the same letter.
        val groups = newPathsFlattened.groupBy(p => p.wordsTillNow.lastOption.getOrElse(-1))
       // println(groups)
        val newest = groups.map(grp => grp._2.head).toList
       // println(newest)
        newest

      })

      if(res.nonEmpty){
        val validPaths = res.filter( path => path.currentPartWord.isEmpty )
        val wordIndices = validPaths(0).wordsTillNow
        println(wordIndices.map(i => words(i)).mkString(" "))
      }else{
        println("WRONG PASSWORD")
      }
    }
  }

}
