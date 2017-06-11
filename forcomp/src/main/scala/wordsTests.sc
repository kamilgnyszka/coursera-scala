import forcomp.Anagrams
import forcomp.Anagrams._

object WordsTest{

  val w = "ThisissomeexamplewordtobegroupedBy"
  val s = List("This", "is", "my", "first", "sentence")
  val s2 = "mar"
  val s3 = "marathon"

  //w.toLowerCase groupBy (c=> c) for ((c,cc) <- words )
  val occ2 = (for ((c,cc) <- (s2.toLowerCase groupBy (c=> c))) yield ((c,cc.length)) ).toList.sortWith((x,y) => x._1 < y._1)
  val occ3 = (for ((c,cc) <- (s3.toLowerCase groupBy (c=> c))) yield ((c,cc.length)) ).toList.sortWith((x,y) => x._1 < y._1)

  val comb = combinations(occ2)
    //.toList.sortWith((x,y) => x._1 < y._1)

  comb.size

  //println(comb.mkString("\n"))

  //(s flatMap wordOccurrences groupBy (_._1) map((c) => (c._1,(c._2.unzip)._2.sum))).toList.sortWith((x,y) => x._1 < y._1)

  //((Anagrams.dictionary take 1000 ) map (c=> (c,wordOccurrences(c))) groupBy (_._2) ) map((c) => (c._1,(c._2.unzip)._1))
 /* (for ((c,i) <- occ2  ;
        j <- 0 to i )
    yield (c,j) )

  occ3 forall (x=> occ2.exists(y=> (x._1 == y._1 && x._2 <= y._2) ))

  subtract(occ3,occ2)

  val comblove = combinations(sentenceOccurrences(List("I","am","R") ))

  comblove.size
  List().size
  println(comblove.mkString("\n"))

  sentenceAnagrams(List("Yes", "man")).size*/

  sentenceAnagrams(List("Yes", "man")).size
  println(sentenceAnagrams( List("Yes", "man")) mkString("\n"))

  val allCombs = combinations(sentenceOccurrences(s))
  for (c <- allCombs
       if c.size>0) yield(c)

  def getWords(occurrences: Occurrences): Sentence = {
    dictionaryByOccurrences get (occurrences) match {
      case Some(a) => a
      case None => List()
    }
  }

  for ( cl <- (
  for (comb <- combinations(occ3)
                          ) yield(comb));
    word <- getWords(cl)
  ) yield(word,cl)


}
