object ListFuncs{
  val l = List(List(1,2,3),List(2,4),List(6,7,8,9))

  def flatten(list: List[Any]): List[Any] = list match{
    case Nil => Nil
    case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
    case head :: tail => head :: flatten(tail)
  }

  flatten(l)

  def encode(chars: List[Char]): List[List[Char]] = chars match{
    case List() => List()
    case x::xs => {
      val (first,last) = chars span (c => c==x)
      List(first):::encode(last)
    }
  }

/*  def counts(charLists: List[List[Char]]) List[(Char,Int)] = {

  }*/

  encode(("asdfasdfgfswertxvbdfasdfbdsgmikrthiolfgsd".toList).sortWith((c1,c2)=> c1<c2)).map(x => (x.head,x.length))
}