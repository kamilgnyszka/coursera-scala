package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r ) 1
      else pascal(c-1,r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance2(chars: List[Char], parCnt: Int): Boolean = {
        if (parCnt<0) false
        else {
          if (chars.isEmpty) parCnt == 0
          else chars.head match {
            case '(' => balance2(chars.tail, parCnt + 1)
            case ')' => balance2(chars.tail, parCnt - 1)
            case _ => balance2(chars.tail, parCnt)
          }
        }

      }
      balance2(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChange2(money: Int, coins: List[Int]) : Int = {
        if (money < 0 || coins.isEmpty ) 0 else{
          if (money == 0) 1
          else countChange2(money - coins.head, coins) + countChange2(money, coins.tail)
        }
      }

      countChange2(money,coins.sortWith(_>_))
    }

  }
