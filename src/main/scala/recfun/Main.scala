package recfun
import common._

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
  def pascal(c: Int, r: Int): Long = {
    def findFactorial(num: Long, result: Long): Long = {
      if(num < 0 || num > 25) throw new IllegalArgumentException
      else if(num == 0) result
      else findFactorial(num-1,result * num)
      }
    (findFactorial(r, 1))/(findFactorial(c,1)*findFactorial(r-c,1))
  }

  
  /**
   * Exercise 2
   */
   def balance(chars: List[Char]): Boolean = {
    def countParenthesis(c:Int, chars: List[Char]): Boolean = 
      if (chars.isEmpty || c < 0) verifyBalance(c,chars)
      else { chars.head match {
        case '(' => countParenthesis(c+1,chars.tail)
        case ')' => countParenthesis(c-1,chars.tail)
        case _ =>   countParenthesis(c,chars.tail)
      }
     }
    def verifyBalance(c: Int, chars: List[Char]): Boolean = {
      if (c == 0) true
      else false
    }
    countParenthesis(0,chars)
  } 
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def countHelper(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countHelper(money, coins.tail) + countHelper(money - coins.head, coins )
    }
    countHelper(money, coins)
  }
}
