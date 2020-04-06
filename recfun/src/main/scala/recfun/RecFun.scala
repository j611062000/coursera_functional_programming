package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == r || c == 0) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(leftPar: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) leftPar == 0
      else {
        chars.head match {
          case ')' => if (leftPar>= 1)  loop(leftPar - 1, chars.tail) else false
          case '(' => loop(leftPar + 1, chars.tail)
          case _ => loop(leftPar, chars.tail)
        }
      }
    }
    loop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (money >0 && coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)

  }
}
