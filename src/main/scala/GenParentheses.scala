class GenParentheses {
  def generateParenthesis(n: Int): Array[String] = {
    def Helper(open: Int, close: Int, current: String, result: Array[String]): Array[String] = {
      if (open == 0 && close == 0) {
        (current +: result)
      } else {
        var newResult = result
        if (open > 0) {
          newResult = Helper(open - 1, close, current + "(", newResult)
        }
        if (close > open) {
          newResult = Helper(open, close - 1, current + ")", newResult)
        }
        newResult
      }
    }
    Helper(n, n, "", Array.empty[String])
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val n = 4
    val parentheses = new GenParentheses()
    val combinations = parentheses.generateParenthesis(n)
    combinations.foreach(println)
  }
}
