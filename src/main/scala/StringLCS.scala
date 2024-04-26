object Main {
  def lcs(s1:String, s2:String, idx1:Int, idx2:Int, memo:Array[Array[Int]]):Int = {

    if(idx1 < s1.length && idx2 < s2.length) {

      if(memo(idx1)(idx2) != -1) return memo(idx1)(idx2)
      if (s1(idx1) == s2(idx2)) {
        memo(idx1)(idx2)  = 1+ lcs(s1, s2, idx1 + 1, idx2 + 1,memo)
        return memo(idx1)(idx2)
      }
      else {
        val oneway = lcs(s1, s2, idx1 + 1, idx2,memo)
        val secondway = lcs(s1, s2, idx1, idx2 + 1,memo)
        memo(idx1)(idx2) = math.max(oneway, secondway)
        memo(idx1)(idx2)
      }
    }
    0
  }
  def main(args: Array[String]): Unit = {
    val s1 = "Helloworld"
    val s2 = "Hell"
    val memo = Array.fill(s1.length+1,s2.length+1)(-1)
    val ans = lcs(s1,s2,0,0,memo)
    println(s"longest common substring length is $ans")
  }
}