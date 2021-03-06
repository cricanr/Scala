package higherorderfunctions

object higherorderfunctions {
	def main(args: Array[String]) {
	  println("bla")
	  
	  val coll = values(x => x * x, -5, 5)
	  println(coll)
	  
	  val a = Array(20, 12, 6, 15, 2, 9)
	  val item = a.reduceLeft(_ max _)
	  println(item)
	  
	  println("Fact(" + 10 + ") = " + fact(10) )
	  
	  val f = largest(x => 10 * x - x * x, 1 to 10)
	  println("Largest value: " + f)
	  
	  val f2 = largestIndex(x => 10 * x - x * x, 1 to 10)
	  println("Largest value: " + f2)
	  
	  val pairs =(1 to 10) zip (11 to 20)
	  println("Original pairs: " + pairs)
	  val adjusted = adjustToPair(_ * _)((6,7))
	  val sumPair = pairs.map(adjustToPair(_ + _))
	  println(sumPair)
	  unless (10 < 2) {
		  println("Motan")
	  }
	  
	  unless (10 > 2) {
		  println("Motan")
	  }	  
	  
	  val words = Array("here","are","some","words")
	  val length = Array(4,3,4,5)
	  words.corresponds(length)(_.length == _)
	}
	
	def values(fun : (Int) => Int, low: Int, high: Int) = {
	  (low to high).map(x => (x, fun(x)))
	}
	
	def fact(n : Int) : Int = {
	  if (n < 1) 0 else (1 to n).reduceLeft(_ * _)
	}
	
	def largest(fun: (Int) => Int, inputs: Seq[Int]) = {
	  inputs.map(fun).max
	}	
	
	def largestIndex(fun: (Int) => Int, inputs: Seq[Int]) = {
	  inputs.map(fun).max
	  inputs.reduceLeft( (x, y) => if (fun(x) > fun(y)) x else y)
	}
	
	def adjustToPair(f : (Int, Int) => Int) = { 
	  (x: (Int, Int)) => f(x._1, x._2)
	}
	
	def unless(condition: => Boolean)(block: => Unit) {
	  if (!condition) {
	    block
	  }
	}
}