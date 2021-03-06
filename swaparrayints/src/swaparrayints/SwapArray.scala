package swaparrayints

class SwapArray {
	def swap(a: Array[Int]) : Array[Int] = {
	  var c = new Array[Int](a.length)

	  for ( i <- 0 until (a.length, 2)) {
		  c(i) = a(i+1)
		  c(i+1) = a(i)
	  }
	  
	  c
	}	
}

object Test {
	def main(args: Array[String]) {
	  val a = Array(1, 3, 2, 5, 62, 6)   				// 3, 1, 5, 2, 6, 62
	  val swapArray = new SwapArray()
	  val c = swapArray.swap(a)

	  println(a(4) + "\n\n")
	  
	  for (i <- 0 until c.length) {
		  println("C(" + i + ") = " + c(i) + ", \n")
	  }
	}
}
