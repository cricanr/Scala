

import scala.io.Source
import java.io.PrintWriter

object FilesAndRegex {

  def main(args: Array[String]): Unit = {
  	println("files & regex examples\n")
  	
  	// EXAMPLE 1
  	
  	reverseFileLines("/home/radu/Scala/Scalafortheimpacient/FilesAndRegex/src/mytest1.txt")
  	
  	// END
  }

	def reverseFileLines(fileName: String) : Unit = {
		  val source = Source.fromFile(fileName)
		  val lines = source.getLines.toArray
		  source.close
		  
		  println("Initial file content is: ")
		  for (l <- lines) {
		    println(l)
		  }
		  
		  val reversedLines = lines.reverse
		  val printWriter = new PrintWriter(fileName)
		  for (rl <- reversedLines) {
		    printWriter.println(rl)
		  }
		  printWriter.close
		  
		  println("\nReversed file lines content is: ")
		  for (rl <- reversedLines) {
		    println(rl)
		  }
	}
}