package maps

import scala.beans.BeanProperty

object maps {
	def main(args: Array[String]) {
		val products = Map(("a", 10), ("b", 20), ("c", 4), ("d", 21))
		val prodWithDiscount = for ((k,v) <- products) yield (k, (10 * v / 100))
		
		for ((k,v) <- prodWithDiscount) Console.printf("prodkey:%s : prodval: %d \n", k, v)
	}
}

object maps2 {
	def main(args: Array[String]) {
		val numbers = Array(1,5,2,6,3,74,23)
		
		Console.printf("minmax is %s: ",minmax(numbers))
		
		val tuple = lteqgt(numbers, 6)
	    Console.printf("Tuple is: (%d, %d, %d)\n", tuple._1, tuple._2, tuple._3)
	}
	
	def minmax(values: Array[Int]) : Map[Int, Int] = {
	  val sorted = values.sortWith(_ < _)	  
	  Map((sorted(0), sorted(sorted.length -1)))
	}	
	
	def lteqgt(values: Array[Int], v: Int) : Tuple3[Int, Int, Int] = {
		scala.util.Sorting.quickSort(values)
		val lessThenV = for (value <- values if value < v) yield value
	    val equalToV = for (value <- values if value == v) yield value
	    val greaterThenV = for (value <- values if value > v) yield value
	    
	    (lessThenV.length, equalToV.length, greaterThenV.length)
	}
}

object oop {
  def main(args: Array[String]) {
    val bankAccount = new BankAccount
    bankAccount.deposit(40)
    bankAccount.deposit(12)
    bankAccount.withdraw(7)
    
    Console.printf("Current balance is: %d\n", bankAccount.current)
    
    val time1 = new Time(12, 23)
    val time2 = new Time(12, 22)
    
    Console.printf("time1 is before time2? %b\n", time1.before(time2))
    
    //val time3 = new Time(232, 1)
    
    val student = new Student("Samy", "12")   
    
    val car = new Car("Toyota", "Yaris", 2008, "BH-09-JZR")
    
    Console.printf(car.toString)
    
    Conversions.inchesToCentimeters(23) 
  }
  
  class BankAccount {
    var balance = 0
    
    def current = balance
    
    def deposit(amount: Int): Int = {
      balance += amount
      balance
    }
    
    def withdraw(amount: Int): Int = {
      balance -= amount
      balance
    }
  }
  
  class Time(private val hrs: Int, private val min: Int) {        
	  private val minutes = hrs * 60 + min
	  require (minutes >= 0 && minutes <= (24 * 60 - 1))
    
      def before(other: Time): Boolean = {
        hrs < other.hrs || (hrs == other.hrs && min < other.min)
      }
  }
  
  class Student(@BeanProperty var name: String, @BeanProperty var id: String) { 
  }
  
  class Car(val manufacturer: String, val modelName: String) {
    var licensePlate = ""

    private var _modelYear:Int = -1
    def modelYear = _modelYear 
        
    def this(manufacturer: String, modelName: String, mYear: Int = -1, licensePlate: String = "") = {
	    this(manufacturer, modelName);
	    _modelYear = mYear   
	    this.licensePlate = licensePlate
    }
   

    
    override def toString: String = {
      return "manufacturer: %s ; modelName: %s ; modelYear: %d ; licensePlate: %s\n".format(
          this.manufacturer, this.manufacturer, 12, this.manufacturer)
    }
  }
  
  object Conversions extends UnitConversion{
    def inchesToCentimeters(inches: Int): Double = {
      inches * 2.54
    }
    
    def gallonsToLiters(gallons:Int): Double = {
      gallons * 3.78541178
    }
  }
  
  abstract class UnitConversion {
    def inchesToCentimeters(inches: Int): Double
    def gallonsToLiters(gallons:Int): Double
  } 
}