package oop

import scala.collection.mutable.ArrayBuffer

object oop_examples {
	def main(args: Array[String]) {
		Console.println("test me!")
		
		val bankAccount = new BankAccount(120)
		Console.printf("Opened bank account with initial balance: %1.0f\n", bankAccount.currentBalance)

		val depositAmount = 12
		bankAccount.deposit(depositAmount)
		Console.printf("Balance after deposit: %d is %1.0f\n", depositAmount, bankAccount.currentBalance)
		
		val withdrawAmount = 56		
		bankAccount.withdraw(withdrawAmount)
		Console.printf("Balance after withdraw %d is %1.0f\n", withdrawAmount, bankAccount.currentBalance)
		
		val checkingAccount = new CheckingAccount(32)
		
		Console.printf("Opened checking account with initial balance: %1.0f\n", checkingAccount.currentBalance)

		val depositAmount2 = 12
		checkingAccount.deposit(depositAmount2)
		Console.printf("Balance after deposit: %d is %1.0f\n", depositAmount2, checkingAccount.currentBalance)
		
		val withdrawAmount2 = 56		
		checkingAccount.withdraw(withdrawAmount2)
		Console.printf("Balance after withdraw %d is %1.0f\n", withdrawAmount2, checkingAccount.currentBalance)
		
		val savingsAccount = new SavingsAccount(23) 
		
		savingsAccount.deposit(20)
		savingsAccount.deposit(20)
		savingsAccount.deposit(20)
		savingsAccount.withdraw(23)
		
		val bundle = new Bundle
		val item1 = new SimpleItem(10, "test1")
		bundle.add(item1)
		val item2 = new SimpleItem(20, "test2")
		bundle.add(item2)		
		val price = bundle.price
		val description = bundle.description
		
		Console.printf("Bundle price = %1.0f ; description = %s\n", price, description)
	}
}

class BankAccount(initialBalance: Double) {
  protected var balance = initialBalance
  
  def currentBalance = balance
  
  def deposit(amount : Double) = {
    balance += amount
    balance
  }
  
  def withdraw(amount : Double) = {
    balance -= amount
    balance
  }
}

class CheckingAccount(initialBalance : Double) extends BankAccount(initialBalance: Double) {
  
  override def deposit(amount : Double) = {
    balance += (amount -1)
    balance
  }
  
  override def withdraw(amount : Double) = {
    balance -= (amount - 1)
    balance
  }
}

class SavingsAccount(initialBalance : Double) extends BankAccount(initialBalance : Double) {
  private var transactionCount = 0
  private val monthlyInterest = 4
  
  override def deposit(amount : Double) = {
    transactionCount += 1
    balance += (if (transactionCount < 3) amount else (amount - 1))
    balance
  }
  
  override def withdraw(amount : Double) = {
    transactionCount += 1
    balance -= (if (transactionCount < 3) amount else (amount -1))
    balance
  }
  
  def earnMonthlyInterest : Unit = {
    transactionCount = 0
    
    balance += monthlyInterest  
  }
}

abstract class Item {
  def price : Double
  def description : String
}

class SimpleItem(val price1: Double, val description1: String) extends Item {
  val price = price1
  val description = description1
}

class Bundle extends Item {
  private var items = new ArrayBuffer[Item]
  
  def add(item : Item) : Unit = {
    items += item
  }

  def price: Double = {
    items.map(_.price).sum
  }
  
  
  def description : String = {
    items.foldLeft("")(_+" "+_.description)
  }
}

class Point(x : Int, y : Int) {
  
}

class LabeledPoint(val x : Int, val y: Int, val label: String) extends Point(x,y) {
  
}