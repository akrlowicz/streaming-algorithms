import scala.collection.mutable

object MainFCA {

  def main(args: Array[String]): Unit = {

    val emp1 = new Employee("John", "Doe")
    val emp2 = new Employee("Jenny", "Fromtheblock")
    val emp3 = new Employee("Joe", "Mama")
    val car1 = new Car("Mazda", "XC500", 1999)
    val car2 = new Car("Volvo", "RBY617",2020)
    val car3 = new Car("BMW", "ZIP892",2018)
    val book1 = new Book( "Lolita", "Michail Bulhakov")
    val book2 = new Book( "Inferno", "Dan Brown")
    val book3 = new Book( "Thinking, Fast and Slow", "Daniel Kahneman")


    // stream of different objects
    val arg = car2 #:: car1 #:: car2 #:: car3 #:: car1 #:: car2 #:: car1 #:: car1 #:: car1 #:: car2 #::
      book2 #:: book1 #:: book2 #:: book3 #:: book3 #:: book2 #:: book1 #:: book2 #:: book1 #:: book1 #:: book1 #:: book2 #::
      emp2 #:: emp1 #:: emp2 #:: emp3 #:: emp1 #:: emp2 #:: emp1 #:: emp1 #:: emp1 #:: emp2 #:: LazyList.empty



    /*
    Mazda 5
    Volvo 4
    BMW 1
    Lolita 5
    Inferno 5
    Thinking 2
    Jenny 4
    John 5
    Joe 1

     */

    println("\n\n -----------MISRA GRIES-----------")
    val carsMG = misraGries(2, arg, classOf[Car])
    val booksMG = misraGries(3, arg, classOf[Book])
    val empsMG = misraGries(4, arg, classOf[Employee])

    carsMG.foreach { case (key, value) => println(key.toString + ". VALUE: " + value) }
    booksMG.foreach { case (key, value) => println(key.toString + ". VALUE: " + value) }
    empsMG.foreach { case (key, value) => println(key.toString + ". VALUE: " + value) }


    println("\n\n -----------SPACE SAVING-----------")
    val carsSS = spaceSaving(2, arg, classOf[Car])
    val booksSS = spaceSaving(3, arg, classOf[Book])
    val empsSS = spaceSaving(4, arg, classOf[Employee])

    carsSS.foreach { case (key, value) => println(key.toString + ". VALUE: " + value) }
    booksSS.foreach { case (key, value) => println(key.toString + ". VALUE: " + value) }
    empsSS.foreach { case (key, value) => println(key.toString + ". VALUE: " + value) }


    println("\n\n -----------LOSSY COUNTING-----------")
    val carsLS = lossyCounting(0.5, arg, classOf[Car])
    val booksLS = lossyCounting(0.3, arg, classOf[Book])
    val empsLS = lossyCounting(0.25, arg, classOf[Employee])

    carsLS.foreach { case (key, value) => println(key.toString + ". VALUE: " + value) }
    booksLS.foreach { case (key, value) => println(key.toString + ". VALUE: " + value) }
    empsLS.foreach { case (key, value) => println(key.toString + ". VALUE: " + value) }



    val testSS = 1  #:: 1  #::  2 #:: 1 #:: 2 #:: 3 #:: 4 #:: 2 #:: 1 #:: 2 #:: 1 #:: 2 #:: LazyList.empty
    val testLC : LazyList[Int] = 1 #:: 2 #:: 4 #:: 3 #:: 4 #:: 3 #:: 4 #:: 5 #:: 4 #:: 6 #:: 7 #:: 3 #:: 3 #:: 6 #:: 1 #:: 1 #:: 3 #:: 2 #:: 4 #:: 7  #:: LazyList.empty

    println("\n\n TEST SPACE SAVING")
    val ss = spaceSaving(3, testSS, classOf[Integer])
    ss.foreach {case (key, value) => println(key.toString + ". VALUE: " + value)}


    println("\n\n TEST LOSSY")
    val lc = lossyCounting(0.2, testLC, classOf[Integer])
    lc.foreach {case (key, value) => println(key.toString + ". VALUE: " + value)}



  }


  def findK(n: Int, freq: Int) : Int = { n/(freq - 1)}

  def misraGries[T](k: Int, s: LazyList[T], c: Class[_]): Map[T, Int] = {

    val obj = s.filter(x => x.getClass == c)
    val map = mutable.Map[T, Int]()

    for (i <- obj) {
      if (map.contains(i)) map(i) += 1 //update key value
      else if (map.size < k - 1) map(i) = 1 //new key value pair, counter set to 1
      else {
        for (key <- map.keys) {
          map(key) -= 1
          if (map(key) == 0) map -= key //delete
        }
      }
    }
  map.toMap
}



  def spaceSaving[T](k: Int, s: LazyList[T], c: Class[_]): Map[T, Int] ={

    val obj = s.filter(x => x.getClass == c)

    val map = mutable.Map[T, Int]()


    for (i <- obj) {
      if (map.contains(i)) map(i) += 1 //update key value
      else if (map.size < k ) map(i) = 1 //new key value pair, counter set to 1
      else {
        val c = map(map.minBy(_._2)._1) //get counter
        map -= map.minBy(_._2)._1 //delete
        map(i) = c + 1 //new key with counter from previous obj with min counter
      }
    }
    map.toMap
  }


  def lossyCounting[T](coeff: Double, s: LazyList[T], c: Class[_]): Map[T, LossyCountItem] ={

    val obj = s.filter(x => x.getClass == c)
    val map = mutable.Map[T, LossyCountItem]()
    var bCurrent = 1
    val w = 1/coeff
    var N = 0

    for (i <- obj) {
      N += 1
      if (map.contains(i)) map(i).incrementFreq() //update key value freq
      else map(i) = new LossyCountItem(1, bCurrent - 1) //new key value pair, freq set to 1
      if (N % w == 0) {
        for ((key, item) <- map) { if (item.frequency + item.error <= bCurrent) map -= key } //delete freq
        bCurrent += 1
      }
    }
    map.toMap
  }



}

class LossyCountItem(var frequency: Double, var error: Double){

  def incrementFreq(): Unit ={
    frequency += 1
  }

  override def toString: String = "Frequency: " + frequency + ", error: " + error

}

class Car(name : String, model: String, year: Int) {

  override def toString: String = "Name: " + name + ", model: " + model + ", year: " + year
}

class Book(title:String, author: String) {

  override def toString: String = "Title: " + title + ", author: " + author
}

class Employee(name: String, surname: String) {

  override def toString: String = "Name: " + name + ", surname: " + surname
}
