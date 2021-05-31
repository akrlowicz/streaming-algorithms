import scala.util.hashing.MurmurHash3

object MainCBF {

  def main(args: Array[String]): Unit = {

    val emp1 = new Employee("John", "Doe")
    val emp2 = new Employee("Jenny", "Fromtheblock")
    val emp3 = new Employee("Joe", "Mama")
    val emp4 = new Employee("Julia", "Joke")
    val emp5 = new Employee("James", "Bond")

    val arg = emp1 #:: emp2 #:: emp3 #:: emp4 #:: LazyList.empty

    val cbf = new CountingBloomFilter(arg, 100, 10)

    println("Does James Bond work here?")
    println(cbf.check(emp5))
    println("Let's hire James Bond")
    cbf.add(emp5)
    println("Does James Bond work here?")
    println(cbf.check(emp5))
    println("Does John Doe work here?")
    println(cbf.check(emp1))
    println("Firing John Doe")
    cbf.delete(emp1)
    println("Does John Doe work here?")
    println(cbf.check(emp1))
  }

}


class CountingBloomFilter(s: LazyList[Employee], m: Int, k: Int) {

  require(s.size > 0, "Empty stream")
  require(m > 0, "Number of counters must be grater than zero")
  require(k > 0, "Number of hash function must be grater than zero")

  var buffer: Array[Int] = new Array[Int](m)

  // initialize counters to zero
  for (i <- 0 until m)
    buffer(i) = 0


  s.foreach {
    x => {
      for (i <- 0 until k)
          buffer((MurmurHash3.stringHash(x.toString, i) % m).abs) += 1 //add counter
    }
  }


  // add element to filter
  def add(x: Employee): Unit = {
    for (i <- 0 until k)
      buffer((MurmurHash3.stringHash(x.toString, i) % m).abs) += 1
  }

  //remove element from filter
  def delete(x: Employee): Unit = {
    for (i <- 0 until k)
      buffer((MurmurHash3.stringHash(x.toString, i) % m).abs) -= 1 //decrement counter
  }

  // check for element
  def check(x: Employee): Boolean = {

    var found : Boolean = true

    for (i <- 0 until k)
      if (buffer((MurmurHash3.stringHash(x.toString, i) % m).abs) == 0) found = false

    found
  }

}

