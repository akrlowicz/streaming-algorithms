import scala.util.hashing.MurmurHash3

object MainCBF {

  def main(args: Array[String]): Unit = {

    val emp1 = new Employee("John", "Doe")
    val emp2 = new Employee("Jenny", "Fromtheblock")
    val emp3 = new Employee("Joe", "Mama")
    val emp4 = new Employee("Julia", "Joke")
    val emp5 = new Employee("James", "Bond")

    val arg = emp1 #:: emp2 #:: emp3 #:: emp4 #:: LazyList.empty

    val Filter = new CountingBloomFilter(arg, 100, 10)

    println("Does James Bond work here?")
    Filter.check(emp5)
    println("Let's hire James Bond")
    Filter.add(emp5)
    println("Does James Bond work here?")
    Filter.check(emp5)
    println("Does John Doe work here?")
    Filter.check(emp1)
    println("Firing John Doe")
    Filter.delete(emp1)
    println("Does John Doe work here?")
    Filter.check(emp1)
  }

}


class CountingBloomFilter(s: LazyList[Employee], m: Int, k: Int) {
  var buffer: Array[Int] = new Array[Int](m)

  for (i <- 0 until m)
    buffer(i) = 0


  s.foreach {
    x => {
      for (i <- 0 until k)
        buffer((MurmurHash3.stringHash(x.toString, i) % m).abs) += 1

    }
  }

  def add(x: Employee): Unit = {
    for (i <- 0 until k)
      buffer((MurmurHash3.stringHash(x.toString, i) % m).abs) += 1
  }

  def delete(x: Employee): Unit = {
    for (i <- 0 until k)
      buffer((MurmurHash3.stringHash(x.toString, i) % m).abs) -= 1
  }

  def check(x: Employee): Unit = {

    var notFound: Boolean = false

    for (i <- 0 until k)
      if (buffer((MurmurHash3.stringHash(x.toString, i) % m).abs) == 0) notFound = true

    if (notFound) println("The employee does not work here")
    else println("The employee works here")
  }

}

