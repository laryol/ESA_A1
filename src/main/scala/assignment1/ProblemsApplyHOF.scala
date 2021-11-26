package assignment1

import list.implementation.SinglyLinkedIntList
import list.traits.IntList

import scala.collection.View.Empty
import list.implementation

/**
  * Complete the following exercises to practice the usage of higher order functions.
  */
object ProblemsApplyHOF {

  /**
    * multiplyAndFilterEven should multiply all elements of the IntList by
    * the factor x and filter all element that are even
    */
  def multiplyAndFilterEven(l: IntList, x: Int): IntList = {
    l.map(Z=>Z*x).filter(Y=>Y%2==0)
  }
  /**
    * findMin should find the smallest element of a list
    */
  def findMin(l: IntList): Int = {
    l.insertionSort.head
  }
 /* def findMin(l: IntList): Int = {
    // var i = Int.MaxValue
    var i = l.head

    def test(l: IntList): Int = {
      println(i)
      if ((l.head < i) && (l.size > 1)) {
        i = l.head
        test(l.tail)
      }
      if ((l.head >= i) && (l.size > 1)) test(l.tail)
      if ((l.head < i) && (l.size == 1)) i = l.head
      //  if ((l.head <= i) && (l.size == 1)) i
      if (l.tail == Nil) 0
      else i
    }

    test(l)
  } */
  /*
def findMin(l: IntList): Int = {
  var i = 111
  println(i)
  //if (l.tail == Nil) return i
  if ((l.head < i) && (l.size > 1)) {
    i = l.head
    findMin(l.tail)
  }
  if ((l.head >= i) && (l.size > 1)) findMin(l.tail)
  if ((l.head < i) && (l.size == 1)) i = l.head
//  if ((l.head <= i) && (l.size == 1)) i
  if (l.tail == Nil) 0
  else i
} */

  /**
    * sumOddNumbers should sum up all odd numbers of a list
    */
  def sumOddNumbers(l: IntList): Int = {
    //l.filter(X=>X%2==1).reduceLeft(Y=>Y+Y)
    l.filter(X=>X%2==1).reduceLeft((Y,Z)=> Y+Z)

  }
/*  def sumOddNumbers(l: IntList): Int = {
    var i = 0;
    // println(i)
    println(l.head)
    if ((l.head % 2 != 0) && (l.size > 1)) i += l.head + sumOddNumbers(l.tail)
    if ((l.head % 2 == 0) && (l.size > 1)) i += sumOddNumbers(l.tail)
    if ((l.head % 2 != 0) && (l.size == 1)) i += l.head
    // if ((l.head %2 == 0) && (l.size == 1)) i += 0
    if (l.tail == Nil) 0
    else i
    /*  this match {
        case (l.head % 2 != 0) => l.head + sumOddNumbers(l.tail)
        case (l.head %2 == 0) => sumOddNumbers(l.tail)
        case (l.head==Nil) => 0
      }*/
  }
*/
  /**
    * countEvenNumbers should count all even numbers of a list
    */
  def countEvenNumbers(l: IntList): Int = {
    l.filter(X=> X%2==0).size
  }
 /* def countEvenNumbers(l: IntList): Int = {
    var c = 0;
    println(l.head)
    if ((l.head % 2 == 0) && (l.size > 1)) c += 1 + countEvenNumbers(l.tail)
    if ((l.head % 2 != 0) && (l.size > 1)) c += countEvenNumbers(l.tail)
    if ((l.head % 2 == 0) && (l.size == 1)) c += 1
    // if ((l.head %2 != 0) && (l.size == 1)) c += 0
    if (l.tail == Nil) 0
    else c
    //even filtern und dann size-> liste
  }
  */
  /* {
     var counter = 0;
     if (l.head % 2 ==0) (counter += 1 ) else counter
   }*/
  /*{
  l.filter(predicateFunc = if (l.head * x % 2 == 0) true else false)

}*/
  /*
    def multiplyAndFilterEven(l: IntList, x: Int): IntList = ???
   {
      //var li: SinglyLinkedIntList
      val li = Nil

      //  val li: List[Nothing] = List()
      def f1(l: IntList, x: Int): IntList = {
        //var n = l.head * x
        if (((l.head * x) % 2 == 0) && (l.size > 1)) {
          val n: Int = l.head * x
          println(n)
          li.append(n)
          println(li)
          f1(l.tail, x)
        }
        if (((l.head * x) % 2 != 0) && (l.size > 1)) {
          val n: Int = l.head * x
          println(n)
          //  println(li)
          f1(l.tail, x)
        }
        if (((l.head * x) % 2 == 0) && (l.size == 1)) {
          val n: Int = l.head * x
          SinglyLinkedIntList
          li.append(7)
          print("hi")
          println(li)
        }
        if (l.tail == Nil) {
          li
        }
        else {
          li
        }
      }

      f1(l, x)
    } */
  /*{
    this match {
      case ((l.head * x) % 2 == 0) => l.head * x
      case ((l.head * x) % 2 != 0) => l.delete(l.head)
      case (l.head == Nil) => l
      case (l.size == 0) => l
      case (l.isEmpty == true) => l
    }
  }*/
  /* def multiplyAndFilterEven(l: IntList, x: Int): IntList = {
     this match{
       case Cons(head,tail)
       case l.isEmpty == false => this
     }
   }
 */
}
