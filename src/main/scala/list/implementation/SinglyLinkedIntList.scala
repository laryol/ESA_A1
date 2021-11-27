package list.implementation

import list.traits.IntList

import scala.::

/**
  * A companion object for the singly linked list.
  * This enables creating lists list this: val list = SinglyLinkedIntList(1,2,3)
  * which results in Cons(1,Cons(2,Cons(3,Empty))))
  */
object SinglyLinkedIntList {


  /** The apply function is a special function in scala.
    * It can be invoked with SinglyLinkedIntList.apply(args) or simply SinglyLinkedIntList(args).
    * This particular implementation of it is also a variadic function, i.e.
    * a function which accepts one or more arguments of the same type (integers) as parameters.
    */
  //inside this method xs is of type Seq[int]
  def apply(xs: Int*): SinglyLinkedIntList = xs match {
    case Seq() => Empty
    //: _* results in the sequence being passed as multiple parameters - (1,2,3) instead of Seq[Int]{1,2,3}
    case _ => Cons(xs.head, SinglyLinkedIntList(xs.tail: _*))
  }
}

abstract class SinglyLinkedIntList extends IntList {

  override def prefix(other: IntList): IntList = other match {

    case Empty => this
    case Cons(h, t) => Cons(h, prefix(t))
  }

  override def size: Int = this match {

    case Empty => 0
    case _ => 1 + tail.size
  }

  /** ------------------------------------------
    *
    * Assignment 1
    *
    * ------------------------------------------ */


  override def map(mapFunc: Int => Int): IntList = {
    this match {
      case Empty => Empty
      case Cons(head, tail) => if (tail.isEmpty) Cons(mapFunc(head), Empty) else Cons(mapFunc(head), tail.map(mapFunc))
    }
  }

  override def filter(filterFunc: Int => Boolean): IntList = {
    this match {
      case Empty => Empty
      case Cons(head, tail) => if (filterFunc(head) == true) Cons(head, tail.filter(filterFunc)) else tail.filter(filterFunc)
    }
  }

  override def forAll(predicateFunc: Int => Boolean): Boolean = {
    this match {
      case Empty => false
      case Cons(head, Empty) => if (predicateFunc(head) == false) false else true
      //case Empty => isEmpty
      case Cons(head, tail) => if (predicateFunc(head) == false) false else tail.forAll(predicateFunc)
    }
  }

  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Empty => throw new ArithmeticException("Keine Listenelemente zum Berechnen")
      case _ => if (tail.isEmpty) head else Cons(reduceFunc(initial, head), tail).reduceLeft(reduceFunc)
    }
  }

  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Empty => throw new ArithmeticException("Keine Listenelemente zum Berechnen")
      case Cons(head, tail) => if (tail.isEmpty) head else {
        println(Cons(reduceFunc(head, tail.head), tail))
        Cons(reduceFunc(head, tail.head), tail.delete(tail.head)).reduceLeft(reduceFunc)
      }
    }
  }

  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Empty => -1
      case Cons(head, tail) => if (tail.isEmpty) reduceFunc(head, initial) else reduceFunc(head, tail.foldRight(initial)(reduceFunc))
    }
  }

  override def reduceRight(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Empty => -1
      case Cons(head, tail) => if (tail.isEmpty) head else reduceFunc(head, tail.reduceRight(reduceFunc))
    }
  }


  override def insertionSort: IntList = this match {
    case Empty => Empty
    //eins wird richtig eingesetzt aber nicht von der alten stelle entfernt und nur ein durchlauf
    case Cons(head, tail) => tail.insertionSort.insertSorted(head)
  }

  override def insertSorted(elem: Int): IntList = {
    this match {
      //wenn liste leer gebe liste mit uebergebenen elem zurueck
      case Empty => Cons(elem, Empty)
      //wenn kopf groesser als elem dann fuege elem vor head ein
      case Cons(head, tail) => if (head >= elem) {
        Cons(head, tail).prepend(elem)
      }
      //sonst fuegt man den int rekursiv in das tail der liste
      else {
        Cons(head, tail.insertSorted(elem))
      }
    }
  }

  override def foldLeft[A](initial: A)(reduceFunc: (A, Int) => A): A = this match {
    case Empty => initial
    //Cons kann nur (int, list) sein
    //i need a substitution of the cons since this function only takes integer as an input
    //cons willl int aber reduce an der stelle will A
    case Cons(head, tail) => tail.foldLeft(reduceFunc(initial, head))(reduceFunc)
  }
  // override def foldLeft[A](initial: A)(reduceFunc: (A, Int) => A): A = ???
  /*
    override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Empty => throw new ArithmeticException("Keine Listenelemente zum Berechnen")
      case _ => if (tail.isEmpty) head else Cons(reduceFunc(initial, head), tail).reduceLeft(reduceFunc)
    }
  }

   */


}