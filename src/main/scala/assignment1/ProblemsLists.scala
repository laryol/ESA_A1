package assignment1

//import list.implementation.SinglyLinkedIntList

import list.implementation.SinglyLinkedIntList
import list.traits.IntList
import list.implementation.{Cons, Empty, SinglyLinkedIntList}

//import list.implementation.Cons

object ProblemsLists {

  /**
    *
    * Given a number i that should be duplicated a number of times
    * returns an IntList that contains the duplicated i
    *
    * E.x. duplicateNum(4,3)
    * -> SinglyLinkedList(4, 4, 4, 4)
    *
    * @param i     number to duplicate
    * @param times number of duplicates
    * @return List of duplicated numbers
    */
  def duplicateNum(i: Int, times: Int): IntList = times match {

    //ich muss einfach times mal eine liste zurück geben
    //das passiert mit irgendeinen rekursiven aufruf
    case 0 => Empty
    case _ => Cons(head = i, tail = duplicateNum(i, times - 1))
  }
  /* {
     var x = times
     for (x ==1 ) Cons.append

   }
 */

  /**
    *
    * Given an IntList l that contains even and odd numbers
    * All even numbers of the list should be duplicated an number of times
    * returns an IntList that contains the all duplicated even numbers and the
    * remaining odd numbers in the same order as they occur in the origin list
    *
    * E.x. duplicateEqualNumbers(3,SinglyLinkedList(1,4,3,5,8))
    * -> SinglyLinkedList(1, 4, 4, 4, 3, 5, 8, 8, 8)
    *
    * @param times number of duplicates
    * @param l     IntList that should be processed
    * @return IntList that contains the duplicates and all other nums
    */
  def duplicateEqualNumbers(times: Int, l: IntList): IntList = ???

  /**
    *
    * Given two ordered IntLists l1 and l2 (ascending)
    * The function should merge the two ordered lists in a manner
    * that the result is ordered as well
    *
    * E.x. merge(SinglyLinkedList(3, 5, 7), SinglyLinkedList(1, 3, 5, 8))
    * -> SinglyLinkedList(1, 3, 3, 5, 5, 7, 8)
    *
    * @param l1 IntList in an ascending order
    * @param l2 IntList in an ascending order
    * @return IntList that contains all numbers of both lists in an ascending order
    */
  def merge(l1: IntList, l2: IntList): IntList = ???

  /**
    *
    * Given an unordered IntList
    * The function should split the List in the middle
    * It returns two separated lists
    * If the size of the origin list is odd, the first resulting list
    * should contain one more element than the second
    *
    * E.x. splitList(SinglyLinkedList(3, 5, 7, 1, 3, 5, 8))
    * -> SinglyLinkedList(3, 5, 7, 1) SinglyLinkedList(3, 5, 8)
    *
    * @param l IntList to split
    * @return A tuple of two IntLists that contains the separated lists
    */
  def splitList(l: IntList): (IntList, IntList) = ???
  //size : 2 -> 1.liste rekursiv solange abtrennen bis size erreicht, 2.liste rest

  /**
    *
    * Given an unordered IntList
    * The function should sort the list using the merge sort algorithm
    *
    * E.x. mergeSort(SinglyLinkedList(3, 5, 7, 1, 3, 5, 8))
    * -> SinglyLinkedList(1, 3, 3, 5, 5, 7, 8)
    * Merge Sort Algorithm: https://de.wikipedia.org/wiki/Mergesort
    *
    * @param l IntList to sort
    * @return Sorted IntList
    */
  def mergeSort(l: IntList): IntList = ???

  /*
  * Given the weight in kilograms, that a bag can hold, and a list of items represented by their weights
   * in kilograms.
   * Calculate the maximum weight that fits into the bag.
   *
   * examples:
   * Input:  items      = {4, 8, 5, 4, 2, 1}, Bag Capacity c = 10
   * Output: 10
   *   With e.g. {4, 4, 2} , {8, 1, 1} or {5, 4, 1} the maximum could be reached
   *
   * Input:  items       = {3, 3, 4, 4, 8}, Bag Capacity c = 9
   * Output: 8
   * With {4, 4} the maximum could be reached
   *
   * @param capacity   the capacity of a bag in kg
   * @param items weights of the items in kilograms that are available
   * @return maximum filling capacity
  */
  def packProblem(capacity: Int, items: IntList): Int = ???

  /**
    * Given the weight in kilograms, that a bag can hold, and a list of items represented by their weights
    * in grams, calculate how many bags are needed to hold all of the given items.
    *
    * Use recursions for the calculation
    *
    * examples:
    * Input:  weights       = {4, 8, 1, 4, 2, 1}, Bin Capacity c = 10
    * Output: 2
    * We need minimum 2 bins to accommodate all items
    * First bin contains {4, 4, 2} and second bin {8, 1, 1}
    *
    * Input:  weights       = {9, 8, 2, 2, 5, 4}
    * Bin Capacity c = 10
    * Output: 4
    * We need minimum 4 bins to accommodate all items.
    *
    * @param capacity   the capacity of a bag in kg
    * @param itemWeight weights of the items in grams (all item weights must be lower than the capacity because in
    *                   this case they won't fit into the bag
    * @return minimum number of bags required
    */
  def minBagsCount(capacity: Int, itemWeights: IntList): Int = ???


}
