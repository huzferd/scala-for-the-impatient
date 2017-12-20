package chap

import scala.collection.mutable.ArrayBuffer

object Chap03 {
  //1. Write a code snippet that sets a to an array of n random integers between 0 (inclusive) and n  (exclusive).
  def addToArray(n: Int): ArrayBuffer[Int] = {
    val a = ArrayBuffer[Int]()
    for(i <- 0 to n) 
      a.insert(i,i); a
  } //addToArray: (n: Int)scala.collection.mutable.ArrayBuffer[Int]
  addToArray(10) // res0: scala.collection.mutable.ArrayBuffer[Int] = ArrayBuffer(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  //2. Write a loop that swaps adjacent elements of an array of integers. For example, Array(1, 2, 3, 4, 5) becomes Array(2, 1, 4, 3, 5).
  def swapArray(a: Array[Int]): Array[Int] = {
    for(i <- 0 until a.length if(i % 2 == 1)) {
      val temp = a(i);
      a(i) = a(i-1);
      a(i-1) = temp;
    }
    a
  } // swapArray: (a: Array[Int])Array[Int]
  swapArray(Array[Int](1,2,3,4,5,6,7)) // res45: Array[Int] = Array(2, 1, 4, 3, 6, 5, 7)

  //3. Repeat the preceding assignment, but produce a new array with the swapped values. Use for/yield.
  def swapArrayWithYeild(a: Array[Int]) = {
    for(i <- 0 until a.length) yield 
      if(i % 2 == 1) 
        a(i - 1) 
      else {
        if(i == a.length - 1) 
          a(i) 
        else a(i + 1)
      }
  }
  swapArrayWithYeild(Array[Int](1,2,3,4,5,6,7)) //res47: scala.collection.immutable.IndexedSeq[Int] = Vector(2, 1, 4, 3, 6, 5, 7)

  //4. Given an array of integers, produce a new array that contains all positive values of the originalarray, 
  //in their original order, followed by all values that are zero or negative, in their original order.
  def sortPos(a: Array[Int]): Array[Int] = {
    val(p, n) = a.partition(_ > 0)
    p ++ n
  }
  sortPos(Array[Int](2,4,-2,5,0,-4-8,6,1)) // res48: Array[Int] = Array(2, 4, 5, 6, 1, -2, 0, -12)

  //5. How do you compute the average of an Array[Double]?
  def avg(a: Array[Double]): Double = {
    a.sum / a.length
  }
  avg(Array[Double](2.0, 3.5, 4.2, 5.8)) // res49: Double = 3.875

  //6. How do you rearrange the elements of an Array[Int] so that they appear in reverse sorted order? How do you do the same with an ArrayBuffer[Int]?
  def sortWith(a: Array[Int]): Array[Int] = {
    a.sortWith(_ > _)
  }
  sortWith(Array[Int](3,2,65,8,5,4,2,4)) //res50: Array[Int] = Array(65, 8, 5, 4, 4, 3, 2, 2)

  //7. Write a code snippet that produces all values from an array with duplicates removed. (Hint: Look at Scaladoc.)
  def dist(a: Array[Int]): Array[Int] = {
    a.distinct
  }
  dist(Array[Int](1,-3,2,2,3,-3,6,7,4,3,6,1,1,2)) //res51: Array[Int] = Array(1, -3, 2, 3, 6, 7, 4)

  //8. Suppose you are given an array buffer of integers and want to remove all but the first negative number. 
  //Here is a sequential solution that sets a flag when the first negative number is called, then removes all elements beyond.
  //This is a complex and inefficient solution. Rewrite it in Scala by collecting positions of the
  //negative elements, dropping the first element, reversing the sequence, and calling a.remove(i) for each index.
  //9. Improve the solution of the preceding exercise by collecting the positions that should be moved
  //and their target positions. Make those moves and truncate the buffer. Don’t copy any elements
  //before the first unwanted element.

  def dropFirst(a: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    val seq = for (i <- 0 until a.length if a(i) < 0) yield i
    for (i <- seq.reverse.dropRight(1))
      a.remove(i)
    a
  }
  dropFirst(ArrayBuffer[Int](2,3,4,5,6,-1,4,-1,-7,2,9,8)) //res62: scala.collection.mutable.ArrayBuffer[Int] = ArrayBuffer(2, 3, 4, 5, 6, -1, 4, 2, 9, 8)

  //10. Make a collection of all time zones returned by
  //java.util.TimeZone.getAvailableIDs that are in America. Strip off the
  //"America/" prefix and sort the result.

  //11. Import java.awt.datatransfer._ and make an object of type SystemFlavorMap with the call
  //val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
  //Then call the getNativesForFlavor method with parameter DataFlavor.imageFlavor and get the return value as a Scala buffer. 
  //(Why this obscure class? It’s hard to find uses of java.util.List in the standard Java library.)

}