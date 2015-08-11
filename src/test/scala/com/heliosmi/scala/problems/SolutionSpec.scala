package com.heliosmi.scala.problems

import org.scalatest._
import com.heliosmi.scala.problems.ScalaProblems._
class SolutionSpec extends FlatSpec with Matchers {

  it should "Find the last element of the list" in {
    assert(lastElement(List(1, 2, 3)) === 3)
    assert(lastElement(List('a', 'b', 'c')) === 'c')
  }

  it should "Find the last but one element of a list." in {
    assert(penultimateElement(List(1, 2, 3)) === 2)
    assert(penultimateElement(List('a', 'b', 'c')) == 'b')
  }

  it should "Find the Kth element of a list." in {
    assert(nth(2, List(1, 1, 2, 3, 5, 8)) === 2)
  }

  it should "Find the number of elements of a list." in {
    assert(lengthList(List(1, 1, 2, 3, 5, 8)) === 6)
  }

  it should "Reverse a list." in {
    assert(reverse(List(1, 1, 2, 3, 5, 8)) === List(8, 5, 3, 2, 1, 1))
  }

  it should "Find out whether a list is a palindrome." in {
    assert(isPalindrome(List(1, 2, 3, 2, 1)) === true)
  }

  it should "Flatten a nested list structure." in {
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) === List(1, 1, 2, 3, 5, 8))
  }

  it should " Eliminate consecutive duplicates of list elements." in {
    assert(
      compress(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        === List('a, 'b, 'c, 'a, 'd, 'e))
  }

  it should "Pack consecutive duplicates of list elements into sublists." in {
    //Using recursive and fold
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      === List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
      
      assert(pack2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      === List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }
  
  it should "Run-length encoding of a list." in{
    assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) 
        === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
  
  it should " Modified run-length encoding." in{
    assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) 
        === 
      List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }
  
  it should "Decode a run-length encoded list." in {
    assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) 
        === List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e) )
  }
}