package com.heliosmi.scala.problems

import org.scalatest._
import com.heliosmi.scala.problems.ScalaProblems._
class SolutionSpec extends FlatSpec with Matchers {

  it should "Find the last element of the list" in {
    assert(lastElement(List(1, 2, 3)) === 3)
    assert(lastElement(List('a', 'b', 'c')) === 'c')
  }

  it should "Find the last but one element of a list." in {
	  assert(penultimateElement(List(1,2,3)) === 2)
	  assert(penultimateElement(List('a','b','c')) == 'b')
  }
  
  it should "Find the Kth element of a list." in {
    assert(nth(2, List(1, 1, 2, 3, 5, 8)) === 2)
  }
  
  it should "Find the number of elements of a list." in  {
    assert(lengthList(List(1, 1, 2, 3, 5, 8)) === 6)
  }
  
  it should "Reverse a list." in {
    assert(reverse(List(1, 1, 2, 3, 5, 8)) === List(8, 5, 3, 2, 1, 1))
  }
  
  it should "Find out whether a list is a palindrome." in {
    assert(isPalindrome(List(1, 2, 3, 2, 1)) === true)
  }
  
  it should "Flatten a nested list structure." in {
    
  }

}