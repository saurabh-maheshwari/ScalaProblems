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

}