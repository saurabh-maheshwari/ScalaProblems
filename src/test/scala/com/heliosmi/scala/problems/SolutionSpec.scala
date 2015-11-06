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

  it should "Run-length encoding of a list." in {
    assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      === List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  it should " Modified run-length encoding." in {
    assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      ===
      List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

  it should "Decode a run-length encoded list." in {
    assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
      === List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  it should "Run-length encoding of a list (direct solution)" in {
    assert(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      === List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }
  it should "Duplicate the elements of a list." in {
    assert(duplicate(List('a, 'b, 'c, 'c, 'd)) === List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  it should "Duplicate the elements of a list a given number of times." in {
    assert(duplicateN(3, List('a, 'b, 'c, 'c, 'd)) === List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  it should " Drop every Nth element from a list." in {
    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  it should "Split a list into two parts." in {
    assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  it should "Extract a slice from a list." in {
    assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('d, 'e, 'f, 'g))
  }

  it should "Rotate a list N places to the left." in {
    assert(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    assert(rotate(-13, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    // assert(rotate(-2, List()) === List())
  }

  it should "Remove the Kth element from a list." in {
    assert(removeAt(1, List('a, 'b, 'c, 'd)) === (List('a, 'c, 'd), 'b))

  }

  it should "Insert an element at a given position into a list." in {
    assert(insertAt('new, 1, List('a, 'b, 'c, 'd)) === List('a, 'new, 'b, 'c, 'd))
  }

  it should "Create a list containing all integers within a given range." in {
    assert(range(4, 9) === List(4, 5, 6, 7, 8, 9))
  }

  it should "Extract a given number of randomly selected elements from a list." in {
    assert(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)).size === 3)
  }

  it should "Draw N different random numbers from the set 1..M." in {
    assert(lotto(6, 49).size === 6)
  }

  it should "Generate a random permutation of the elements of a list." in {
    assert(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)).size === 6)
  }

  it should "Generate the combinations of K distinct objects chosen from the N elements of a list." in {
    assert(combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)).size === 20)
  }

  it should "Group the elements of a set into disjoint subsets." in {
    //Problem statement not clear
  }

  it should "Sorting a list of lists according to length of sublists" in {
    assert(lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) 
       === List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))
  }
   it should "Determine the greatest common divisor of two positive integer numbers." in {
     assert(gcd(72,63) === 9)
   }
  
}