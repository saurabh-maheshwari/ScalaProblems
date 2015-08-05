package com.heliosmi.scala.problems

import java.util.NoSuchElementException

object ScalaProblems {

  def lastElement[A](ls: List[A]): A = ls match {
    case last :: Nil => last
    case _ :: tail => lastElement(tail)
    case Nil => throw new NoSuchElementException
  }

  def penultimateElement[A](ls: List[A]): A = ls match {
    case e :: _ :: Nil => e
    case _ :: tail => penultimateElement(tail)
    case _ => throw new NoSuchElementException
  }

  def nth[A](n: Int, ls: List[A]): A = (n, ls) match {
    case (0, e :: _) => e
    case (n, _ :: tail) => nth(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }

  def lengthList[A](ls: List[A]): Int = {
    ls.foldLeft(0){(c,_) => c+1}
  }

  def reverse[A](ls: List[A]): List[A] = ls.foldLeft(List[A]()){(h,r) => r::h}   

  def isPalindrome[A](ls: List[A]): Boolean = {
    ls == reverse(ls)
  }

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  def compress[A](ls: List[A]): List[A] = ls.foldRight(List[A]()) { (h, r) =>
    if (r.isEmpty || r.head != h) h :: r
    else r
  }

}