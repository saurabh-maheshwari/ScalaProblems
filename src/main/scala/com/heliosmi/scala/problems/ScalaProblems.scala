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
    def lengthRecursive(acc: Int, currentList: List[A]): Int = currentList match {
      case Nil => acc
      case (_ :: tail) => lengthRecursive(acc + 1, tail)
    }
    lengthRecursive(0, ls)
  }

  def reverse[A](ls: List[A]): List[A] = ls match {
    case Nil => ls
    case e :: tail => reverse(tail) ::: List(e)
  }

  def isPalindrome[A](ls: List[A]): Boolean = {
    ls == reverse(ls)
  }

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

}