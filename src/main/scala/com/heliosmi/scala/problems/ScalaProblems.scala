package com.heliosmi.scala.problems

object ScalaProblems {
  
  def lastElement[A](ls : List[A]) : A = ls match {
    case last :: Nil => last
    case _ :: tail => lastElement(tail)
    case Nil => throw new NoSuchElementException
  }
  
  def penultimateElement[A](ls : List[A]) : A = ls match {
    case e :: _ :: Nil => e
    case _ :: tail => penultimateElement(tail)
    case _ => throw new NoSuchElementException
  }

}