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
    ls.foldLeft(0) { (c, _) => c + 1 }
  }

  def reverse[A](ls: List[A]): List[A] = ls.foldLeft(List[A]()) { (h, r) => r :: h }

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

  //Recursive
  def pack[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => Nil
    case x :: _ => val (hs, ts) = xs.span(x==); hs :: pack(ts)
  }

  //Using fold
  def pack2[A](xs: List[A]): List[List[A]] =
    xs.foldRight(List[List[A]]()) {
      case (x, (ys @ (y :: _)) :: rs) if x == y => (x :: ys) :: rs
      case (x, ys) => List(x) :: ys
    }

  def encode[A](ls: List[A]) = {
    pack2(ls).map(x => (x.length, x.head))
  }

  def encodeModified[A](ls: List[A]): List[Any] =
    encode(ls) map { t => if (t._1 == 1) t._2 else t }

  // Just for fun, here's a more typesafe version.
  /*  def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] =
    encode(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t) }
*/

  def decode[A](ls: List[(Int, A)]): List[A] = ls flatMap { e => List.fill(e._1)(e._2) }

  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span { ls.head == _ }
      (packed.length, packed.head) :: encodeDirect(next)
    }
  }

  def duplicate[A](ls: List[A]): List[A] = ls flatMap { e => List(e, e) }

  def duplicateN[A](n: Int, ls: List[A]): List[A] = ls flatMap { List.fill(n)(_) }

  def drop[A](n: Int, ls: List[A]): List[A] = ls.zipWithIndex filter { e => (e._2 + 1) % n != 0 } map { _._1 }

  def split[A](n: Int, ls: List[A]): (List[A], List[A]) = ls.splitAt(n)

  def slice[A](n: Int, m: Int, ls: List[A]): List[A] = ls.zipWithIndex filter { e => (e._2 >= n) && (e._2 < m) } map { _._1 }

  def rotate[A](n: Int, ls: List[A]) = {
    if (ls.isEmpty) Nil

    def moduloBoundary(m: Int): Int =
      if (m < 0) moduloBoundary(m + ls.length)
      else m % ls.length

    val boundary = moduloBoundary(n)
    (ls drop boundary) ::: (ls take boundary)
  }

  def removeAt[A](n: Int, ls: List[A]) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post) => (pre ::: post, e)
    case (pre, Nil) => throw new NoSuchElementException
  }

  def insertAt[A](elem: A, n: Int, ls: List[A]): List[A] = ls.splitAt(n) match {
    case (pre, post) => pre ::: List(elem) ::: post
  }

  //def range(n:Int, m:Int) = n until (m+1) 
  def range(n: Int, m: Int) = List.range(n, m + 1)

}