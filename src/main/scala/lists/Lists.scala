package lists

import scala.annotation.tailrec

object Lists {
  @tailrec
  def last[A](list: List[A]): A = list match {
    case h :: Nil => h
    case _ :: t   => last(t)
    case _        => throw new IllegalArgumentException("List must not be empty")
  }

  @tailrec
  def penultimate[A](list: List[A]): A = list match {
    case f :: _ :: Nil => f
    case _ :: t        => penultimate(t)
    case _             => throw new IllegalArgumentException("List must contain at least 2 elements")
  }

  @tailrec
  def nth[A](index: Int, list: List[A]): A = (index, list) match {
    case (n, l) if n > 0 && l.isEmpty => throw new IllegalArgumentException("n must be < list length")
    case (0, h :: _)                  => h
    case (n, _ :: t)                  => nth(n - 1, t)
  }

  def length[A](list: List[A]): Int = {
    @tailrec
    def lengthInner(currentLength: Int, list: List[A]): Int = list match {
      case _ :: Nil             => currentLength + 1
      case _ :: t if t.nonEmpty => lengthInner(currentLength + 1, t)
      case _                    => currentLength
    }
    lengthInner(0, list)
  }

  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def reverseInner(currentList: List[A], list: List[A]): List[A] = list match {
      case Nil    => currentList
      case h :: t => reverseInner(h :: currentList, t)
    }

    reverseInner(Nil, list)
  }

  def isPalindrome[A](list: List[A]): Boolean = {
    list == reverse(list)
  }

  // Should be possible to write this with match
  def flatten(list: List[Any]): List[Any] = list.flatMap {
    case t: List[_] => flatten(t)
    case h          => List(h)
  }
}
