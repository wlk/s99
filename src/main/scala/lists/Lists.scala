package lists

import scala.annotation.tailrec

object Lists {
  @tailrec
  def last[A](list: List[A]): A = list match {
    case h :: Nil => h
    case _ :: t => last(t)
    case _ => throw new IllegalArgumentException("List must not be empty")
  }

  @tailrec
  def penultimate[A](list: List[A]): A = list match {
    case f :: _ :: Nil => f
    case _ :: t => penultimate(t)
    case _ => throw new IllegalArgumentException("List must contain at least 2 elements")
  }
}
