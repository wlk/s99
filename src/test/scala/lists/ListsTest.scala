package lists

import org.scalatest.{FlatSpec, Matchers}

class ListsTest extends FlatSpec with Matchers {
  import Lists._
  "last" should "work" in {
    last(List(1, 1, 2, 3, 5, 8)) shouldBe 8
    an[IllegalArgumentException] should be thrownBy last(List.empty)
  }

  "penultimate" should "work" in {
    penultimate(List(1, 1, 2, 3, 5, 8)) shouldBe 5
    an[IllegalArgumentException] should be thrownBy penultimate(List(1))
  }

  "nth" should "work" in {
    nth(2, List(1, 1, 2, 3, 5, 8)) shouldBe 2
    an[IllegalArgumentException] should be thrownBy nth(3, List(1))
  }

  "length" should "work" in {
    Lists.length(List(1, 1, 2, 3, 5, 8)) shouldBe 6
    Lists.length(List.empty) shouldBe 0
  }

  "reverse" should "work" in {
    reverse(List(1, 1, 2, 3, 5, 8)) shouldBe List(8, 5, 3, 2, 1, 1)
    reverse(List.empty) shouldBe List.empty
    reverse(List(1)) shouldBe List(1)
  }

  "isPalindrome" should "work" in {
    isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
    isPalindrome(List(1, 2, 3)) shouldBe false
    isPalindrome(List(1)) shouldBe true
    isPalindrome(List.empty) shouldBe true
  }

  "flatten" should "work" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
    flatten(List(1, 2, 3, 4)) shouldBe List(1, 2, 3, 4)
    flatten(List.empty) shouldBe List.empty
  }
}
