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
}
