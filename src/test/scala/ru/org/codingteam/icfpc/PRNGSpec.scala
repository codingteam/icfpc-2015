package ru.org.codingteam.icfpc

import org.scalatest.{Matchers, FlatSpec}

class PRNGSpec extends FlatSpec with Matchers {

  "The PRNG" should "generate sequence from specification" in {
    val prng = new PRNG(17)
    val result = for (x <- List.range(0, 10)) yield prng.next
    val expected = List(0, 24107, 16552, 12125, 9427, 13152, 21440, 3383, 6873,
                        16117)

    assert(result === expected)
  }
}

