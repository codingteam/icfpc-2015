package ru.org.codingteam.icfpc

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by portnov on 08.08.15.
 */
class EncoderSpec extends FlatSpec with Matchers {
  "The Encoder" should "recognize single spell" in {
    val str = "bap"
    val result = Utils.encode(Utils.decode(str))
    val expected = "ei!"
    assert(result === expected)
  }

  "The Encoder" should "recognize spells" in {
    val str = "qbapeeeeio! io!aaa"
    val result = Utils.encode(Utils.decode(str))
    val expected = "dei!bbbbio! io!aaa"
    assert(result === expected)
  }
}
