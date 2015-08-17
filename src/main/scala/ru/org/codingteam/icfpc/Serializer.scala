package ru.org.codingteam.icfpc

import upickle.default._

object Serializer {

  def deserialize[T : Reader](x: String): T = {
    read[T](x)
  }
}

object MapPrinter {

  def test() : Unit = {
    val d = Serializer.deserialize("")
  }
}