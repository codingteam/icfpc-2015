package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.FieldDef

import upickle.default._

object Serializer {

  def serialize(a: Any): String = {
    ???
  }

  def deserialize(x: String): FieldDef = {
    read[FieldDef](x)
  }
}
