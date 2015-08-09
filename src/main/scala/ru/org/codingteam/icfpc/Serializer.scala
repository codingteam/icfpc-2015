package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.{FieldDef, OutputDef}
import upickle.default._

import scala.io.Source

object Serializer {

  def serialize(t: Seq[OutputDef]): String = {
    write[Seq[OutputDef]](t)
  }

  def deserialize(x: String): FieldDef = {
    read[FieldDef](x)
  }

  def fromFile(path : String) : FieldDef = {
    val content = Source.fromFile(path).mkString
    deserialize(content)
  }
}
