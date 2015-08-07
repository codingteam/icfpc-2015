package ru.org.codingteam.icfpc

import org.scalatest.{Matchers, FlatSpec}
import ru.org.codingteam.icfpc.definitions.{UnitDef, CellDef, FieldDef}

class SerializerSpec extends FlatSpec with Matchers {

  "The Serializer" should "deserialize" in {
    val problem0 = """{
                     |    "height": 10,
                     |    "width": 10,
                     |    "sourceSeeds": [0],
                     |    "units": [{
                     |        "members": [{
                     |            "x": 0,
                     |            "y": 0
                     |        }],
                     |        "pivot": {
                     |            "x": 0,
                     |            "y": 0
                     |        }
                     |    }],
                     |    "id": 0,
                     |    "filled": [],
                     |    "sourceLength": 100
                     |}"""
    val field = Serializer.deserialize[FieldDef](problem0)
    val expected = FieldDef(height = 10, width = 10, sourceSeeds = Array(0), units = Array(UnitDef(
      members = Array(CellDef(0, 0)),
      pivot = CellDef(0, 0)
    )), id = 0, filled = Array(), sourceLength = 100)

    assert(field === expected)
  }
}
