package ru.org.codingteam.icfpc

import org.scalatest.{Matchers, FlatSpec}
import ru.org.codingteam.icfpc.definitions.{OutputDef, UnitDef, CellDef, FieldDef}

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
                     |}""".stripMargin
    val field = Serializer.deserialize(problem0)
    val expected = FieldDef(height = 10, width = 10, sourceSeeds = Vector(0), units = Vector(UnitDef(
      members = Vector(CellDef(0, 0)),
      pivot = CellDef(0, 0)
    )), id = 0, filled = Vector(), sourceLength = 100)

    assert(field === expected)
  }

  "The Serializer" should "serialize" in {
    val output = OutputDef(0, 0, "", "")
    val serialized = Serializer.serialize(output)
    assert(serialized !== null)
  }
}
