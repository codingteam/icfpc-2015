package ru.org.codingteam.icfpc

import org.scalatest.{FlatSpec, Matchers}
import ru.org.codingteam.icfpc.definitions.{CellDef, FieldDef, UnitDef}

class StrategistSpec extends FlatSpec with Matchers {

  "The Strategist" should "solve" in {
    val unit = UnitDef(Vector(CellDef(0, 0)), CellDef(0,0))
    val field = FieldDef(
      id = 0,
      units = Vector(unit),
      height = 3,
      width = 3,
      filled = Vector(),
      sourceLength = 1,
      sourceSeeds = Vector(0))
    val solution = Strategist.solution(field, 0)
    assert(solution.nonEmpty)
  }
}
