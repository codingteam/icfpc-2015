package ru.org.codingteam.icfpc

import org.scalatest.{FlatSpec, Matchers}
import ru.org.codingteam.icfpc.definitions.{CellDef, FieldDef, UnitDef}

import scala.io.Source

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

  it should "solve problem_0.json" in {
    val string = Source.fromFile("problem_0.json").getLines().mkString("\n")
    val problem = Serializer.deserialize(string)
    val solution = Strategist.solution(problem, problem.sourceSeeds.head)
    assert(solution.size > 20)
  }

  it should "solve problem_8.json" in {
    val string = Source.fromFile("problem_8.json").getLines().mkString("\n")
    val problem = Serializer.deserialize(string)
    val solution = Strategist.solution(problem, problem.sourceSeeds.head)
    assert(solution.nonEmpty)
  }
}
