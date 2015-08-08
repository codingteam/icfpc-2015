package ru.org.codingteam.icfpc

import org.scalatest.{FlatSpec, Matchers}
import ru.org.codingteam.icfpc.Solver.SolverState

class SolverSpec extends FlatSpec with Matchers {

  "The Solver" should "solve" in {
    val problem = """{
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
    val fieldDef = Serializer.deserialize(problem)
    val field = Field.from(fieldDef)
    val seed = fieldDef.sourceSeeds.head
    val units = fieldDef.getUnits(seed)

    val initialState = SolverState(field, units)
    val solution = Solver.solution(initialState)

    assert(solution !== null)
  }
}
