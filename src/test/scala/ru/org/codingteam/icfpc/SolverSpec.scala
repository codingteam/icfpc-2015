package ru.org.codingteam.icfpc

import org.scalatest.{FlatSpec, Matchers}
import ru.org.codingteam.icfpc.Solver.SolverState
import ru.org.codingteam.icfpc.definitions.{CellDef, UnitDef, FieldDef}

class SolverSpec extends FlatSpec with Matchers {
  val unit = UnitDef(Vector(CellDef(0, 0)), CellDef(0,0))
  val emptyField = FieldDef(
    id = 0,
    units = Vector(unit),
    height = 3,
    width = 3,
    filled = Vector(),
    sourceLength = 1,
    sourceSeeds = Vector(0))

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
                    |    "sourceLength": 10
                    |}""".stripMargin
    val fieldDef = Serializer.deserialize(problem)
    val field = Field.from(fieldDef)
    val seed = fieldDef.sourceSeeds.head
    val units = fieldDef.getUnits(seed)

    val initialState = SolverState(field, units.toVector)
    val solution = Solver.solution(initialState)

    assert(solution !== null)
  }

  it should "find valid unit positions only on impacts" in {
    val field = FieldDef(
      id = 0,
      units = Vector(unit),
      height = 3,
      width = 3,
      filled = Vector(),
      sourceLength = 1,
      sourceSeeds = Vector(0))

    val validPositions = Solver.getValidPositions(Field.from(field), field.units.head).toSet
    assert(validPositions === Set((0, 0),         (2, 0),
                                  (0, 1),         (2, 1),
                                  (0, 2), (1, 2), (2, 2)))
  }

  it should "count minimal row gape as a heuristic" in {
    val field = emptyField.copy(filled = Vector(CellDef(0, 0), CellDef(1, 0)))
    val state = SolverState(Field.from(field), field.getUnits(field.sourceSeeds.head).toVector)
    val heuristic = Solver.heuristic(state)
    assert(heuristic === 1)

    val emptyState = SolverState(Field.from(emptyField), field.getUnits(field.sourceSeeds.head).toVector)
    assert(Solver.heuristic(emptyState) === field.width)
  }

  it should "not count initial state as final" in {
    val state = SolverState(Field.from(emptyField), emptyField.getUnits(emptyField.sourceSeeds.head).toVector)
    assert(!Solver.goalAchieved(state))
  }

  it should "count state with empty unit chain as final" in {
    val state = SolverState(Field.from(emptyField), Vector())
    assert(Solver.goalAchieved(state))
  }

  it should "count state where it cannot place a unit as final" in {
    val bigUnit = UnitDef(Vector(CellDef(0, 0), CellDef(1, 0)), CellDef(0, 0))
    val almostFilledField = emptyField.copy(width = 2, height = 2, filled = Vector(CellDef(1, 0),
                                                                              CellDef(0, 1), CellDef(1, 1)))
    val state = SolverState(Field.from(almostFilledField), Vector(bigUnit))
    assert(Solver.goalAchieved(state))
  }
}
