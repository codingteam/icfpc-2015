package ru.org.codingteam.icfpc

import org.scalatest.{FlatSpec, Matchers}
import ru.org.codingteam.icfpc.BottomSolver.SolverState
import ru.org.codingteam.icfpc.definitions.{CellDef, FieldDef, UnitDef}

class BottomSolverSpec extends FlatSpec with Matchers {

  "The BottomSolver" should "properly calculate bottom positions" in {
    val unit = UnitDef(Vector(CellDef(0, 0)), CellDef(0,0))
    val field = FieldDef(
      id = 0,
      units = Vector(unit),
      height = 3,
      width = 3,
      filled = Vector(CellDef(1, 2)),
      sourceLength = 1,
      sourceSeeds = Vector(0))
    val state = SolverState(Field.from(field), field.units)
    val bottoms = BottomSolver.getBottomPositions(state).toSet

    assert(bottoms === Set((0, 2), (0, 1), (1, 1), (2, 2)))
  }
}
