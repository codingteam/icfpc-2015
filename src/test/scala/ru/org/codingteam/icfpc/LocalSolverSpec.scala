package ru.org.codingteam.icfpc

import org.scalatest.{Matchers, FlatSpec}
import ru.org.codingteam.icfpc.definitions._

class LocalSolverSpec extends FlatSpec with Matchers {
    "LocalSolver" should "find straight path in problem_0" in {
        val unit = UnitDef(Vector(CellDef(0, 0)), CellDef(0,0))
        val field = new Field(10, 10)
        val pos = (4, 9)
        val solution = LocalSolver.findPath(field, unit, pos)
        // there are a number of equivalent solutions, so I'll just test for
        // the cost
        assert(solution.get.length === 10)
    }
}
