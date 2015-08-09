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

    it should "make a proper locking move at last" in {
        val field = new Field(10, 8)
        List((0, 7), (1, 7), (2, 7), (3, 7), (4, 7)).foreach({case (x, y) => field(x, y) = CellState.Full })
        val unit = UnitDef(Vector(CellDef(0, 0), CellDef(1, 0), CellDef(2, 0), CellDef(3, 0)),CellDef(1, 0))
        val target = (6, 7)
        val commands = LocalSolver.findPath(field, unit, target).get

        val emulator = new Emulator(field)
        emulator.initSource(List(unit).iterator)

        emulator.emulate(commands.init)
        val result = emulator.emulatorStep(commands.last)

        assert(result.toLock === true)
    }
}
