package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.Solver.SolverState
import ru.org.codingteam.icfpc.definitions.{UnitDef, FieldDef}

object Strategist {

  def solution(problem: FieldDef, seed: Int): Seq[Command] = {
    val field = Field.from(problem)
    val units = problem.getUnits(seed).toList
    val emulator = new Emulator(field)
    emulator.load(problem)
    emulator.initSource(seed)

    solve(emulator, units)
  }

  private def solve(emulator: Emulator, units: Seq[UnitDef], commands: Seq[Command] = List()): Seq[Command] = {
    def reduce(steps: Seq[SolverState]): Seq[Command] = {
      val cmds = steps.toStream.map(
        step => LocalSolver.findPath(step.field, step.lastMovedUnit.get, step.targetPosition.get))

      val units_ = units.drop(steps.size)
      val commands_ = cmds.takeWhile(c => c.isDefined).flatMap(c => c.get).toVector
      val commandCount = emulator.emulate(commands_)
      if (commandCount == commands_.size) {
        solve(emulator, units_, commands_)
      } else {
        println("!!!Terminated prematurely; please report")
        commands_.take(commandCount)
      }
    }

    val state = SolverState(Field.from(emulator), units)
    val solution = Solver.solution(state)
    solution match {
      case Some(steps) => reduce(steps)
      case None => commands
    }
  }
}
