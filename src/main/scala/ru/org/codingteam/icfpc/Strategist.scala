package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.BottomSolver.SolverState
import ru.org.codingteam.icfpc.definitions.FieldDef

object Strategist {

  def solution(problem: FieldDef, seed: Int, phrases: Set[String]): Seq[Command] = {
    val field = Field.from(problem)
    val units = problem.getUnits(seed).toList
    val emulator = new Emulator(field, phrases)
    emulator.load(problem)
    emulator.initSourceWithSeed(seed)

    solve(emulator, SolverState(Field.from(emulator), units, phrases), phrases).init // Remove the last turn
  }

  private def solve(emulator: Emulator, state: SolverState, phrases: Set[String], commands: Seq[Command] = List()): Seq[Command] = {
    def reduce(steps: Seq[SolverState]): Seq[Command] = {
      val cmds = steps.filter(_.lastMovedUnit.isDefined).toStream.map(
        step => LocalSolver.findPath(step.field, step.lastMovedUnit.get, step.targetPosition.get, phrases)
      )

      val commands_ = cmds.takeWhile(c => c.isDefined).flatMap(c => c.get).toVector
      val commandCount = emulator.emulate(commands_)
      commandCount match {
        case 0 => commands
        case n if n == commands_.size =>
          val state_ = SolverState(
            Field.from(emulator),
            state.units.drop(steps.size),
            phrases,
            steps.lastOption.flatMap(_.lastMovedUnit),
            steps.lastOption.flatMap(_.targetPosition)
          )

          solve(emulator, state_, phrases, commands ++ commands_)
        case _ =>
          //println("!!!Terminated prematurely; please report")
          commands ++ commands_.take(commandCount)
      }
    }

    val solution = BottomSolver.solution(state, phrases)
    solution match {
      case Some(steps) => reduce(steps)
      case None => commands
    }
  }
}
