package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.UnitDef

object Solver {

  type Position = (Int, Int)

  case class SolverState(field: Field,
                         units: Vector[UnitDef]) {

    def currentUnit = units.head
    def moveCurrentUnit(position: Position): SolverState = {
      val emulator = new Emulator(Field.from(field))
      val unit = emulator.translate(currentUnit)(position._1, position._2)
      emulator.lock(unit)

      this.copy(field = Field.from(emulator), units = units.tail)
    }
  }

  def solution(start: SolverState): Seq[SolverState] = {
    var closedSet = Set[SolverState]()
    var openSet = Set(start)
    var cameFrom = Map[SolverState, SolverState]()

    var gScore = Map(start -> 0).withDefaultValue(Int.MaxValue)
    var fScore = Map(start -> heuristic(start)).withDefaultValue(Int.MaxValue)

    while (openSet.nonEmpty) {
      val current = openSet.toList.sortWith(fScore(_) < fScore(_)).head
      if (goalAchieved(current)) {
        return reconstructPath(cameFrom, current)
      }

      openSet -= current
      closedSet += current

      for (neighbor <- neighbours(current).filter(!closedSet.contains(_))) {
        val tentativeGScore = gScore(current) + distBetween(current, neighbor)
        if (!openSet.contains(neighbor) || tentativeGScore < gScore(neighbor)) {
          cameFrom += neighbor -> current
          gScore += neighbor -> tentativeGScore
          fScore += neighbor -> (gScore(neighbor) + heuristic(neighbor))
          openSet += neighbor
        }
      }
    }

    sys.error("Cannot find a way")
  }

  private def heuristic(state: SolverState): Int = state.units.size
  private def goalAchieved(state: SolverState): Boolean = state.units.isEmpty
  private def reconstructPath(cameFrom: Map[SolverState, SolverState], goal: SolverState): Seq[SolverState] = {
    var totalPath = Vector[SolverState]()
    var current = goal
    while (cameFrom.contains(current)) {
      current = cameFrom(current)
      totalPath :+= current
    }

    totalPath
  }

  private def distBetween(current: SolverState, neighbor: SolverState): Int = 1

  private def neighbours(state: SolverState): Seq[SolverState] = {
    val unit = state.currentUnit
    val positions = getValidPositions(state.field, unit)
    positions.map(state.moveCurrentUnit)
  }

  def getValidPositions(field: Field, unit: UnitDef): Seq[Position] = {
    val emulator = new Emulator(field)
    for { x <- 0.until(field.width)
          y <- 0.until(field.height)
          if emulator.check(emulator.translate(unit)(x, y)) && emulator.anyNeighborNotEmpty(x, y)
    } yield (x, y)
  }
}
