package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.UnitDef

object Solver {

  type Position = (Int, Int)

  case class SolverState(field: Field,
                         units: Seq[UnitDef],
                         lastMovedUnit: Option[UnitDef] = None,
                         targetPosition: Option[Position] = None) {

    def currentUnit = units.headOption

    def moveCurrentUnit(position: Position): SolverState = {
      val emulator = new Emulator(Field.from(field))
      emulator.spawnUnit(currentUnit.get)
      val spawned = emulator.currentUnit

      val unit = emulator.translate(currentUnit.get)(position._1, position._2)
      emulator.lock(unit)

      SolverState(
        field = Field.from(emulator),
        units = units.tail,
        Some(spawned),
        Some(position))
    }

    def canPlaceUnit: Boolean = {
      currentUnit exists { unit =>
        val emulator = new Emulator(field)
        for {x <- 0.until(field.width)
             y <- 0.until(field.height)} {
          if (emulator.check(emulator.translate(unit)(x, y))) {
            return true
          }
        }

        return false
      }
    }

    def anyRowFilled: Boolean = {
      0.until(field.height).exists(field.isRowFilled)
    }
  }

  def solution(start: SolverState): Option[Seq[SolverState]] = {
    var closedSet = Set[SolverState]()
    var openSet = Set(start)
    var cameFrom = Map[SolverState, SolverState]()

    var gScore = Map(start -> 0).withDefaultValue(Int.MaxValue)
    var fScore = Map(start -> heuristic(start)).withDefaultValue(Int.MaxValue)

    while (openSet.nonEmpty) {
      val current = openSet.toList.sortWith(fScore(_) < fScore(_)).head
      if (goalAchieved(current)) {
        return Some(reconstructPath(cameFrom, current))
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

    None
  }

  def heuristic(state: SolverState): Int = state.field.minimalGape

  def goalAchieved(state: SolverState): Boolean = state.anyRowFilled || !state.canPlaceUnit

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

  private def neighbours(state: SolverState): Stream[SolverState] = {
    val unit = state.currentUnit
    val positions = getValidPositions(state.field, unit.get)
    positions.map(state.moveCurrentUnit)
  }

  def getValidPositions(field: Field, unit: UnitDef): Stream[Position] = {
    val emulator = new Emulator(field)
    val coords = (for {x <- 0.until(field.width)
                       y <- 0.until(field.height)} yield (x, y)).toStream
    coords.filter({ case (x, y) => emulator.check(emulator.translate(unit)(x, y)) && emulator.anyNeighborNotEmpty(x, y) })
  }
}
