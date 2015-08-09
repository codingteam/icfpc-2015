package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.UnitDef

object BottomSolver {

  type Position = (Int, Int)

  case class SolverState(field: Field,
                         units: Seq[UnitDef],
                         lastMovedUnit: Option[UnitDef] = None,
                         targetPosition: Option[Position] = None) {

    def currentUnit = units.headOption

    def moveCurrentUnit(position: Position): SolverState = {
      val emulator = new Emulator(Field.from(field))

      val pivot = currentUnit.get.pivot
      val unit = emulator.translate(currentUnit.get)(position._1 - pivot.x, position._2 - pivot.y)
      emulator.lock(unit)

      SolverState(
        field = Field.from(emulator),
        units = units.tail,
        currentUnit,
        Some(position))
    }

    def canPlaceUnit: Boolean = {
      currentUnit exists { unit =>
        val emulator = new Emulator(field)
        for {x <- 0.until(field.width)
             y <- 0.until(field.height)} {
          if (emulator.check(emulator.translate(unit)(x - unit.pivot.x, y - unit.pivot.y))) {
            return true
          }
        }

        return false
      }
    }

    def anyRowFilled: Boolean = {
      0.until(field.height).exists(field.isRowFilled)
    }

    def allPositions: Seq[Position] = {
      for {x <- 0.until(field.width)
           y <- 0.until(field.height)} yield (x, y)
    }

    def isEmpty(p: Position): Boolean = field.isValidCell(p._1, p._2) && field(p._1, p._2) == CellState.Empty

    def bottomNotEmpty(coord: Position): Boolean = {
      val coords = List(Direction.SE, Direction.SW) map(Emulator.translateCoord(_)(coord._1, coord._2))
      coords.exists(c => c._2 >= field.height || (field.isValidCell(c._1, c._2) && field(c._1, c._2) == CellState.Full))
    }
  }

  def solution(start: SolverState): Option[Seq[SolverState]] = {
    if (goalAchieved(start)) {
      return None
    }

    var result = Vector[SolverState]()
    var state = start
    while (!goalAchieved(state)) {
      val unit = state.currentUnit.get
      val emulator = new Emulator(state.field)
      val positions = getBottomUnitPositions(state, unit)
      val bottomest = positions.sortWith(_._2 > _._2).headOption
      bottomest match {
        case Some(b) =>
          val newState = state.moveCurrentUnit(b)
          result :+= newState.copy(field = state.field)
          state = newState
        case None =>
          return Some(result)
      }
    }

    Some(result)
  }

  def getBottomPositions(state: SolverState): Stream[Position] = {
    state.allPositions.filter(p => state.bottomNotEmpty(p) && state.isEmpty(p)).toStream
  }

  def getBottomUnitPositions(state: SolverState, unit: UnitDef): Stream[Position] = {
    val lift = unit.members.map(_.y).max - unit.pivot.y
    val bottoms = getBottomPositions(state) map { case (x, y) => (x, y - lift) }

    val emulator = new Emulator(Field.from(state.field))
    bottoms.filter({ case (x, y) =>
      emulator.check(emulator.translate(unit)(x - unit.pivot.x, y - unit.pivot.y)) &&
        emulator.anyNeighborNotEmpty(unit, x, y) &&
        LocalSolver.findPath(state.field, unit, (x, y)).isDefined
    })
  }

  def goalAchieved(state: SolverState): Boolean = state.anyRowFilled || !state.canPlaceUnit
}
