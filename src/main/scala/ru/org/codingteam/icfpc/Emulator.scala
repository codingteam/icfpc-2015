package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.{FieldDef, CellDef, UnitDef}

object Field {

  def from(fieldDef: FieldDef): Field = {
    val field = new Field(fieldDef.width, fieldDef.height)
    field.load(fieldDef)
    field
  }

  def from(emulator: Emulator): Field = from(emulator.field)
  def from(field: Field): Field = {
    val result = new Field(field.width, field.height)
    result.field = field.field.clone()
    for (x <- 0.until(field.width)) {
      result.field(x) = result.field(x).clone()
    }

    result
  }
}

/**
 * Created by portnov on 07.08.15.
 */
class Field(val width : Int, val height : Int) {
  var field = Array.fill[CellState.CellState](width, height){CellState.Empty}

  def refill() : Unit = {
    field = Array.fill[CellState.CellState](width, height){CellState.Empty}
  }

  def load(fd: FieldDef): Unit = {
    fd.filled.foreach({
      case CellDef(x,y) => this(x,y) = CellState.Full
    })
  }

  def isValidCell(x : Int, y : Int) : Boolean = {
    (x >= 0) && (x < width) && (y >= 0) && (y < height)
  }

  def apply(x : Int, y : Int) : CellState.CellState = {
    return field(x)(y)
  }

  def update(x : Int, y : Int, st : CellState.CellState): Unit = {
    field(x)(y) = st
  }

  def isRowFilled(y : Int) : Boolean = {
    (for (x <- List.range(0, width))
      yield field(x)(y)).forall(_ == CellState.Full)
  }

  def clearRow(y0 : Int) : Unit = {
    // Mark y0 row as empty
    for (x <- List.range(0, width)) {
      field(x)(y0) = CellState.Empty
    }
    // Copy values of cells with y < y0 from cell at top
    for (y <- List.range(1, y0)) {
      for (x <- List.range(0, width)) {
        field(x)(y) = field(x)(y-1)
      }
    }
    // Mark 0th row as empty
    for (x <- List.range(0, width)) {
      field(x)(0) = CellState.Empty
    }
  }

  // Return number of cleared rows
  def clearRows() : Int = {
    var count = 0
    for (y <- List.range(0, height)) {
      if (isRowFilled(y)) {
        count += 1
        clearRow(y)
      }
    }
    return count
  }

  def printField() : Unit = {
    for (y <- List.range(0, height)) {
      if (y % 2 == 1) {
        print("  ")
      }
      for (x <- List.range(0, width)) {
        if (field(x)(y) == CellState.Full) {
          print("|XXX")
        } else {
          print("|   ")
        }
      }
      print("|\n")
    }
  }
}

class Emulator (val field : Field) {

  case class StepResult(gameOver : Boolean, toLock : Boolean)

  // Current unit should be in global field coordinates
  var currentUnit : UnitDef = _

  var fieldDef : FieldDef = _

  var score = 0

  private def load(fd : FieldDef) : Unit = {
    field.load(fd)
    fieldDef = fd
  }

  def reloadField() : Unit = {
    field.refill()
    load(fieldDef)
  }

  def initSource(srcIdx : Int) : Unit = {
    val seed = fieldDef.sourceSeeds(srcIdx)
    val prng = new PRNG(seed)
    source = prng.map((i) => fieldDef.units(i % fieldDef.units.size))
  }

  private var source : Iterator[UnitDef] = _
  private var previousUnitClearedLines = 0

  def mapUnit(unit : UnitDef)(f : (Int, Int) => (Int, Int)) : UnitDef = {
    UnitDef (
      unit.members.map({
        case CellDef(x,y) => {
          val res = f(x,y)
          CellDef(res._1, res._2)
        }
      }),
      {
        val res = f(unit.pivot.x, unit.pivot.y)
        CellDef(res._1, res._2)
      }
    )
  }

  // Translate unit coordinates to field coordinates
  // Or just move unit
  def translate(unit : UnitDef)(x0 : Int, y0 : Int) : UnitDef = {
    UnitDef(
      unit.members.map({
        case (CellDef(x,y)) => CellDef(x+x0, y+y0)
      }),
      CellDef(unit.pivot.x+x0, unit.pivot.y+y0)
    )
  }

  // Return true if all members of unit are at empty cells
  def check(unit : UnitDef) : Boolean = {
    for (cell <- unit.members) {
      cell match {
        case CellDef(x,y) =>
          if (! field.isValidCell(x,y)) {
            println(s"($x,$y) is not valid cell.")
            return false
          }
          if (field(x,y) != CellState.Empty) {
            println(s"($x,$y) is not empty.")
            return false
          }
      }
    }
    return true
  }

  // return true if it is possible to spawn new unit
  def spawnUnit(unit : UnitDef) : Boolean = {
    val unitSize = Utils.getUnitActualSize(unit)
    val cY = - unit.members.map(_.y).min
    val minX = unit.members.map(_.x).min
    val cX = (field.width - unitSize._1) / 2 - minX
    //println(s"$cX = (${field.width} - ${unitSize._1}) / 2 - $minX")
    val translated = translate(unit)(cX, cY)
    currentUnit = translated
    return check(currentUnit)
  }

  // return true if it is possible to spawn next unit
  // otherwise the game ends
  def spawnNextUnit(): Boolean = {
    val unit = source.next()
    spawnUnit(unit)
  }

  // Return true if the unit is locked as a result of command execution
  def executeCommand(cmd : Command) : Boolean = {
    def swDeltaX(curY : Int) : Int = {
      if (curY % 2 == 1) {
        0
      } else {
        -1
      }
    }

    currentUnit = cmd match {
                    case Move(direction) =>
                      direction match {
                        case Direction.E => translate(currentUnit)(+1,0)
                        case Direction.W => translate(currentUnit)(-1,0)
                        case Direction.SE => mapUnit(currentUnit)((x,y) =>
                          (x+swDeltaX(y)+1, y+1)
                        )
                        case Direction.SW => mapUnit(currentUnit)((x,y) =>
                          (x+swDeltaX(y), y+1)
                        )
                      }
                  }
    return ! check(currentUnit)
  }

  // Lock current unit: mark all corresponding cells as Full
  def lock(unit : UnitDef) : Unit = {
    unit.members.foreach({
      case CellDef(x,y) => field(x,y) = CellState.Full
    })
  }

  def emulatorStep(cmd : Command) : StepResult = {
    println(s"Execute: $cmd")
    val oldUnit = currentUnit
    val toLock = executeCommand(cmd)
    var gameOver = false
    if (toLock) {
      println("Unit locked.")
      lock(oldUnit)
      val cleared = field.clearRows()
      if (cleared > 0) {
        println(s"$cleared rows cleared.")
      }

      val moveScore = getMoveScore(oldUnit, cleared)
      score += moveScore
      previousUnitClearedLines = cleared

      val nextOk = spawnNextUnit()
      println("Spawning next unit:")
      MapPrinter.printUnit(currentUnit)
      if (! nextOk) {
        println("Game over.")
        gameOver = true
      }
    }
    return StepResult(gameOver, toLock)
  }

  // return number of actually executed commands
  def emulate(cmds : Seq[Command]) : Int = {
    var count = 0
    spawnNextUnit()
    println("First unit:")
    MapPrinter.printUnit(currentUnit)
    for (cmd <- cmds) {
      val res = emulatorStep(cmd)
      count += 1
      if (res.gameOver) {
        return count
      }
    }
    return count
  }

  def printField() : Unit = {
    field.printField()
  }

  private def getMoveScore(unit: UnitDef, cleared: Int): Int = {
    val size = unit.members.size
    val ls = cleared
    val ls_old = previousUnitClearedLines

    // The following were taken from the spec.
    val points = size + 100 * (1 + ls) * ls / 2
    val lineBonus = if (ls_old > 1) {
      (ls_old - 1) * points / 10
    } else {
      0
    }

    points + lineBonus
  }
}

object Emulator {
  def apply(path : String) : Emulator = {
    val fd = Serializer.fromFile(path)
    val field = new Field(fd.width, fd.height)
    val em = new Emulator(field)
    em.load(fd)
    return em
  }
}
