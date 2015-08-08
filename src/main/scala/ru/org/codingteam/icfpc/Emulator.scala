package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.{FieldDef, CellDef, UnitDef}

/**
 * Created by portnov on 07.08.15.
 */

object CellState extends Enumeration {
  type CellState = Value
  val Empty, Full = Value
}

object Direction extends Enumeration {
  type Direction = Value
  val E, W, SE, SW = Value
}

abstract class Command
case class Move(direction : Direction.Direction) extends Command
case class Turn(clockwise : Boolean) extends Command

case class Field(width : Int, height : Int) {
  var field = Array.ofDim[CellState.CellState](width, height)

  def apply(x : Int, y : Int) : CellState.CellState = {
    return field(x)(y)
  }

  def update(x : Int, y : Int, st : CellState.CellState): Unit = {
    field(x)(y) = st
  }
}

class Emulator private (field : Field) {

  // Current unit should be in global field coordinates
  var currentUnit : UnitDef = _

  var fieldDef : FieldDef = _

  private def load(fd : FieldDef) : Unit = {
    fd.filled.foreach({
      case CellDef(x,y) => field(x,y) = CellState.Full
    })
    fieldDef = fd
  }

  def initSource(srcIdx : Int) : Unit = {
    val seed = fieldDef.sourceSeeds(srcIdx)
    val prng = new PRNG(seed)
    source = prng.map((i) => fieldDef.units(i % fieldDef.units.size))
  }

  private var source : Iterator[UnitDef] = _

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
    unit.members.map({
      case (CellDef(x,y)) => field(x,y) == CellState.Empty
    }).forall((x) => x)
  }

  def spawnUnit(unit : UnitDef) : Unit = {
    val unitSize = Utils.getUnitSize(unit)
    val cY = unit.members.map(_.y).max
    val minX = unit.members.map(_.x).min
    val cX = (field.width - unitSize._1) / 2 - minX
    //println(s"$cX = (${field.width} - ${unitSize._1}) / 2 - $minX")
    val translated = translate(unit)(cX, cY)
    currentUnit = translated
  }

  def spawnNextUnit(): Unit = {
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
  def lock() : Unit = {
    currentUnit.members.foreach({
      case CellDef(x,y) => field(x,y) = CellState.Full
    })
  }

  def executeCommands(cmds : Seq[Command]) : Unit = {
    spawnNextUnit
    for (cmd <- cmds) {
      val toLock = executeCommand(cmd)
      if (toLock) {
        lock
        spawnNextUnit
      }
    }
  }

}

object Emulator {
  def apply(path : String) : Emulator = {
    val fd = Serializer.fromFile(path)
    val field = Field(fd.width, fd.height)
    val em = new Emulator(field)
    em.load(fd)
    return em
  }
}
