package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.UnitDef

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

/**
 * Created by portnov on 07.08.15.
 */
object Utils {

  val t9 : Map[Command, List[Char]] =
    Map(Move(Direction.W) -> "p'!.03".toList,
        Move(Direction.E) -> "bcefy2".toList,
        Move(Direction.SW) -> "aghij4".toList,
        Move(Direction.SE) -> "lmno 5".toList,
        Turn(true) -> "dqrvz7".toList,
        Turn(false) -> "kstuwx".toList
       )

  val unt9 : Map[Char, Command] =
    t9.toList.flatMap({
      case (k, letters) => for (letter <- letters) yield (letter, k)
    }).toMap

  def decode(str : String) : List[Command] = {
    str.toList.map((c) => unt9.get(c).get)
  }

  def encodeSimple(cmds : Seq[Command]) : String = {
    cmds.toList.map((c) => t9.get(c).get(0)).mkString("")
  }

  // Size of only unit itself, without empty margins
  def getUnitActualSize(unit : UnitDef) : (Int, Int) = {
    val maxX = (for (cell <- unit.members) yield cell.x).max
    val minX = (for (cell <- unit.members) yield cell.x).min
    val maxY = (for (cell <- unit.members) yield cell.y).max
    val minY = (for (cell <- unit.members) yield cell.y).min
    val w = maxX - minX + 1
    val h = maxY - minY + 1
    return (w, h)
  }

  // Size of all rectangle used by unit
  def getUnitSize(unit : UnitDef) : (Int, Int) = {
    val w = (for (cell <- unit.members) yield cell.x).max
    val h = (for (cell <- unit.members) yield cell.y).max
    return (w+1, h+1)
  }
}
