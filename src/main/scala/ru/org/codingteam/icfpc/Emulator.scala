package ru.org.codingteam.icfpc

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
case class Move(direction : Direction.Direction)
case class Turn(clockwise : Boolean)

class Field(width : Int, height : Int) {
  var field = Array.ofDim[CellState.CellState](width, height)

  def apply(x : Int, y : Int) : CellState.CellState = {
    return field(x)(y)
  }
}

object Emulator {

}
