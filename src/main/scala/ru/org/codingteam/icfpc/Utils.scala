package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.UnitDef

/**
 * Created by portnov on 07.08.15.
 */
object Utils {
  def getUnitSize(unit : UnitDef) : (Int, Int) = {
    val w = (for (cell <- unit.members) yield cell.x).max
    val h = (for (cell <- unit.members) yield cell.y).max
    return (w+1, h+1)
  }
}
