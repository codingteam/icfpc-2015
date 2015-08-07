package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions._

/**
 * Created by portnov on 07.08.15.
 */
object MapPrinter {

  def printMap(fd : FieldDef) : Unit = {
    for (y <- List.range(0, fd.height)) {
      if (y % 2 == 1) {
        print("  ")
      }
      for (x <- List.range(0, fd.width)) {
        if (fd.filled.indexOf(CellDef(x, y)) >= 0) {
          print("| x ")
        } else {
          print("|   ")
        }
      }
      print("|\n")
    }
  }

  val testMap : FieldDef =
    FieldDef(1,Vector.empty,6,6,
      Vector(CellDef(2,0), CellDef(3,0), CellDef(2,5)),
      0,
      Vector(0)
    )

  def test() : Unit = {
    printMap(testMap)
  }

}
