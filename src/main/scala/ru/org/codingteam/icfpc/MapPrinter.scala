package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions._

import scala.io.Source

/**
 * Created by portnov on 07.08.15.
 */
object MapPrinter {

  private def print2d(arr : Seq[CellDef], width : Int, height : Int, mark : String) : Unit = {
    for (y <- List.range(0, height)) {
      if (y % 2 == 1) {
        print("  ")
      }
      for (x <- List.range(0, width)) {
        if (arr.indexOf(CellDef(x, y)) >= 0) {
          print("|" concat  mark)
        } else {
          print("|   ")
        }
      }
      print("|\n")
    }
  }

  def printMap(fd : FieldDef) : Unit = {

    print2d(fd.filled, fd.width, fd.height, "XXX")

    for (unit <- fd.units) {
      println("\nUnit:")
      Utils.getUnitSize(unit) match {
        case (width, height) =>
          print2d(unit.members, width, height, "UUU")
      }
    }
  }

  def printUnit(unit : UnitDef) : Unit = {
    val size = Utils.getUnitSize(unit)
    print2d(unit.members, size._1, size._2, "UUU")
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

  def printTask(filename : String) : Unit = {
    val fd = Serializer.deserialize( Source.fromFile(filename).mkString )
    printMap(fd)
  }

}
