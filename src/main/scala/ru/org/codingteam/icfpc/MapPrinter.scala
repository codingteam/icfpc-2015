package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions._

import scala.io.Source

/**
 * Created by portnov on 07.08.15.
 */
object MapPrinter {

  private def print2d(arr : Seq[CellDef], width : Int, height : Int, mark : String, mbPivot : Option[CellDef]) : Unit = {
    for (y <- List.range(0, height)) {
      if (y % 2 == 1) {
        print("  ")
      }
      for (x <- List.range(0, width)) {
        if (arr.indexOf(CellDef(x, y)) >= 0) {
          print("|" concat  mark)
        } else if (Some(CellDef(x,y)) == mbPivot) {
          print("| o ")
        } else {
          print("|   ")
        }
      }
      print("|\n")
    }
  }

  def printMap(fd : FieldDef) : Unit = {}

  def printUnit(unit : UnitDef) : Unit = {}

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
