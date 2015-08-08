package ru.org.codingteam.icfpc.definitions

import ru.org.codingteam.icfpc.PRNG

case class FieldDef(id: Int,
                    units: Vector[UnitDef],
                    width: Int,
                    height: Int,
                    filled: Vector[CellDef],
                    sourceLength: Int,
                    sourceSeeds: Vector[Int]) {
  def getUnits(seed: Int): Stream[UnitDef] = {
    val prng = new PRNG(seed)
    prng.map((i) => units(i % units.size)).toStream.take(sourceLength)
  }
}

case class CellDef(x: Int, y: Int)
case class UnitDef(members: Vector[CellDef], pivot: CellDef)
case class OutputDef(problemId: Int, seed: Int, tag: String, solution: String)
