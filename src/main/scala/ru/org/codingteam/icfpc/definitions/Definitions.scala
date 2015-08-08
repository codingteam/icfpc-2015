package ru.org.codingteam.icfpc.definitions

case class FieldDef(id: Int,
                    units: Vector[UnitDef],
                    width: Int,
                    height: Int,
                    filled: Vector[CellDef],
                    sourceLength: Int,
                    sourceSeeds: Vector[Int])
case class CellDef(x: Int, y: Int)
case class UnitDef(members: Vector[CellDef], pivot: CellDef)
case class OutputDef(problemId: Int, seed: Int, tag: String, solution: String)
