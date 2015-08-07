package ru.org.codingteam.icfpc.definitions

case class FieldDef(id: Int,
                    units: Array[UnitDef],
                    width: Int,
                    height: Int,
                    filled: Array[CellDef],
                    sourceLength: Int,
                    sourceSeeds: Array[Int])
case class CellDef(x: Int, y: Int)
case class UnitDef(members: Array[CellDef], pivot: CellDef)
