package ru.org.codingteam.icfpc

import org.scalatest.{Matchers, FlatSpec}
import ru.org.codingteam.icfpc.definitions.CellDef

/**
 * Created by portnov on 08.08.15.
 */
class EmulatorSpec extends FlatSpec with Matchers{

  "The Emulator" should "spawn unit in correct position" in {
    val em = Emulator("problem_0.json")
    em.spawnUnit(em.fieldDef.units(1))
    val expected = Vector(CellDef(3,0), CellDef(5,0))
    assert(em.currentUnit.members === expected)
  }

  "The Emulator" should "move unit into correct position" in {
    val em = Emulator("problem_0.json")
    em.spawnUnit(em.fieldDef.units(1))
    em.executeCommand(Move(Direction.SE))
    val expected = Vector(CellDef(3,1), CellDef(5,1))
    assert(em.currentUnit.members === expected)
  }
}
