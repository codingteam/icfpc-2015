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

  "The Emulator" should "execute sequence of commands" in {
    val em = Emulator("problem_0.json")
    em.initSource(0)
    val ei = Utils.decode("ei!")
    val eiei = Stream.continually(ei.toStream).flatten.take(200)

    val cmdsCnt = em.emulate(eiei)
    println(s"Executed $cmdsCnt commands")
    print("\n")
    em.printField()
  }

  it should "return a score of 1 when locked the unit" in {
    val em = Emulator("problem_0.json")
    em.initSource(0)
    val commands = List.fill(7)(Move(Direction.W))
    em.emulate(commands)
    assert(em.score === 1)
  }
}
