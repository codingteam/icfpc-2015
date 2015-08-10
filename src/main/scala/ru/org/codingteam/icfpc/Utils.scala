package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.UnitDef

import scala.collection.mutable.ListBuffer
import scala.io.Source
import upickle.default._

object CellState extends Enumeration {
  type CellState = Value
  val Empty, Full = Value
}

object Direction extends Enumeration {
  type Direction = Value
  val E, W, SE, SW = Value
}


abstract class Command
case class Move(direction : Direction.Direction) extends Command
case class Turn(clockwise : Boolean) extends Command

/**
 * Created by portnov on 07.08.15.
 */

case class Score(
                powerScore : Int,
                seed : Int,
                tag : String,
                createdAt : String,
                score : Int,
                authorId : Int,
                teamId : Int,
                problemId : Int,
                solution : String
                  )

object Utils {

  type Spell = (String, List[Command])
  // Use radix tree instead?
  type Spells = List[Spell]

  val t9 : Map[Command, List[Char]] =
    Map(Move(Direction.W) -> "p'!.03".toList,
        Move(Direction.E) -> "bcefy2".toList,
        Move(Direction.SW) -> "aghij4".toList,
        Move(Direction.SE) -> "lmno 5".toList,
        Turn(true) -> "dqrvz7".toList,
        Turn(false) -> "kstuwx".toList
       )

  val unt9 : Map[Char, Command] =
    t9.toList.flatMap({
      case (k, letters) => for (letter <- letters) yield (letter, k)
    }).toMap

  def toSpell(str : String) : Spell = {
    (str, decode(str))
  }

  val knownSpells : Spells =
    List("ei!", "io! io!", "yuggoth", "r'lyeh") map toSpell

  def decode(str : String) : List[Command] = {
    str.toList.map((c) => unt9.get(c).get)
  }

  def encodeSimple(cmds : Seq[Command]) : String = {
    cmds.toList.map((c) => t9.get(c).get(0)).mkString("")
  }

  def encode(cmds : Seq[Command]) : String = {

    def filterSpells(prefix : Seq[Command], spells : Spells) : Spells = {
      spells.filter(s => s._2.startsWith(prefix))
    }

    var result = ""
    var buffer = new ListBuffer[Command]()
    var currentSpells = knownSpells
    for (cmd <- cmds) {
      buffer += cmd
      val bufstr = buffer.toList.mkString("")
      //println(s"Buffer: ${bufstr}")
      val goodSpells = filterSpells(buffer, currentSpells)
      //println(s"Good spells: ${goodSpells}")
      if (goodSpells.size == 1 && goodSpells(0)._2 == buffer.toList) {
        result += goodSpells(0)._1
        currentSpells = knownSpells
        buffer.clear()
      } else if (goodSpells.isEmpty) {
        result += encodeSimple(buffer)
        currentSpells = knownSpells
        buffer.clear()
      } else {

      }
      //println(s"Result: $result")
    }
    result += encodeSimple(buffer)
    return result
  }

  def evalString(filePath : String, commands : String, srcIdx : Int) : Int = {
    val em = Emulator(filePath)
    em.initSource(srcIdx)
    val cmds = Utils.decode(commands)
    val cmdsCnt = em.emulate(cmds)
    //println(s"Executed $cmdsCnt commands")
    print("\n")
    em.printField()
    //println(s"\nScore: ${em.score}")
    return em.score
  }

  def readScores(filePath : String) : List[Score] = {
    val content = Source.fromFile(filePath).mkString
    read[List[Score]](content)
  }

  def getScore(filePath : String, problemId : Int, seed : Int) : List[Int] = {
    val scores = readScores(filePath)
    for (score <- scores if (score.problemId == problemId) && (score.seed == seed))
      yield score.score
  }

  // Size of only unit itself, without empty margins
  def getUnitActualSize(unit : UnitDef) : (Int, Int) = {
    val maxX = (for (cell <- unit.members) yield cell.x).max
    val minX = (for (cell <- unit.members) yield cell.x).min
    val maxY = (for (cell <- unit.members) yield cell.y).max
    val minY = (for (cell <- unit.members) yield cell.y).min
    val w = maxX - minX + 1
    val h = maxY - minY + 1
    return (w, h)
  }

  // Size of all rectangle used by unit
  def getUnitSize(unit : UnitDef) : (Int, Int) = {
    val w = (for (cell <- unit.members) yield cell.x).max
    val h = (for (cell <- unit.members) yield cell.y).max
    return (w+1, h+1)
  }
}
