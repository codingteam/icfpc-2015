package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.BottomSolver.Position
import ru.org.codingteam.icfpc.definitions.UnitDef

import scala.collection.mutable.{HashSet, PriorityQueue}
import scala.math.{abs, max, signum}

case class Solution(pathCost: Long, estimatedCost: Long, position: Position,
    noOfCWTurns: Int, commands: List[Command]) {

    // that's how I want it to be for HashSet to work
    override def hashCode = position.hashCode

    override def equals(obj: scala.Any): Boolean = obj match {
        case Solution(_, _, pos, _, _) => pos == position
        case _ => false
    }
}

object LocalSolver {
    def findPath(field: Field, unit: UnitDef, pos: Position,
    phrases: Set[String]): Option[List[Command]] = {
        // the lower the cost the better
        implicit val SolutionOrderer = new Ordering[Solution] {
            def compare(x: Solution, y: Solution): Int =
                -(x.pathCost + x.estimatedCost)
                .compare(y.pathCost + y.estimatedCost)
        }

        val spells: Set[List[Command]] = phrases.map(Utils.decode)

        // need our own instance so that we can call getSpawnPosition
        val emul: Emulator = new Emulator(field, phrases)

        val openset: PriorityQueue[Solution] = new PriorityQueue()
        val closedset: HashSet[Solution] = new HashSet()
        val (cX, cY) = emul.getSpawnPositionShifts(unit)
        val spawnPosition: Position = ( unit.pivot.x + cX,
                                        unit.pivot.y + cY )
        openset.enqueue(new Solution(0,
                                     distance(spawnPosition, pos),
                                     spawnPosition,
                                     0,
                                     List()))

        var solution: Solution = null

        while (!openset.isEmpty && solution == null) {
            val current = openset.dequeue
            if(current.position == pos) {
                // great, we've found a solution! Now let's find one final
                // command that will lock the unit in place
                val newX = current.position._1 - unit.pivot.x
                val newY = current.position._2 - unit.pivot.y
                val translated = emul.translate(unit)(newX, newY)

                val lockDir = List(Direction.E, Direction.W,
                                   Direction.SE, Direction.SW)
                    .map(dir => (dir, emul.willLock(translated, Move(dir))))
                    .filter(x => x._2)
                    .head
                    ._1

                solution = new Solution(current.pathCost,
                                        current.estimatedCost,
                                        current.position,
                                        current.noOfCWTurns,
                                        Move(lockDir) :: current.commands)
            } else {
                if (closedset.add(current)) {

                    // extend current solution with each possible command and add
                    // the result to openset

                    def go(dir: Direction.Direction): Solution = {
                    val new_pos =
                        Emulator.translateCoord(dir)(current.position._1,
                                                     current.position._2)
                        new Solution(current.pathCost + 3,
                            distance(new_pos, pos),
                            new_pos,
                            current.noOfCWTurns,
                            Move(dir) :: current.commands)
                    }

                    def turn(clockwise: Boolean): Solution = {
                        val turns = ( (if (clockwise) 1 else -1)
                                    + current.noOfCWTurns + 1
                                    ) % 6
                        new Solution(current.pathCost + 2,
                                     current.estimatedCost,
                                     current.position,
                                     turns,
                                     Turn(clockwise) :: current.commands)
                    }

                    val newX = current.position._1 - unit.pivot.x
                    val newY = current.position._2 - unit.pivot.y
                    val rotated = {0 to current.noOfCWTurns+1}
                    .foldLeft(unit)((u, _) => emul.tryCommand(u, Turn(true)))
                    val translated = emul.translate(rotated)(newX, newY)

                    if (!emul.willLock(translated, Move(Direction.E))) {
                        openset.enqueue(go(Direction.E))
                    }
                    if (!emul.willLock(translated, Move(Direction.W))) {
                        openset.enqueue(go(Direction.W))
                    }
                    if (!emul.willLock(translated, Move(Direction.SE))) {
                        openset.enqueue(go(Direction.SE))
                    }
                    if (!emul.willLock(translated, Move(Direction.SW))) {
                        openset.enqueue(go(Direction.SW))
                    }
                    if (!emul.willLock(translated, Turn(false))) {
                        openset.enqueue(turn(false))
                    }
                    if (!emul.willLock(translated, Turn(true))) {
                        openset.enqueue(turn(true))
                    }

                    // and now, let's try the spells
                    def executeCommandSequence(unit: UnitDef,
                    cmds: List[Command]): UnitDef =
                    if(cmds.isEmpty) {
                        unit
                    } else {
                        executeCommandSequence(emul.tryCommand(unit, cmds.head),
                                               cmds.tail)
                    }

                    for(spell <- spells) {
                        if(! emul.willLockSequence(translated, spell)) {
                            val turns = spell.map(
                                x => x match {
                                    case Move(_)  => 0
                                    case Turn(cw) => if(cw) 1 else -1
                                }).sum
                            val result = executeCommandSequence(translated,
                                                                spell)
                            val new_position =
                                (result.pivot.x - current.position._1,
                                 result.pivot.y - current.position._2)
                            val estimate = distance(current.position,
                                                    new_position)

                            openset.enqueue(
                                new Solution(
                                    // cost of using a spell is 0
                                    current.pathCost,
                                    estimate,
                                    new_position,
                                    turns,
                                    spell.reverse ::: current.commands))
                        }
                    }
                }
            }
        }

        if(solution == null) None else Some(solution.commands.reverse)
    }

    /* Distance between two cells */
    private def distance(a: (Int, Int), b: (Int, Int)): Int = {
        /* As per section 5 of
         * http://www-cs-students.stanford.edu/~amitp/Articles/HexLOS.html
         */

        def floor2(x: Int): Int = if (x >= 0) (x/2).toInt else ((x-1)/2).toInt
        def ceil2(x: Int): Int = if (x >= 0) ((x+1)/2).toInt else (x/2).toInt

        val dx = (b._1 - floor2(b._2) - (a._1 - floor2(a._2))).toInt
        val dy = (b._1 + ceil2(b._2) - (a._1 + ceil2(a._2))).toInt
        if (signum(dx) == signum(dy)) {
            max(abs(dx), abs(dy));
        } else {
            abs(dx) + abs(dy);
        }
    }
}
