package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.BottomSolver.Position
import ru.org.codingteam.icfpc.definitions.UnitDef

import scala.collection.mutable.{HashSet, PriorityQueue}
import scala.math.{abs, max, signum}

case class Solution(pathCost: Long, estimatedCost: Long, position: Position,
    commands: List[Command]) {

    // that's how I want it to be for HashSet to work
    override def hashCode = position.hashCode

    override def equals(obj: scala.Any): Boolean = obj match {
        case Solution(_, _, pos, _) => pos == position
        case _ => false
    }
}

object LocalSolver {
    def findPath(field: Field, unit: UnitDef, pos: Position):
    Option[List[Command]] = {
        // the lower the cost the better
        implicit val SolutionOrderer = new Ordering[Solution] {
            def compare(x: Solution, y: Solution): Int =
                -(x.pathCost + x.estimatedCost)
                .compare(y.pathCost + y.estimatedCost)
        }

        // need our own instance so that we can call getSpawnPosition
        val emul: Emulator = new Emulator(field)

        val openset: PriorityQueue[Solution] = new PriorityQueue()
        val closedset: HashSet[Solution] = new HashSet()
        val (cX, cY) = emul.getSpawnPositionShifts(unit)
        val spawnPosition: Position = ( unit.pivot.x + cX,
                                        unit.pivot.y + cY )
        openset.enqueue(new Solution(0,
                                     distance(spawnPosition, pos),
                                     spawnPosition,
                                     List()))

        var solution: Solution = null

        while (!openset.isEmpty && solution == null) {
            val current = openset.dequeue
            if(current.position == pos) {
                // great, we've found a solution! Now let's find one final
                // command that will lock the unit in place
                val translated = emul.translate(unit)(current.position._1,
                                                      current.position._2)
                val lockDir = List(Direction.E, Direction.W,
                                   Direction.SE, Direction.SW)
                    .map(dir => (dir, emul.willLock(translated, Move(dir))))
                    .filter(x => x._2)
                    .head
                    ._1

                solution = new Solution(current.pathCost,
                                        current.estimatedCost,
                                        current.position,
                                        Move(lockDir) :: current.commands)
            } else {
                if (closedset.add(current)) {

                    // extend current solution with each possible command and add
                    // the result to openset

                    def go(dir: Direction.Direction): Solution = {
                    val new_pos =
                        Emulator.translateCoord(dir)(current.position._1,
                                                     current.position._2)
                        new Solution(current.pathCost + 1,
                            distance(new_pos, pos),
                            new_pos,
                            Move(dir) :: current.commands)
                    }

                    val translated = emul.translate(unit)(current.position._1,
                        current.position._2)

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
