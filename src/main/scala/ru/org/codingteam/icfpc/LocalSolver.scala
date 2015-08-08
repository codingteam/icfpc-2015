package ru.org.codingteam.icfpc

import ru.org.codingteam.icfpc.definitions.{UnitDef,CellDef}
import ru.org.codingteam.icfpc.Solver.Position
import scala.math.{floor,ceil,signum,max,abs}
import scala.collection.mutable.PriorityQueue

case class Solution(pathCost: Long, estimatedCost: Long, position: Position,
    commands: List[Command])

object LocalSolver {
    def findPath(field: Field, unit: UnitDef, pos: Position):
    Option[List[Command]] = {
        // FIXME: find out how to do that properly, i.e. with a singleton
        // object or something
        implicit val SolutionOrderer = new Ordering[Solution] {
            def compare(x: Solution, y: Solution): Int =
                        (x.pathCost + x.estimatedCost)
                .compare(y.pathCost + y.estimatedCost)
        }

        val openset: PriorityQueue[Solution] = new PriorityQueue()

        None
    }

    /* Distance between two cells */
    private def distance(a: CellDef, b: CellDef): Int = {
        /* As per section 5 of
         * http://www-cs-students.stanford.edu/~amitp/Articles/HexLOS.html
         */

        def floor2(x: Int): Int = if (x >= 0) (x/2).toInt else ((x-1)/2).toInt
        def ceil2(x: Int): Int = if (x >= 0) ((x+1)/2).toInt else (x/2).toInt

        val dx = (b.x - floor2(b.y) - (a.x - floor2(a.y))).toInt
        val dy = (b.x + ceil2(b.y) - (a.x + ceil2(a.y))).toInt
        if (signum(dx) == signum(dy)) {
            max(abs(dx), abs(dy));
        } else {
            abs(dx) + abs(dy);
        }
    }
}
