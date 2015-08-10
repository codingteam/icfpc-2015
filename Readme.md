ICFPC 2015
==========

That is our solution for ICFP Contest 2015.

_NOTE_: we were about 153 in the qualification round before the leaderboard gone down.

Dependencies
------------

* [sbt][]

[sbt]: http://www.scala-sbt.org/

Usage
-----

    # Run application for submission
    > sbt "run -f filename.json [-print true] [-p phrase1] [-p phrase2] ..." 

    # Run The Visualizator:
    # If solution is not provided The Visualizator starts in Manual Mode
    > sbt "run-main ru.org.codingteam.icfpc.visual.VisualizatorApplication [filename.json] [solution] [srcIndex]"

Visualizer Shorcuts
-------------------

## Manual Mode ##

- `W` — Move(W)
- `E` — Move(E)
- `S` — Move(SW)
- `D` — Move(SE)
- `,` — Turn(clockwise = true)
- `.` — Turn(clockwise = false)
- `O` — Open another problem file

## Replay Mode ##

- `SPACE` — pause/resume replay

Team
----
Team members in alphabetical order:

- [ForNeVeR][fornever]
- [grouzen][]
- [Minoru][minoru]
- [portnov][]
- [rexim][]
- [ulidtko][]

[fornever]: https://github.com/ForNeVeR
[grouzen]: https://github.com/grouzen
[minoru]: https://github.com/Minoru
[portnov]: https://github.com/portnov
[rexim]: https://github.com/rexim
[ulidtko]: https://github.com/ulidtko
