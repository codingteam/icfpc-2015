package ru.org.codingteam.icfpc.visual

import java.awt.event.{KeyEvent, KeyListener}
import java.awt.{Color, Dimension}
import javax.swing.{WindowConstants, JFrame}

import ru.org.codingteam.icfpc.{Move, Emulator, Direction}

object VisualizatorApplication {
  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Codingteam ICFPC 2015")

    val emulator = Emulator("problem_1.json")
    val board = new Board(emulator.fieldDef.height, emulator.fieldDef.width)
    val visualizator = new Visualizator(emulator, board)

    emulator.initSource(0)
    emulator.spawnNextUnit();
    visualizator.visualizeState

    frame.addKeyListener(new KeyListener {

      override def keyTyped(keyEvent: KeyEvent): Unit = {}

      override def keyPressed(keyEvent: KeyEvent): Unit = {
        keyEvent.getKeyCode match {
          case KeyEvent.VK_W => emulator.emulatorStep(Move(Direction.W))
          case KeyEvent.VK_E => emulator.emulatorStep(Move(Direction.E))
          case KeyEvent.VK_S => emulator.emulatorStep(Move(Direction.SW))
          case KeyEvent.VK_D => emulator.emulatorStep(Move(Direction.SE))
          case _ =>
        }
        visualizator.visualizeState
      }

      override def keyReleased(keyEvent: KeyEvent): Unit = {}
    })


    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setContentPane(board)
    frame.pack()
    frame.setSize(new Dimension(600, 800))
    frame.setVisible(true)
  }
}
