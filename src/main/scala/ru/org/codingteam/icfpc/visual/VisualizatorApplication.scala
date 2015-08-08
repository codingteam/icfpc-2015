package ru.org.codingteam.icfpc.visual

import java.awt.event.{KeyEvent, KeyListener}
import java.awt.{Color, Dimension}
import java.io.File
import javax.swing.{JFileChooser, WindowConstants, JFrame}

import ru.org.codingteam.icfpc.{Move, Emulator, Direction}

object VisualizatorApplication {
  private def loadProblem(fileName: String): Emulator = {
    val emulator = Emulator(fileName)
    emulator.initSource(0)
    emulator.spawnNextUnit()
    emulator
  }

  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Codingteam ICFPC 2015")

    var emulator = loadProblem("problem_0.json")
    val board = new Board
    val visualizator = new Visualizator(board)

    visualizator.visualizeState(emulator)

    frame.addKeyListener(new KeyListener {

      override def keyTyped(keyEvent: KeyEvent): Unit = {}

      override def keyPressed(keyEvent: KeyEvent): Unit = {
        keyEvent.getKeyCode match {
          case KeyEvent.VK_W => emulator.emulatorStep(Move(Direction.W))
          case KeyEvent.VK_E => emulator.emulatorStep(Move(Direction.E))
          case KeyEvent.VK_S => emulator.emulatorStep(Move(Direction.SW))
          case KeyEvent.VK_D => emulator.emulatorStep(Move(Direction.SE))
          case KeyEvent.VK_O => {
            val fileChooser = new JFileChooser()
            fileChooser.setCurrentDirectory(new File(System.getProperty("user.dir")))
            val returnVal = fileChooser.showOpenDialog(frame)
            if (returnVal == JFileChooser.APPROVE_OPTION) {
              val file = fileChooser.getSelectedFile
              emulator = loadProblem(file.getAbsolutePath)
            }
          }
          case _ =>
        }

        visualizator.visualizeState(emulator)
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
