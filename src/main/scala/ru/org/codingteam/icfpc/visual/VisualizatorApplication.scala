package ru.org.codingteam.icfpc.visual

import java.awt.Dimension
import java.awt.event.{KeyEvent, KeyListener}
import java.io.File
import javax.swing.{JFileChooser, JFrame, WindowConstants}

import ru.org.codingteam.icfpc._

import scala.collection.mutable.ListBuffer

object VisualizatorApplication {
  private def loadProblem(fileName: String): Emulator = {
    val emulator = Emulator(fileName)
    emulator.initSource(0)
    emulator.spawnNextUnit()
    emulator
  }

  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Codingteam ICFPC 2015")

    println(args.toList)

    val filePath = if (args.length == 1) args(0) else "problem_0.json"

    var emulator = loadProblem(filePath)
    val board = new Board
    val visualizator = new Visualizator(board)

    val commands = new ListBuffer[Command]()

    visualizator.visualizeState(emulator)

    frame.addKeyListener(new KeyListener {

      override def keyTyped(keyEvent: KeyEvent): Unit = {}

      override def keyPressed(keyEvent: KeyEvent): Unit = {

        def process(cmd : Command) : StepResult = {
          val result = emulator.emulatorStep(cmd)
          commands += cmd
          result
        }

        val res : StepResult = keyEvent.getKeyCode match {
                    case KeyEvent.VK_W => process(Move(Direction.W))
                    case KeyEvent.VK_E => process(Move(Direction.E))
                    case KeyEvent.VK_S => process(Move(Direction.SW))
                    case KeyEvent.VK_D => process(Move(Direction.SE))
                    case KeyEvent.VK_COMMA => process(Turn(true))
                    case KeyEvent.VK_PERIOD => process(Turn(false))
                    case KeyEvent.VK_O => {
                      val fileChooser = new JFileChooser()
                      fileChooser.setCurrentDirectory(new File(System.getProperty("user.dir")))
                      val returnVal = fileChooser.showOpenDialog(frame)
                      if (returnVal == JFileChooser.APPROVE_OPTION) {
                        val file = fileChooser.getSelectedFile
                        emulator = loadProblem(file.getAbsolutePath)
                        commands.clear()
                      }
                      StepResult(false, false)
                    }
                    case KeyEvent.VK_P => {
                      println(Utils.encodeSimple(commands.toList))
                      StepResult(false, false)
                    }
                    case _ => StepResult(false, false)
                  }
        if (res.gameOver) {
          board.setGameOver(true)
          println(Utils.encodeSimple(commands.toList))
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
