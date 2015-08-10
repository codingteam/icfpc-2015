package ru.org.codingteam.icfpc.visual.controller

import java.awt.event.{KeyEvent, KeyListener}
import java.io.File
import javax.swing.JFileChooser

import ru.org.codingteam.icfpc._
import ru.org.codingteam.icfpc.visual.Visualizator

import scala.collection.mutable.ListBuffer

class ManualController(visualizator: Visualizator, filePath: String) extends Controller {
  private val commands = new ListBuffer[Command]()
  private var emulator = loadProblem(filePath)

  visualizator.visualizeState(emulator)

  override def title: String = "Manual Control — Codingteam ICFPC 2015"

  override def keyListener: KeyListener = new KeyListener {
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
          val returnVal = fileChooser.showOpenDialog(null)
          if (returnVal == JFileChooser.APPROVE_OPTION) {
            val file = fileChooser.getSelectedFile
            emulator = loadProblem(file.getAbsolutePath)
            commands.clear()
          }
          StepResult(false, false)
        }
        case KeyEvent.VK_P => {
          //println(Utils.encode(commands.toList))
          StepResult(false, false)
        }
        case _ => StepResult(false, false)
      }
      if (res.gameOver) {
        //println(Utils.encode(commands.toList))
      }

      visualizator.visualizeState(emulator)
    }

    override def keyReleased(keyEvent: KeyEvent): Unit = {}
  }
}
