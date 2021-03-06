package ru.org.codingteam.icfpc.visual.controller

import java.awt.event.{ActionEvent, ActionListener, KeyEvent, KeyListener}

import javax.swing.Timer

import ru.org.codingteam.icfpc.Utils
import ru.org.codingteam.icfpc.visual.Visualizator

class ReplayController(visualizator: Visualizator, filePath: String, solution: String, srcIndex: Int = 0) extends Controller(srcIndex) with ActionListener {
  private val emulator = loadProblem(filePath)
  private val timer = new Timer(50, this)
  private var commands = Utils.decode(solution)

  visualizator.visualizeState(emulator)
  timer.start()

  override def keyListener: KeyListener = new KeyListener {
    override def keyTyped(keyEvent: KeyEvent): Unit = {}

    override def keyPressed(keyEvent: KeyEvent): Unit = {
      keyEvent.getKeyCode match {
        case KeyEvent.VK_SPACE => if (timer.isRunning) timer.stop() else timer.start()
        case _ =>
      }
    }

    override def keyReleased(keyEvent: KeyEvent): Unit = {}
  }

  override def title: String = "Replay — Codingteam ICFPC 2015"

  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    commands match {
      case cmd :: cmds => {
        emulator.emulatorStep(cmd)
        visualizator.visualizeState(emulator)
        commands = cmds
      }

      case List() => timer.stop()
    }
  }
}
