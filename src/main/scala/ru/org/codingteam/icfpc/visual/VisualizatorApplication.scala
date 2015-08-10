package ru.org.codingteam.icfpc.visual

import java.awt.Dimension
import javax.swing.{JFrame, WindowConstants}

import ru.org.codingteam.icfpc.visual.controller.{ReplayController, ManualController}

object VisualizatorApplication {
  def main(args: Array[String]): Unit = {
    //println(args.toList)

    val board = new Board
    val frame = new JFrame
    val controller = args match {
      case Array(filePath) => new ManualController(new Visualizator(board), filePath)
      case Array(filePath, solution) => new ReplayController(new Visualizator(board), filePath, solution)
      case Array(filePath, solution, srcIndex) => new ReplayController(new Visualizator(board), filePath, solution, srcIndex.toInt)
      case _ => new ManualController(new Visualizator(board), "problem_0.json")
    }

    frame.setContentPane(board)
    frame.setTitle(controller.title)
    frame.addKeyListener(controller.keyListener)

    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.pack()
    frame.setSize(new Dimension(600, 800))
    frame.setVisible(true)
  }
}
