package ru.org.codingteam.icfpc.visual.controller

import java.awt.event.KeyListener

import ru.org.codingteam.icfpc.Emulator

abstract class Controller {
  protected def loadProblem(fileName: String): Emulator = {
    val emulator = Emulator(fileName)
    emulator.initSource(0)
    emulator.spawnNextUnit()
    emulator
  }

  def keyListener: KeyListener
  def title: String
}
