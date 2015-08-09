package ru.org.codingteam.icfpc.visual.controller

import java.awt.event.KeyListener

import ru.org.codingteam.icfpc.Emulator

abstract class Controller(srcIndex: Int = 0) {
  protected def loadProblem(fileName: String): Emulator = {
    val emulator = Emulator(fileName)
    emulator.initSource(srcIndex)
    emulator.spawnNextUnit()
    emulator
  }

  def keyListener: KeyListener
  def title: String
}
