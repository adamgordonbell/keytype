package com.cascadeofinsights.lib.util

import aiyou._
import aiyou.implicits._
import cats.implicits._
import com.cascadeofinsights.lib.core._
import com.googlecode.lanterna.input._
import com.googlecode.lanterna.terminal._

import scala.io.Source

object Terminals {
  private val terminal = {
    val f = new DefaultTerminalFactory()
     f.setForceTextTerminal(true)
     val term = f.createTerminal
//    val screen = new TerminalScreen(term)
//    val gui = new MultiWindowTextGUI(screen)
//    screen.startScreen()
    term
  }


  def clearScreen: IO[Unit] = IO.primitive(terminal.clearScreen)

  def beep : IO[Unit] = IO.primitive(terminal.bell())

  def readKey: IO[KeyPressEvent] = IO primitive {
    def readChar: KeyPressEvent = {
      val k = terminal.readInput
      k.getKeyType match {
        case KeyType.Character => KeyPressEvent.create(k.getCharacter)
        case KeyType.Backspace =>  KeyPressEvent.createBackspace()
        case KeyType.Escape => KeyPressEvent.createEscape()
        case _ => readChar
      }
    }
    readChar
  }

  def writeText(col: Int, row: Int, text: String): IO[Unit] = IO primitive {
    terminal.setCursorPosition(col, row)
    text.foreach(terminal.putCharacter(_))
    terminal.flush
  }

  def outputFile(col: Int, row: Int, file: String): IO[Unit] = {
    def readFile(file: String): IO[List[String]] = IO.primitive(Source.fromFile(file).getLines.toList)
    for {
      ls <- readFile(file)
      _ <- ls.zipWithIndex.map({ case (l, i) => writeText(col, row + i, l) }).sequence
    } yield ()
  }
}
