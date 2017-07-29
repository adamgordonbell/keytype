package com.cascadeofinsights.lib.util

import aiyou._
import aiyou.implicits._
import cats.implicits._
import com.cascadeofinsights.lib.core._
import com.googlecode.lanterna.input._
import com.googlecode.lanterna.terminal._

import scala.io.Source

object Terminals {
  private val terminal = new DefaultTerminalFactory().createTerminal


  def clearScreen: IO[Unit] = IO.primitive(terminal.clearScreen)

  def beep : IO[Unit] = IO.primitive(terminal.bell())

  def readKey: IO[Key] = IO primitive {
    def readChar: Key = {
      val k = terminal.readInput
      k.getKeyType match {
        case KeyType.Character => Key.create(k.getCharacter)
        case KeyType.Backspace =>  Key.createBackspace()
        case KeyType.Escape => Key.createEscape()
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
