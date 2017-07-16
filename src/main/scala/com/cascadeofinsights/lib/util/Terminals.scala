package com.cascadeofinsights.lib.util

import aiyou._
import aiyou.implicits._
import cats.implicits._
import com.googlecode.lanterna.input._
import com.googlecode.lanterna.terminal._

import scala.io.Source

object Terminals {
  private val terminal = new DefaultTerminalFactory().createTerminal

  case class Key(char: Char)

  def clearScreen: IO[Unit] = IO.primitive(terminal.clearScreen)

  def readKey: IO[Key] = IO primitive {
    def readChar: Char = {
      val k = terminal.readInput
      if (k.getKeyType == KeyType.Character) k.getCharacter else readChar
    }
    Key(readChar)
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
