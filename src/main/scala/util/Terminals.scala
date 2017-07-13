package util

import aiyou._
import com.googlecode.lanterna.input._
import com.googlecode.lanterna.terminal._

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
}
