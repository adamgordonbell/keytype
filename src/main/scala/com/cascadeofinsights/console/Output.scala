package com.cascadeofinsights.console

import aiyou._
import aiyou.implicits._
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.all._
import com.cascadeofinsights.lib.util.Data._
import com.cascadeofinsights.lib.util.IOEffect._
import com.cascadeofinsights.lib.util.Terminals._

import scala.io.Source

object Output {

  private def makes(word: String, guesses: Set[Char]): String =
    word.flatMap(c => if (guesses.contains(c)) s"$c " else "  ")

  private def readFile(file: String): IO[List[String]] = IO.primitive(Source.fromFile(file).getLines.toList)

  private def outputFile(col: Int, row: Int, file: String): IO[Unit] = {
    for {
      ls <- readFile(file)
      _ <- ls.zipWithIndex.map({ case (l, i) => writeText(col, row + i, l) }).sequence
    } yield ()
  }

  def outputImage(context: Context): IO[Unit] =
    outputFile(0, 8, s"${context.numMisses}-miss.txt")

  def outputStatus(context : Context): IO[Unit] = {
    for {
      _ <- writeText(10, 9, makes(context.word, context.guesses))
      _ <- writeText(10, 10, List.fill(context.word.size)('-').mkString(" "))
      _ <- writeText(10, 12, s"Misses: ${context.misses}")
      _ <- context.calculateResult match {
        case Continue => IO.pure(())
        case YouWin => writeText(10, 14, "You win!!\n")
        case YouLose => writeText(10, 14, s"The word is ${context.word}.  You Lose.\n")
      }
    } yield ()
  }

  def outputScreen[R: _config : _context : _io]: Eff[R, Unit] = {
    for {
      _ <- fromIO(clearScreen)
      context <- get[R, Context]
      _ <- fromIO(Output.outputImage(context))
      _ <- fromIO(Output.outputStatus(context))
    } yield ()
  }
}
