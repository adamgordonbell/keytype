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

  private def makes(word: String, guesses: Seq[Char]): String =
    word.flatMap(c => if (guesses.contains(c)) s"$c " else "  ")

  private def readFile(file: String): IO[List[String]] = IO.primitive(Source.fromFile(file).getLines.toList)

  private def outputFile(col: Int, row: Int, file: String): IO[Unit] = {
    for {
      ls <- readFile(file)
      _ <- ls.zipWithIndex.map({ case (l, i) => writeText(col, row + i, l) }).sequence
    } yield ()
  }

  def outputImage(context: Context): IO[Unit] = {
    outputFile(0, 0, s"header.txt")
  }

  def outputStatus(context : Context): IO[Unit] = {
    for {
      _ <- writeText(0, 25, "Text:")
      _ <- writeText(0, 27, "the big fox jumps over the red thingy")
      _ <- writeText(0, 28, "You:")
      _ <- writeText(0, 30, "___ ___")
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
