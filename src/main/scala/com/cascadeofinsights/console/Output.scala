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

  def outputImage(context: Context): IO[Unit] = {
    outputFile(0, 0, s"header.txt")
  }

  def outputStatus(context : Context): IO[Unit] = {
    for {
      _ <- writeText(0, 14, "Text:")
      _ <- writeText(0, 16, "the big fox jumps over the red thingy")
      _ <- writeText(0, 20, "You:")
      _ <- writeText(0, 22, "___ ___")
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
