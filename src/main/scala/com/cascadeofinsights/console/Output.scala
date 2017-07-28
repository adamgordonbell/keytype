package com.cascadeofinsights.console

import aiyou._
import com.cascadeofinsights.lib.util.Data._
import com.cascadeofinsights.lib.util.IOEffect._
import com.cascadeofinsights.lib.util.Terminals._
import org.atnos.eff._
import org.atnos.eff.all._

object Output {

  def outputImage(context: Context): IO[Unit] = for {
    _ <- outputFile(0, 0, s"header.txt")
    _ <- writeText(0, 13, s". = exit")
  } yield ()

  def outputStatus(context : Context): IO[Unit] = {
    for {
      _ <- writeText(0, 14, "Text:")
      _ <- writeText(0, 16,  context.text.text)
      _ <- writeText(0, 20, "You:")
      _ <- writeText(0, 22, context.result())
    } yield ()
  }

  def initialScreen[R: _config : _context : _io]: Eff[R, Unit]= outputScreen

  def outputScreen[R: _config : _context : _io]: Eff[R, Unit] = {
    for {
      _ <- fromIO(clearScreen)
      context <- get[R, Context]
      _ <- fromIO(Output.outputImage(context))
      _ <- fromIO(Output.outputStatus(context))
    } yield ()
  }
}
