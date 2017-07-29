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
  } yield ()

  def outputStatus(context : Context): IO[Unit] = {
    for {
      _ <- writeText(0, 14, "Text:")
      _ <- writeText(0, 16,  context.text.text)
      _ <- writeText(0, 20, "You:")
      _ <- writeText(0, 22, context.result())
    } yield ()
  }

  def initialScreen[R: _config : _context : _io]: Eff[R, Unit]= {
    for {
      context <- get[R, Context]
      _ <- fromIO(clearScreen)
      _ <- fromIO(Output.outputImage(context))
      _ <- fromIO(Output.outputStatus(context))
    } yield ()
  }

  def update[R: _config : _context : _io]: Eff[R, Unit] = {
    def outputDiff(context : Context): IO[Unit] = {
      val offset = context.correctKeys().length - 1
      val key = context.lastkey.get
      if(key.correct) {
        writeText(offset, 22, key.char.toString)
      } else {
        IO.pure(beep)
      }
    }
    for {
      context <- get[R, Context]
      _ <- fromIO(outputDiff(context))
    } yield ()
  }
}
