package com.cascadeofinsights.console

import aiyou._
import com.cascadeofinsights.lib.core.{Character, TypedKey, TypingImp}
import com.cascadeofinsights.lib.util.Data._
import com.cascadeofinsights.lib.util.IOEffect._
import com.cascadeofinsights.lib.util.Terminals._
import org.atnos.eff._
import org.atnos.eff.all._

object Output {

  def initialScreen[R: _config : _context : _io]: Eff[R, Unit]= {
    for {
      context <- get[R, Context]
      _ <- fromIO(clearScreen)
      _ <- fromIO(Output.header(context))
      _ <- fromIO(Output.stats())
      _ <- fromIO(Output.typeArea(context))
    } yield ()
  }

  private def typeArea(context : Context): IO[Unit] = {
    for {
      _ <- writeText(0, 14, "Text:")
      _ <- writeText(0, 16,  context.text.text)
      _ <- writeText(0, 20, "You:")
      _ <- writeText(0, 22, context.result())
    } yield ()
  }

  private def header(context: Context): IO[Unit] = for {
    _ <- outputFile(0, 0, s"header.txt")
  } yield ()

  def stats(): IO[Unit] = IO.pure{
    val recent = TypingImp.results.take(3).zipWithIndex
    for((r,i) <- recent){
      writeText(0,11+i,s"WPM $i : ${r.wpm}").unsafePerformIO() //ToDo no unsafe
    }
  }

  def updateTypingArea[R: _config : _context : _io]: Eff[R, Unit] = {
    def outputDiff(context : Context): IO[Unit] = {
      val offset = context.correctKeys().length - 1
      val key = context.lastkey.get
      key match {
        case TypedKey(Character(x), _, true) =>
          writeText(offset, 22, x.toString)
        case TypedKey(Character(x),_, false) =>
            IO.pure(beep)
        case _ => IO.pure(())
      }
    }
    for {
      context <- get[R, Context]
      _ <- fromIO(outputDiff(context))
    } yield ()
  }
}
