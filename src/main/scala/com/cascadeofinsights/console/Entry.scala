package com.cascadeofinsights.console

import com.cascadeofinsights.lib.core.TypingImp
import com.cascadeofinsights.lib.util.Data._
import com.cascadeofinsights.lib.util.IOEffect._
import com.cascadeofinsights.lib.util.Terminals._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.future._

import scala.concurrent.ExecutionContext.Implicits.global

object Entry {

  def start[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      word <- fromIO(TypingImp.nextText())
      _ <- put[R, Context](Context.create(word))
      _ <- Output.initialScreen
      _ <- loop
    } yield ()
  }

  def loop[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      key <- fromIO(readKey)
      _ <- modify(Context.update(key))
      _ <- Output.update
      _ <- maybeDone
    } yield ()
  }

  private def maybeDone[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      context <- get[R, Context]
      _ <- (context.complete, context.lastkey) match {
        case (true,_) => results
        case _ => loop
      }
    } yield ()
  }

  private def results[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      context <- get[R, Context]
      result = context.toResult()
      _ <- fromIO(writeText(0,30,s"wpm:${result.wpm}"))
      _ <- fromIO(TypingImp.storeResult(result))
      _ <- exit
    } yield ()
  }

  private def exit[R: _config : _context : _io] = Eff.pure[R, Unit](())

}
