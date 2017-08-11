package com.cascadeofinsights.console

import com.cascadeofinsights.lib.core.{Escape, TypedKey}
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
      config <- ask[R, Config]
      word <- Eff.pure(config.typing.nextText())
      _ <- put[R, Context](Context.create(word))
      _ <- Output.initialEntryScreen
      _ <- loop
    } yield ()
  }

  def loop[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      key <- fromIO(readKey)
      _ <- modify(Context.update(key))
      _ <- Output.updateTypingArea
      _ <- maybeDone
    } yield ()
  }

  private def maybeDone[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      context <- get[R, Context]
      _ <- (context.complete, context.lastkey) match {
        case (true,_) => results
        case (false,Some(TypedKey(Escape,_,_))) => results
        case _ => loop
      }
    } yield ()
  }

  private def results[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      context <- get[R, Context]
      results = context.toResult()
      config <- ask[R, Config]
      _ <- fromIO(config.typing.storeResults(results))//Todo make total
      _ <- exit
    } yield ()
  }

  private def exit[R: _config : _context : _io] = Eff.pure[R, Unit](())

}
