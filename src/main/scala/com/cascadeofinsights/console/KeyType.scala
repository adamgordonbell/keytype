package com.cascadeofinsights.console

import com.cascadeofinsights.lib.util.Data._
import com.cascadeofinsights.lib.util.IOEffect._
import com.cascadeofinsights.lib.util.Terminals._
import com.cascadeofinsights.lib.util.{TypedKey, TypingImp}
import org.atnos.eff.all._
import org.atnos.eff.future._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.future._
import org.atnos.eff.{ExecutorServices, _}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object KeyType extends App {
  implicit val scheduler = ExecutorServices.schedulerFromScheduledExecutorService(ExecutorServices.scheduledExecutor(10))

  def startGame[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      word <- fromIO(TypingImp.nextText())
      _ <- put[R, Context](Context.create(word))
      _ <- Output.initialScreen
      _ <- gameLoop
    } yield ()
  }

  def gameLoop[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      key <- fromIO(readKey)
      _ <- modify(Context.update(key))
      _ <- Output.outputScreen
      _ <- maybeExit
    } yield ()
  }

  private def maybeExit[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      context <- get[R, Context]
      _ <- (context.complete, context.lastkey) match {
        case (true,_) => results
        case (_,Some(TypedKey('.', _ ,_))) => results
        case _ => gameLoop
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

  Await.result(
    startGame[Stack]
      .runReader(Config.empty)
      .runState(Context.empty)
      .runIO
      .runSequential,
    Duration.Inf)
}
