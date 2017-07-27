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

  def gameLoop[R: _config : _context : _io]: Eff[R, Unit] = {
    for {
      _ <- Output.outputScreen
      key <- fromIO(readKey)
      _ <- modify(Context.update(key))
      context <- get[R, Context]
      _ <- maybeExit // unless we are done
    } yield ()
  }

  def maybeExit[R: _config : _context : _io]: Eff[R, Unit] = {
    for {
      context <- get[R, Context]
      _ <- (context.complete, context.lastkey) match {
        case (true,_) => Eff.pure[R, Unit](())
        case (_,Some(TypedKey('z', _ ,_))) => Eff.pure[R, Unit](())
        case _ => gameLoop
      }
    } yield ()
  }

  def startGame[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      word <- fromFuture(TypingImp.nextText())
      _ <- put[R, Context](Context.create(word))
      _ <- gameLoop
      _ <- Output.outputScreen
    } yield ()
  }

  Await.result(
    startGame[Stack]
      .runReader(Config.empty)
      .runState(Context.empty)
      .runIO
      .runSequential,
    Duration.Inf)
}
