package com.cascadeofinsights.console

import Output._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.future._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.future._
import org.atnos.eff.{ExecutorServices, _}
import com.cascadeofinsights.lib.util.Data._
import com.cascadeofinsights.lib.util.IOEffect._
import com.cascadeofinsights.lib.util.Terminals._
import com.cascadeofinsights.lib.util.TypingImp

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
      _ <- gameLoop // unless we are done
    } yield ()
  }

  def startGame[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      word <- fromFuture(TypingImp.nextText())
      _ <- put[R, Context](Context.create(word))
      result <- gameLoop
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
