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
      _ <- context.calculateResult() match {
        case Continue => gameLoop
        case YouWin | YouLose => Eff.pure[R, Unit](())
      }
    } yield ()
  }

  def startGame[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      word <- Input.randomWord
      _ <- put[R, Context](Context(word, Set.empty))
      result <- gameLoop
      _ <- Output.outputScreen
    } yield ()
  }

  Await.result(
    startGame[Stack]
      .runReader(Config(5, 10, false))
      .runState(Context("", Set.empty))
      .runIO
      .runSequential,
    Duration.Inf)
}
