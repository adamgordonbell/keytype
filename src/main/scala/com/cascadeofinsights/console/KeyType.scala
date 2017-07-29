package com.cascadeofinsights.console

import com.cascadeofinsights.lib.core.Key
import com.cascadeofinsights.lib.util.Data._
import com.cascadeofinsights.lib.util.IOEffect._
import com.cascadeofinsights.lib.util.Terminals._
import org.atnos.eff.future._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.future._
import org.atnos.eff.{ExecutorServices, _}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object KeyType extends App {
  implicit val scheduler = ExecutorServices.schedulerFromScheduledExecutorService(ExecutorServices.scheduledExecutor(10))

  def loop[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      _ <- Entry.start
      _ <- fromIO(writeText(0,32,s"press space (? exits)"))
      key <- fromIO(readKey)
      _ <- maybeExit(key)
    } yield ()
  }

  private def maybeExit[R: _config : _context : _future : _io](key : Key): Eff[R, Unit] = {
    for {
      _ <- key match {
        case Key('?',_) => exit
        case _ => loop
      }
    } yield ()
  }
  private def exit[R: _config : _context : _io] = Eff.pure[R, Unit](())

  Await.result(
    loop[Stack]
      .runReader(Config.empty)
      .runState(Context.empty)
      .runIO
      .runSequential,
    Duration.Inf)
}
