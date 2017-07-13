package adam

import util.IOEffect._
import util.Terminals._
import util.Data._
import aiyou._
import aiyou.implicits._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.future._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.future._
import org.atnos.eff.{ExecutorServices, _}
import util.Terminals

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.io.{Source, StdIn}
import scala.util.Random

object Hangman extends App {
  implicit val scheduler = ExecutorServices.schedulerFromScheduledExecutorService(ExecutorServices.scheduledExecutor(10))

  def randomNum(min: Int, max: Int): IO[Int] = IO.primitive(new Random().nextInt(max - min + 1) + min)

  def randomWord(len: Int): Future[String] = Future(List("hello", "world", "scala", "taiwan")(new Random().nextInt(4)))

  def randomWordEff[R: _config : _future : _io]: Eff[R, String] = {
    for {
      config <- ask[R, Config]
      Config(min, max, _) = config
      len <- fromIO(randomNum(min, max))
      word <- fromFuture(randomWord(len))
    } yield word.toUpperCase
  }

  def updateContext(key : Terminals.Key)(context : Context) : Context = {
    context.copy(guesses = key.char.toUpper :: context.guesses)
  }

  def outputScreen[R: _config : _context : _io]: Eff[R, Unit] = {
    for {
      _ <- fromIO(clearScreen)
      context <- get[R, Context]
      Context(word, guesses) = context
      _ <- fromIO(Output.outputImage(word, guesses))
      _ <- fromIO(Output.outputStatus(word, guesses))
      config <- ask[R, Config]
      _ <- if (config.cheat) fromIO(writeText(0, 20, word)) else Eff.pure[R, Unit](())
    } yield ()
  }

  def gameLoop[R: _config : _context : _io]: Eff[R, Unit] = {
    for {
      _ <- outputScreen
      key <- fromIO(readKey)
      _ <- modify(updateContext(key))
      context <- get[R, Context]
      _ <- Output.calculateResult(context.word, context.guesses) match {
        case Continue => gameLoop
        case YouWin | YouLose => Eff.pure[R, Unit](())
      }
    } yield ()
  }

  def startGame[R: _config : _context : _future : _io]: Eff[R, Unit] = {
    for {
      word <- randomWordEff
      _ <- put[R, Context](Context(word, List()))
      result <- gameLoop
      _ <- outputScreen
    } yield ()
  }

  Await.result(
    startGame[Stack]
      .runReader(Config(5, 10, false))
      .runState(Context("", List()))
      .runIO
      .runSequential,
    Duration.Inf)
}
