package util

import aiyou._
import cats.data._
import org.atnos.eff._

object Data {

  case class Config(minWordLen: Int, maxWordLen: Int, cheat: Boolean)

  case class Context(word: String, guesses: List[Char])

  trait Result
  case object Continue extends Result
  case object YouWin extends Result
  case object YouLose extends Result

  type ConfigReader[A] = Reader[Config, A]

  type ContextState[A] = State[Context, A]

  type Stack = Fx.fx4[ConfigReader, ContextState, TimedFuture, IO]

  type _config[R] = ConfigReader |= R

  type _context[R] = ContextState |= R
}
