package com.cascadeofinsights.lib.util

import aiyou._
import cats.data._
import org.atnos.eff._

object Data {

  case class Config()

  case class Context2(text : Text, results : Seq[Result])
  case class Context(word: String, guesses: Seq[Char]){

    def calculateResult(): Result = ???

    def numMisses(): Int = ???

    def misses(): String = ???

  }
  object Context {

    def update(key : Terminals.Key)(context : Context) : Context = {
      context.copy(guesses = context.guesses :+ key.char)
    }
  }

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
