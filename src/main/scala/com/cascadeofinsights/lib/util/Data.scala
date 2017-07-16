package com.cascadeofinsights.lib.util

import aiyou._
import cats.data._
import org.atnos.eff._

object Data {

  case class Config(minWordLen: Int, maxWordLen: Int, cheat: Boolean)

  case class Context(word: String, guesses: Set[Char]){

    def calculateResult(): Result = {
      if (word.toSet == guesses.intersect(word.toSet)) {
        YouWin
      } else if (numMisses >= 6) {
        YouLose
      } else {
        Continue
      }
    }

    def numMisses(): Int = guesses.filterNot(c => word.contains(c.toString)).size

    def misses(): String =
      guesses.filterNot(c => word.contains(c.toString)).seq.toList.reverse.mkString(" ")

  }
  object Context {

    def update(key : Terminals.Key)(context : Context) : Context = {
      context.copy(guesses = context.guesses + key.char.toUpper)
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
