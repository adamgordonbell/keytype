package com.cascadeofinsights.lib.util

import aiyou._
import cats.data._
import org.atnos.eff._

object Data {

  case class Config()

  object Config {
    def empty() = Config()
  }

  case class Context(text : Text, keys : Seq[Key])

  object Context{
    def update(key : Key)(context : Context) : Context = {
      context.copy(keys = context.keys :+ key)
    }
    def empty() : Context = {
      Context(Text.create(""), Seq.empty)
    }
    def create(text : Text) : Context = {
      Context(text, Seq.empty)
    }
  }

  trait Status
  case object Exit extends Status
  case object InProgress extends Status
  case object Done extends Status

  type ConfigReader[A] = Reader[Config, A]

  type ContextState[A] = State[Context, A]

  type Stack = Fx.fx4[ConfigReader, ContextState, TimedFuture, IO]

  type _config[R] = ConfigReader |= R

  type _context[R] = ContextState |= R
}
