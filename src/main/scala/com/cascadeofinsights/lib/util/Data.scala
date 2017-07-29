package com.cascadeofinsights.lib.util

import aiyou._
import cats.data._
import com.cascadeofinsights.lib.core.{Key, Result, Text, TypedKey}
import org.atnos.eff._

import scala.util.Try

object Data {

  case class Config()

  object Config {
    def empty() = Config()
  }

  case class Context(text : Text, keys : Seq[TypedKey])
  {
    //ToDo: make total
    def toResult() = Result.create(text.text,result(),keys.head.zonedDateTime,keys.last.zonedDateTime)

    def correctKeys(): Seq[TypedKey] = keys.filter(_.correct)

    def result(): String = correctKeys()
      .map(_.char)
      .toList.mkString("")

    val lastkey: Option[TypedKey] = keys.lastOption

    val complete = text.text == result()

    def expectedKey : Option[Char] = Try(text.text.charAt(correctKeys().length)).toOption

  }

  object Context{
    def update(key : Key)(context : Context) : Context = {
      context.copy(keys = context.keys :+ TypedKey.create(key, context.expectedKey.getOrElse(' ')))
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
