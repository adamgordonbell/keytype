package com.cascadeofinsights.lib.util

import aiyou._
import cats.data._
import com.cascadeofinsights.lib.core._
import org.atnos.eff._

import scala.util.Try

object Data {

  case class Config(file : String){
    val typing = new TypingImp(file, MinSplit(50,"\\s".r))
  }


  case class Context(text : Text, keys : Option[(ScreenDrawEvent,Seq[TypedKey])])
  {
    def toResult(): Seq[Result] = {
      keys.map { k =>
        val first = k
//        val l = NonEmptyList[Event](k._1,k._2.map(_.)toList)
        Result.create(
          text.text,
          result(),
          k._1.zonedDateTime,
          k._2.last.zonedDateTime)
        }.toList
    }

    def correctKeys(): Seq[TypedKey] = keys.map(_._2.filter(_.correct)).getOrElse(Seq.empty)

    def result(): String = correctKeys().collect{case TypedKey(Character(c), _,_) => c}.toList.mkString("")

    val lastkey: Option[TypedKey] = keys.flatMap(_._2.lastOption)

    val complete = text.text == result()

    def expectedKey : Option[Char] = Try(text.text.charAt(correctKeys().length)).toOption

  }

  object Context{
    def update(key : KeyPressEvent)(context : Context) : Context = {
      val (a,ks) = context.keys.get //ToDo make total
      context.copy(keys = Some((a,ks :+ TypedKey.create(key, context.expectedKey.getOrElse(' ')))))
    }

    def update(draw : ScreenDrawEvent)(context : Context) : Context = {
      context.copy(keys = Some((draw,Seq.empty)))
    }

    def empty() : Context = {
      Context(Text.create(""), None)
    }

    def create(text : Text) : Context = {
      Context(text, None)
    }
  }

  type ConfigReader[A] = Reader[Config, A]

  type ContextState[A] = State[Context, A]

  type Stack = Fx.fx4[ConfigReader, ContextState, TimedFuture, IO]

  type _config[R] = ConfigReader |= R

  type _context[R] = ContextState |= R

//  type _stack[R] = ContextState |= ConfigReader |= TimedFuture |= IO |= R
}
