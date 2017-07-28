package com.cascadeofinsights.lib.util

import java.time.{Duration, _}
import java.util.UUID

import aiyou.IO

import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.util.Random

case class Key(char: Char, zonedDateTime: ZonedDateTime)

object Key {
  def create(char : Char): Key = Key(char, ZonedDateTime.now())
}

case class TypedKey(char: Char, zonedDateTime: ZonedDateTime, correct : Boolean)

object TypedKey{
  def create(key : Key, expectedKey : Char) = TypedKey(key.char, key.zonedDateTime, key.char == expectedKey)
}

trait Typing[Text,Result] {
 def nextText() : IO[Text]
 def storeResult(result : Result) : IO[Unit]
}

case class Result(id : UUID, expected : String, result : String, duration : Duration, date : ZonedDateTime){
  def wpm : Double = (result.length.toDouble / duration.toMillis) * 60 * 1000 / 5
}

object Result{
  def create(expected : String, result : String, duration : Duration, date : ZonedDateTime) = {
    Result(id = UUID.randomUUID(), expected, result, duration, date)
  }
  def create(expected : String, result : String, start : ZonedDateTime , end : ZonedDateTime): Result ={
    val duration : Duration = Duration.between(start, end)
    Result(id = UUID.randomUUID(), expected , result,duration, start)
  }
}

case class Text(id : UUID = UUID.randomUUID(), text : String)

object Text {
  def create(text : String) = {
    Text(id = UUID.randomUUID(),text)
  }
}

object TypingImp extends Typing[Text,Result] {
  lazy val lines = {
    val file = "paragrams.txt"
    IO.primitive(Source.fromFile(file).getLines.toList)
  }

  override def nextText(): IO[Text] ={
    lines.map{ls =>
      Text.create(
        Random.shuffle(ls)
        .head
        .filterNot(_ == '.')
         + "."
      )
    }
  }

  override def storeResult(result: Result): IO[Unit] = IO.pure({})
}