package com.cascadeofinsights.lib.core

import java.time.{Duration, _}
import java.util.UUID

import aiyou.IO

import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.util.Random

sealed trait Input {
  def equals(c : Char): Boolean = {
    this match {
      case Character(char) => char == c
      case _ => false
    }
  }

  def char():Char = {
    this match {
      case Character(char) =>  char
      case _ => ' '
    }
  }
}
case object Escape extends Input
case object Backspace extends Input
case class Character(c : Char) extends Input

case class Key(input: Input, zonedDateTime: ZonedDateTime)

object Key {
  def createEscape(): Key = Key(Escape, ZonedDateTime.now())
  def create(c : Char): Key = Key(Character(c), ZonedDateTime.now())
  def createBackspace(): Key = Key(Backspace, ZonedDateTime.now())
}

case class TypedKey(input: Input, zonedDateTime: ZonedDateTime, correct : Boolean)

object TypedKey{
  def create(key : Key, expectedKey : Char) = TypedKey(key.input, key.zonedDateTime, key.input.equals(expectedKey))
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
      )
    }
  }

  override def storeResult(result: Result): IO[Unit] = IO.pure({})
}