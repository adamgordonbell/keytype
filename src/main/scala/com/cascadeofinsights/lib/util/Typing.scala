package com.cascadeofinsights.lib.util

import java.time.{Duration, _}
import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class Key(char: Char, zonedDateTime: ZonedDateTime)

object Key {
  def create(char : Char): Key = Key(char, ZonedDateTime.now())
}

case class TypedKey(char: Char, zonedDateTime: ZonedDateTime, correct : Boolean)

object TypedKey{
  def create(key : Key, expectedKey : Char) = TypedKey(key.char, key.zonedDateTime, key.char == expectedKey)
}

trait Typing[Text,Result] {
 def nextText() : Future[Text]
 def storeResult(result : Result) : Future[Unit]
}

case class Result(id : UUID, expected : String, result : String, duration : Duration, date : ZonedDateTime){
  def wpm() : Double = 50
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
  override def nextText(): Future[Text] = Future(Text.create("The red fox jumped over the brown dog"))

  override def storeResult(result: Result): Future[Unit] = Future({})
}