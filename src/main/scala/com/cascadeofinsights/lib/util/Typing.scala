package com.cascadeofinsights.lib.util

import java.time.Duration
import java.time._
import java.util.UUID

import aiyou._
import cats.data._
import org.atnos.eff._


trait Typing[Text,Result] {
 def nextText() : Text
 def storeResult(result : Result) : Unit
}

case class Result(id : UUID, expected : String, result : String, duration : Duration, date : ZonedDateTime){
  def wpm() : Double = 50
}

object Result{
  def create(expected : String, result : String, duration : Duration, date : ZonedDateTime) = {
    Result(id = UUID.randomUUID(), expected, result, duration, date)
  }
}

case class Text(id : UUID = UUID.randomUUID(), text : String)

object Text {
  def create(text : String) = {
    Text(id = UUID.randomUUID(),text)
  }
}
object TypingImp extends Typing[Text,Result] {
  override def nextText(): Text = Text.create("The red fox jumped over the brown dog")

  override def storeResult(result: Result): Unit = {}
}