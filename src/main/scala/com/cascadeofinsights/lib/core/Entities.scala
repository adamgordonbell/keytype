package com.cascadeofinsights.lib.core

import java.time.{Duration, _}
import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

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

