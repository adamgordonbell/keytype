package com.cascadeofinsights.lib.core

import aiyou.IO

import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.util.Random

trait Typing[Text,Result] {
 def nextText() : Text
 def storeResults(results : Seq[Result]) : IO[Unit]
 def getResults() : IO[Seq[Result]]
}


class TypingImp(file : String, splitter : Split = SentinalSplit()) extends Typing[Text,Result] {
  lazy val lines: List[String] = {
    Source.fromFile(file).getLines.toList
  }
  val randomLines : List[Text] = {
    Random.shuffle(lines).map(Text.create(_)).flatMap(splitter.split(_).toList)
  }

  var i = 0
  override def nextText(): Text ={
    val r = randomLines(i)
    i += 1
    r
  }

  var results: List[Result] = List.empty

  override def storeResults(result: Seq[Result]): IO[Unit] = IO.pure{
    results = result.toList ::: results
  }

  override def getResults(): IO[Seq[Result]] = IO.pure(results)
}