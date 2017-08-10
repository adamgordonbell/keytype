package com.cascadeofinsights.lib.core

import aiyou.IO

import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.util.Random

trait Typing[Text,Result] {
 def nextText() : IO[Text]
 def storeResults(results : Seq[Result]) : IO[Unit]
 def getResults() : IO[Seq[Result]]
}


class TypingImp(file : String) extends Typing[Text,Result] {
  lazy val lines = {
    IO.primitive(Source.fromFile(file).getLines.toList)
  }

  override def nextText(): IO[Text] ={
    lines.map{ ls =>
      Text.create(
        Random.shuffle(ls)
        .head
      )
    }
  }

  var results: List[Result] = List.empty

  override def storeResults(result: Seq[Result]): IO[Unit] = IO.pure{
    results = result.toList ::: results
  }

  override def getResults(): IO[Seq[Result]] = IO.pure(results)
}