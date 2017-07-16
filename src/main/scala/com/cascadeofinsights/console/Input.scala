package com.cascadeofinsights.console

import aiyou._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.future._
import com.cascadeofinsights.lib.util.Data._
import com.cascadeofinsights.lib.util.IOEffect._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object Input  {

  def randomWord[R: _config : _future : _io]: Eff[R, String] = {
    for {
      config <- ask[R, Config]
      Config(min, max, _) = config
      len <- fromIO(randomNum(min, max))
      word <- fromFuture(randomWord(len))
    } yield word.toUpperCase
  }

  private def randomNum(min: Int, max: Int): IO[Int] = {
    IO.primitive(new Random().nextInt(max - min + 1) + min)
  }

  private def randomWord(len: Int): Future[String] = {
    Future(List("hello", "world", "scala", "taiwan")(new Random().nextInt(4)))
  }
}
