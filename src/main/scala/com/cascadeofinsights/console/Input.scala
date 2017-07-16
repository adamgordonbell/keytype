package com.cascadeofinsights.console

import aiyou._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.future._
import com.cascadeofinsights.lib.util.Data._
import com.cascadeofinsights.lib.util.IOEffect._
import com.cascadeofinsights.lib.util.TypingImp

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object Input  {

  def randomWord[R: _config : _future]: Eff[R, String] = {
    for {
      config <- ask[R, Config]
      text <- fromFuture(textFromConfig(config))
    } yield text
  }


  private def textFromConfig(config : Config): Future[String] = {
    Future(TypingImp.nextText().text)
  }
}
