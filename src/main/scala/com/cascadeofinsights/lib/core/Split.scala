package com.cascadeofinsights.lib.core

import scala.util.matching.Regex

trait Split {
  def split(t : Text) : Seq[Text]
}

case class MinSplit(min : Int, on : Regex) extends Split{
  override def split(t: Text): Seq[Text] = {
    def loop(previous : String, remaining : String, minText : List[String] = List.empty): List[String] ={
      val rMatch: Option[Regex.Match] = on.findFirstMatchIn(remaining)
      rMatch.map { r =>
        if (previous.length + r.before.length < min){
          loop(previous + r.before + r.matched,r.after.toString,minText)
        } else {
         val m: String = previous + r.before
          loop("",r.after.toString,m :: minText)
        }
      }.getOrElse(previous + remaining :: minText)
    }
    loop("",t.text).map(Text.create(_)).reverse
  }
}
