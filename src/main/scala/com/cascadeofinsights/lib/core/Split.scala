package com.cascadeofinsights.lib.core

import scala.util.matching.Regex

trait Split {
  def split(t : Text) : Seq[Text]
}

case class SentinelSplit() extends Split{
  override def split(t: Text): Seq[Text] = Seq(t)
}

case class CompositeSplit(splits : List[Split]) extends Split {
  override def split(t: Text) = {
     splits.foldLeft(List(t).toSeq){ (list, split) =>
       list.flatMap(split.split(_))
     }
  }
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

case class MaxSplit(max : Int, on : Regex = "\\s".r) extends Split {
  override def split(t: Text): Seq[Text] = {
    def loop(previous : String, remaining : String, minText : List[String] = List.empty): List[String] ={
      if(remaining.length < max){
        remaining :: minText
      } else {
        on.findAllMatchIn(remaining)
          .toList
          .filter(_.before.length() <= max)
          .lastOption.map { m =>
          val chunk = m.before.toString
          val lessChunk = m.after.toString
          loop("",lessChunk,chunk :: minText)
        }.getOrElse {
          val chunk = remaining.substring(0,max)
          val lessChunk = remaining.substring(max)
          loop("",lessChunk,chunk :: minText)
        }
      }
    }
    loop("",t.text).map(Text.create(_)).reverse
  }
}
