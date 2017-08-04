package com.cascadeofinsights.lib.core

import java.time._

import scala.concurrent.ExecutionContext.Implicits.global

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
      case Enter =>  '\n'
      case _ => ' '
    }
  }
}
case object Escape extends Input
case object Enter extends Input
case object Backspace extends Input
case class Character(c : Char) extends Input

sealed trait Event
case class ScreenDrawEvent(text : String, zonedDateTime: ZonedDateTime = ZonedDateTime.now)
case class KeyPressEvent(input: Input, zonedDateTime: ZonedDateTime)

object KeyPressEvent {
  def createEscape(): KeyPressEvent = KeyPressEvent(Escape, ZonedDateTime.now())
  def create(c : Char): KeyPressEvent = KeyPressEvent(Character(c), ZonedDateTime.now())
  def createBackspace(): KeyPressEvent = KeyPressEvent(Backspace, ZonedDateTime.now())
  def createEnter(): KeyPressEvent = KeyPressEvent(Enter, ZonedDateTime.now())
}

case class TypedKey(input: Input, zonedDateTime: ZonedDateTime, correct : Boolean)

object TypedKey{
  def create(key : KeyPressEvent, expectedKey : Char) = TypedKey(key.input, key.zonedDateTime, key.input.equals(expectedKey))
}

