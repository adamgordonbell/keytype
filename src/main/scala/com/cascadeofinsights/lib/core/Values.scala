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

case class Key(input: Input, zonedDateTime: ZonedDateTime)

object Key {
  def createEscape(): Key = Key(Escape, ZonedDateTime.now())
  def create(c : Char): Key = Key(Character(c), ZonedDateTime.now())
  def createBackspace(): Key = Key(Backspace, ZonedDateTime.now())
  def createEnter(): Key = Key(Enter, ZonedDateTime.now())
}

case class TypedKey(input: Input, zonedDateTime: ZonedDateTime, correct : Boolean)

object TypedKey{
  def create(key : Key, expectedKey : Char) = TypedKey(key.input, key.zonedDateTime, key.input.equals(expectedKey))
}

