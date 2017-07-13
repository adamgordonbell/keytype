package adam

import aiyou._
import aiyou.implicits._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.future._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.future._
import org.atnos.eff.{ExecutorServices, _}
import util.Data._
import util.IOEffect._
import util.Terminals
import util.Terminals._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.{Source, StdIn}
import scala.util.Random

object Output {

  def makes(word: String, guesses: List[Char]): String =
    word.flatMap(c => if (guesses.contains(c)) s"$c " else "  ")

  def numMisses(word: String, guesses: List[Char]): Int =
    guesses.filterNot(c => word.contains(c.toString)).size

  def misses(word: String, guesses: List[Char]): String =
    guesses.filterNot(c => word.contains(c.toString)).reverse.mkString(" ")

  def calculateResult(word: String, guesses: List[Char]): Result = {
    if (word.toSet == guesses.toSet.intersect(word.toSet)) {
      YouWin
    } else if (numMisses(word, guesses) >= 6) {
      YouLose
    } else {
      Continue
    }
  }

  def readLine: IO[String] = IO.primitive(StdIn.readLine)

  def readFile(file: String): IO[List[String]] = IO.primitive(Source.fromFile(file).getLines.toList)

  def outputFile(col: Int, row: Int, file: String): IO[Unit] = {
    for {
      ls <- readFile(file)
      _ <- ls.zipWithIndex.map({ case (l, i) => writeText(col, row + i, l) }).sequence
    } yield ()
  }

  def outputImage(word: String, guesses: List[Char]): IO[Unit] =
    outputFile(0, 8, s"${numMisses(word, guesses)}-miss.txt")

  def outputStatus(word: String, guesses: List[Char]): IO[Unit] = {
    for {
      _ <- writeText(10, 9, makes(word, guesses))
      _ <- writeText(10, 10, List.fill(word.size)('-').mkString(" "))
      _ <- writeText(10, 12, s"Misses: ${misses(word, guesses)}")
      _ <- calculateResult(word, guesses) match {
        case Continue => IO.pure(())
        case YouWin => writeText(10, 14, "You win!!\n")
        case YouLose => writeText(10, 14, s"The word is $word.  You Lose.\n")
      }
    } yield ()
  }
}
