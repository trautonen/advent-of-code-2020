package aoc.puzzles

import aoc.{AnswerApp, Resource}
import cats.effect.{ExitCode, IO}

import scala.util.matching.Regex

object Day02 extends AnswerApp {

  final val PasswordPattern: Regex = """(\d+)-(\d+) (\w): (\w+)""".r

  def validCounts(passwords: fs2.Stream[IO, String]): IO[Int] = {
    passwords
      .filter {
        case PasswordPattern(min, max, c, password) =>
          val matches = password.toCharArray.count(_ == c.charAt(0))
          min.toInt <= matches && max.toInt >= matches
      }
      .foldMap(_ => 1)
      .compile
      .lastOrError
  }

  def validPositions(passwords: fs2.Stream[IO, String]): IO[Int] = {
    passwords
      .filter {
        case PasswordPattern(i1, i2, c, password) =>
          val char = c.charAt(0)
          val chars = password.toCharArray
          val matches = Vector(chars(i1.toInt - 1) == char, chars(i2.toInt - 1) == char)
          matches.count(identity) == 1
      }
      .foldMap(_ => 1)
      .compile
      .lastOrError
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val passwords = Resource.lines("input_02.txt")
    for {
      answer1 <- validCounts(passwords)
      answer2 <- validPositions(passwords)
    } yield answers(answer1, answer2)
  }
}
