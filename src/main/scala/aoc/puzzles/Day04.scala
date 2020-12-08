package aoc.puzzles

import aoc.{AnswerApp, Resource}
import cats.effect.{ExitCode, IO}
import fs2.Chunk

import scala.util.matching.Regex

object Day04 extends AnswerApp {

  final val KeyValuePattern: Regex = """(\w+):(#?\w+)""".r
  final val RequiredKeys: List[String] = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  final val Validators: Map[String, String => Boolean] = Map(
    "byr" -> (v => v.length == 4 && between(v, 1920, 2002)),
    "iyr" -> (v => v.length == 4 && between(v, 2010, 2020)),
    "eyr" -> (v => v.length == 4 && between(v, 2020, 2030)),
    "hgt" -> (v => (v.endsWith("cm") && between(v.dropRight(2), 150, 193)) ||
                   (v.endsWith("in") && between(v.dropRight(2), 59, 76))),
    "hcl" -> (v => v.matches("""^#[0-9a-f]{6}$""")),
    "ecl" -> (v => Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(v)),
    "pid" -> (v => v.matches("""^[0-9]{9}$"""))
  )

  def between(number: String, min: Int, max: Int): Boolean = {
    val numeric = number.toInt
    numeric >= min && numeric <= max
  }

  def validFields(passports: fs2.Stream[IO, Map[String, String]]): IO[Int] = {
    passports
      .filter(passport => RequiredKeys.forall(passport.contains))
      .foldMap(_ => 1)
      .compile
      .lastOrError
  }

  def validValues(passports: fs2.Stream[IO, Map[String, String]]): IO[Int] = {
    passports
      .filter(passport => Validators.forall {
        case (key, validate) => passport.get(key).exists(validate)
      })
      .foldMap(_ => 1)
      .compile
      .lastOrError
  }

  def toPassport(lines: Chunk[String]): Map[String, String] = {
    lines
      .iterator
      .filter(_.nonEmpty)
      .flatMap(line => KeyValuePattern.findAllMatchIn(line).map(m => m.group(1) -> m.group(2)))
      .toMap
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val passports = Resource.lines("input_04.txt", ignoreEmpty = false).split(_.isEmpty).map(toPassport)
    for {
      answer1 <- validFields(passports)
      answer2 <- validValues(passports)
    } yield answers(answer1, answer2)
  }
}
