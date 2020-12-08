package aoc.puzzles

import aoc.{AnswerApp, Resource}
import cats.effect.{ExitCode, IO}

object Day01 extends AnswerApp {

  final val ExpectedSum = 2020

  def expenseProduct(expenses: fs2.Stream[IO, Int], entryCount: Int): IO[Int] = {
    combinations(expenses, entryCount)
      .filter(entries => entries.sum == ExpectedSum)
      .map(_.product)
      .compile
      .lastOrError
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val expenses = Resource.lines("input_01.txt").evalMap(v => IO(v.toInt))
    for {
      answer1 <- expenseProduct(expenses, 2)
      answer2 <- expenseProduct(expenses, 3)
    } yield answers(answer1, answer2)
  }
}
