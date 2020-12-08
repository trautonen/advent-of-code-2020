package aoc.puzzles

import aoc.{AnswerApp, Resource}
import cats.effect.{ExitCode, IO}

object Day03 extends AnswerApp {

  def collectTrees(slopes: fs2.Stream[IO, Array[Char]], right: Int, down: Int): IO[Long] = {
    slopes
      .zipWithIndex
      .flatMap {
        case (slope, index) =>
          if (index > 0 && index % down == 0) fs2.Stream(slope)
          else fs2.Stream.empty
      }
      .zipWithIndex
      .filter {
        case (slope, index) =>
          val pos = ((index + 1) * right) % slope.length
          slope(pos.toInt) == '#'
      }
      .foldMap(_ => 1)
      .map(_.toLong)
      .compile
      .lastOrError
  }

  def runsProduct(runs: fs2.Stream[IO, (Int, Int)], slopes: fs2.Stream[IO, Array[Char]]): IO[Long] = {
    runs
      .evalMap {
        case (right, down) => collectTrees(slopes, right, down)
      }
      .reduce(_ * _)
      .compile
      .lastOrError
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val slopes = Resource.lines("input_03.txt").map(_.toCharArray)
    for {
      answer1 <- collectTrees(slopes, 3, 1)
      answer2 <- runsProduct(fs2.Stream((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)), slopes)
    } yield answers(answer1, answer2)
  }
}
