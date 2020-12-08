package aoc

import cats.effect.{ExitCode, IOApp}

trait AnswerApp extends IOApp with StreamUtils {

  def answers(answer1: Any, answer2: Any): ExitCode = {
    println("Answer 1: " + answer1.toString)
    println("Answer 2: " + answer2.toString)
    ExitCode.Success
  }

}
