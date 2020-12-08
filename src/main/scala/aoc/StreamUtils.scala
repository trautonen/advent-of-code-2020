package aoc

import cats.effect.IO

trait StreamUtils {

  def combinations[A](stream: fs2.Stream[IO, A], n: Int): fs2.Stream[IO, List[A]] = {
    def loop(ss: fs2.Stream[IO, List[A]], nn: Int): fs2.Stream[IO, List[A]] = {
      if (nn < n) {
        ss.zipWithIndex.flatMap {
          case (combined, index) => loop(ss.drop(1 + index), nn + 1).map(rest => combined ++ rest)
        }
      } else {
        ss
      }
    }
    loop(stream.map(v => List(v)), 1)
  }

}
