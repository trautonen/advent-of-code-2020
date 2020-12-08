package aoc

import cats.effect.{Blocker, ContextShift, IO}

object Resource {

  def stream(resource: String)(implicit cs: ContextShift[IO]): fs2.Stream[IO, Byte] = {
    fs2.Stream.resource(Blocker[IO]).flatMap(blocker => {
      fs2.io.readInputStream(IO(getClass.getClassLoader.getResourceAsStream(resource)), 1024, blocker)
    })
  }

  def lines(resource: String, ignoreEmpty: Boolean = true)(implicit cs: ContextShift[IO]): fs2.Stream[IO, String] = {
    val base = stream(resource).through(fs2.text.utf8Decode).through(fs2.text.lines)
    if (ignoreEmpty) base.filter(_.nonEmpty)
    else base
  }
}
