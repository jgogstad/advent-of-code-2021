package jgogstad.utils

import cats.ApplicativeThrow
import cats.instances.string._
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.{Files, Path}
import squants.information.{Information, Mebibytes}

import java.io.FileNotFoundException
import java.nio.file.Paths

object FileIo {

  /**
   * Reads contents of file relative to classpath root.
   *
   * Inspect the `resourceDirectories` sbt key for sources,
   * e.g. `sbt 'show mymodule / Test / resourceDirectories'`
   */
  def contentsOf[F[_]: ApplicativeThrow: Files](
    file: String,
    chunkSize: Information = Mebibytes(2)
  ): fs2.Stream[F, String] = {
    val resolveFile = ApplicativeThrow[F].catchNonFatal(getClass.getClassLoader.getResource(file)).map(Option.apply)
    Stream.eval(resolveFile).flatMap {
      case Some(url) =>
        Files[F]
          .readAll(Path.fromNioPath(Paths.get(url.toURI)))
          .through(fs2.text.utf8.decode)
          .through(fs2.text.lines)
      case None => fs2.Stream.raiseError(new FileNotFoundException(file))
    }
  }
}
