package jgogstad.sbt

object Versions {
  val V = this

  object build {
    val Scala213Version = "2.13.7"
    val MonadicFor      = "0.3.1"
    val KindProjector   = "0.13.2"
  }

  object data {
    val Scodec = "1.11.9"
  }

  object fp {
    val Cats       = "2.7.0"
    val CatsEffect = "3.3.0"
    val Fs2Core    = "3.2.2"
  }

  object logging {
    val Odin  = "0.13.0"
    val Slf4J = "1.7.32"
  }

  object math {
    val Spire   = "0.17.0"
    val Jgrapht = "1.5.1"
    val Breeze  = "2.0"
  }

  object types {
    val ScalaCollectionContrib = "0.2.2"
    val Squants                = "1.8.3"
    val Newtype                = "0.4.4"
  }

  object parsing {
    val Fastparse = "2.3.3"
  }

}
