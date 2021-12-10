import jgogstad.sbt.Versions.V

ThisBuild / organization := "jgogstad"

lazy val `advent` = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"           %% "cats-effect"              % V.fp.CatsEffect,
      "co.fs2"                  %% "fs2-core"                 % V.fp.Fs2Core,
      "com.github.valskalla"    %% "odin-core"                % V.logging.Odin,
      "co.fs2"                  %% "fs2-io"                   % V.fp.Fs2Core,
      "org.typelevel"           %% "cats-core"                % V.fp.Cats,
      "org.typelevel"           %% "squants"                  % V.types.Squants,
      ("org.scala-lang.modules" %% "scala-collection-contrib" % V.data.CollectionContrib)
        .cross(CrossVersion.for3Use2_13),
      "org.typelevel" %% "spire"        % V.math.Spire,
      "org.jgrapht"    % "jgrapht-core" % V.math.Jgrapht,
      "org.typelevel" %% "cats-parse"   % V.parsing.CatsParse,
      "org.scalanlp"  %% "breeze"       % V.math.Breeze exclude ("org.typelevel", "spire_2.13"),
      "org.scodec"    %% "scodec-core"  % V.data.Scodec
    )
  )
