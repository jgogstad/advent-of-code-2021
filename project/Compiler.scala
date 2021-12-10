package jgogstad.sbt

import sbt.{Def, Runtime, _}
import sbt.Keys._
import Versions.V
import _root_.io.github.davidgregory084.TpolecatPlugin

object Compiler extends AutoPlugin {
  override def trigger  = allRequirements
  override def requires = TpolecatPlugin

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    scalaVersion := V.build.Scala3Version,
    /*
     * Always fork JVMs launched from Compile configuration. This is typically processes launched with runMain. We always
     * want them to run in a separate JVM so that it shuts down properly.
     */
    Compile / fork := true,
    scalacOptions ++= Seq(
      "-source:future",
      "-Xtarget:11"
    )
  )

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    // run and runMain have broken resource loading
    // https://github.com/sbt/sbt/issues/3963
    runMain := Defaults.runMainTask(Runtime / fullClasspath, run / runner).evaluated,
    run     := Defaults.runTask(Runtime / fullClasspath, Compile / run / mainClass, run / runner).evaluated,
    scalacOptions --= Seq("-Xfatal-warnings")
  )
}
