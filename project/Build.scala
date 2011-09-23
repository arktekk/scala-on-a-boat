import sbt._
import Keys._

object Settings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "no.arktekk.scalaboat",
    version := "1.0",
    scalaVersion := "2.9.1"
  )
}


object ScalaOnABoat extends Build {

  import Settings._

  val description = SettingKey[String]("description")

  val testDependencies = Seq(
    "org.scalatest" %% "scalatest" % "1.6.1" % "test",
    "junit" % "junit" % "4.9" % "test"
  )

  lazy val formhandlingProject = Project(
    id = "formhandling",
    base = file("formhandling"),
    settings = buildSettings ++ Seq(
      description := "Scala Web Form Framework",
      libraryDependencies ++= testDependencies
    )
  )
}

