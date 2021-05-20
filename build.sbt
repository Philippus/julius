name := "julius"
organization := "nl.gn0s1s"
startYear := Some(2016)
homepage := Some(url("https://github.com/philippus/julius"))
licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

developers := List(
  Developer(
    id = "philippus",
    name = "Philippus Baalman",
    email = "",
    url = url("https://github.com/philippus")
  )
)

scalaVersion := "2.13.5"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
)
