name         := "julius"
organization := "nl.gn0s1s"
startYear    := Some(2016)
homepage     := Some(url("https://github.com/philippus/julius"))
licenses += ("MPL-2.0", url("https://www.mozilla.org/MPL/2.0/"))

developers := List(
  Developer(
    id = "philippus",
    name = "Philippus Baalman",
    email = "",
    url = url("https://github.com/philippus")
  )
)

ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / versionPolicyIntention := Compatibility.None

Compile / packageBin / packageOptions += Package.ManifestAttributes("Automatic-Module-Name" -> "nl.gn0s1s.julius")

scalaVersion := "2.13.13"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.18.0" % Test
)
