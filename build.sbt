name := "julius"
organization := "nl.gn0s1s"
version := "1.0.2"
startYear := Some(2016)
homepage := Some(url("https://github.com/philippus/julius"))
licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

scalaVersion := "2.13.5"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.15.3" % Test
)

pomExtra :=
  <scm>
    <url>git@github.com:Philippus/julius.git</url>
    <connection>scm:git@github.com:Philippus/julius.git</connection>
  </scm>
  <developers>
    <developer>
      <id>philippus</id>
      <name>Philippus Baalman</name>
      <url>https://github.com/philippus</url>
    </developer>
  </developers>
