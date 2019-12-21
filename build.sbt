name := "kamon-open-tracing"

organization := "org.slasch"

version := "1.3"

scalaVersion := "2.12.9"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Xfuture"
)

libraryDependencies += "io.kamon" %% "kamon-core" % "2.0.0" % Provided

publishMavenStyle := true

bintrayOrganization := None

bintrayRepository   := "maven"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

bintrayReleaseOnPublish in ThisBuild := false
