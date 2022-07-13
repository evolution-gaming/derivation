publish / skip := true

ThisBuild / scalaVersion := Version.scala

ThisBuild / version := "0.1.0-SNAPSHOT"

testFrameworks += new TestFramework("munit.Framework")

val scala3Settings = scalacOptions ++= Vector(
  "-Yexplicit-nulls",
  "-encoding",
  "utf-8",
)

val testDependencies = libraryDependencies ++= Vector(
  "org.scalameta" %% "munit" % Version.munit % Test,
)

val defaultSettings = testDependencies ++ scala3Settings

val modules = file("modules")

lazy val derivation = project
    .in(modules / "derivation")
    .settings(
      name := "derivation-core",
    )
    .settings(defaultSettings)

lazy val circeDerivation = project
    .in(modules / "circeDerivation")
    .settings(
      name                              := "derivation-circe",
      libraryDependencies += "io.circe" %% "circe-core"   % Version.circe,
      libraryDependencies += "io.circe" %% "circe-parser" % Version.circe % Test,
      defaultSettings,
    )
    .dependsOn(derivation)

lazy val tapirDerivation = project
    .in(modules / "tapirDerivation")
    .settings(
      name                                                 := "derivation-tapir",
      libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % Version.tapir,
      defaultSettings,
    )
    .dependsOn(derivation)

lazy val playJsonDerivation = project
    .in(modules / "playJsonDerivation")
    .settings(
      name                                       := "derivation-play-json",
      libraryDependencies += "com.typesafe.play" %% "play-json" % Version.playJson cross CrossVersion.for3Use2_13,
      defaultSettings,
    )
    .dependsOn(derivation)
