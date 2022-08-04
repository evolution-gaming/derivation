name           := "derivation"
publish / skip := true

ThisBuild / scalaVersion := Version.scala

ThisBuild / organization := "com.evolution"

testFrameworks += new TestFramework("munit.Framework")

val scala3Settings = scalacOptions ++= Vector(
  "-Yexplicit-nulls",
  "-encoding",
  "utf-8",
)

val testDependencies = libraryDependencies ++= Vector(
  "org.scalameta" %% "munit" % Version.munit % Test,
)

lazy val publishUserName = sys.env.getOrElse("ARTIFACTORY_PUBLISH_USERNAME", "")
lazy val publishPass     = sys.env.getOrElse("ARTIFACTORY_PUBLISH_PASS", "")

lazy val releasesRepo = MavenRepository(
  s"artifactory-evolution-maven-local-releases",
  s"https://evolution.jfrog.io/artifactory/maven-local-releases",
)

lazy val snapshotsRepo = MavenRepository(
  s"artifactory-evolution-maven-local-snapshots",
  s"https://evolution.jfrog.io/artifactory/maven-local-snapshots",
)

lazy val publishSettings = Vector(
  homepage               := Some(url("https://github.com/evolution-gaming/derivation")),
  developers             := List(
    Developer(
      "Odomontois",
      "Oleg Nizhnik",
      "onizhnikov@evolution.com",
      url("https://github.com/Odomontois"),
    ),
    Developer(
      "FunFunFine",
      "Anton Voitsishevskii",
      "avoitsishevskii@evolution.com",
      url("https://github.com/FunFunFine"),
    ),
  ),
  publishMavenStyle      := true,
  Test / publishArtifact := false,
  publishTo              := Some {
      if (isSnapshot.value) snapshotsRepo else releasesRepo
  },
  credentials += {
      if (publishUserName.nonEmpty)
          Credentials(
            realm = "Artifactory Realm",
            host = "evolution.jfrog.io",
            userName = publishUserName,
            passwd = publishPass,
          )
      else
          Credentials(Path.userHome / ".sbt" / "evo.credentials")
  },
)

enablePlugins(GitVersioning)

val defaultSettings = testDependencies ++ scala3Settings ++ publishSettings

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
