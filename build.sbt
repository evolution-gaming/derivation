import sbt.librarymanagement.For3Use2_13

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
  homepage                := Some(url("https://github.com/evolution-gaming/derivation")),
  developers              := List(
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
  publishMavenStyle       := true,
  Test / publishArtifact  := false,
  publishTo               := Some {
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
  versionScheme           := Some("early-semver"),
  git.baseVersion         := "0.1",
  git.formattedShaVersion :=
      git.gitHeadCommit.value map { sha => s"${git.baseVersion.value}-$sha-SNAPSHOT" },
)

enablePlugins(GitVersioning)

val defaultSettings = testDependencies ++ scala3Settings ++ publishSettings

val modules = file("modules")

lazy val derivation = project
    .in(modules / "core")
    .settings(
      name := "derivation-core",
    )
    .settings(defaultSettings)

lazy val tests = project
    .in(modules / "tests")
    .settings(
      publish / skip                    := true,
      defaultSettings,
      libraryDependencies += "io.circe" %% "circe-parser" % Version.circe % Test,
    )
    .dependsOn(derivation, circe, tapir, cats, playJson)

lazy val circe = project
    .in(modules / "circe")
    .settings(
      name                              := "derivation-circe",
      libraryDependencies += "io.circe" %% "circe-core" % Version.circe,
      defaultSettings,
    )
    .dependsOn(derivation)

// circe codecs which for compatibility uses circe-core_2.13 artifact
lazy val `circe-compat_213` = project
    .in(modules / "circe")
    .settings(
      name                              := "derivation-circe-compat213",
      target                            := (file("modules") / "circe" / "target-compat213" / "jvm").getAbsoluteFile,
      libraryDependencies += "io.circe" %% "circe-core" % Version.circe cross For3Use2_13(),
      defaultSettings,
    )
    .dependsOn(derivation)

lazy val tapir = project
    .in(modules / "tapir")
    .settings(
      name                                                 := "derivation-tapir",
      libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % Version.tapir,
      defaultSettings,
    )
    .dependsOn(derivation)

lazy val cats = project
    .in(modules / "cats")
    .settings(
      name                                   := "derivation-cats",
      libraryDependencies += "org.typelevel" %% "cats-kernel" % Version.cats,
      defaultSettings,
    )
    .dependsOn(derivation)

lazy val playJson = project
    .in(modules / "playJson")
    .settings(
      name                                       := "derivation-play-json",
      libraryDependencies += "com.typesafe.play" %% "play-json" % Version.playJson cross CrossVersion.for3Use2_13,
      defaultSettings,
    )
    .dependsOn(derivation)
