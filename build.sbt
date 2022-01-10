import java.nio.file.StandardCopyOption
import java.nio.file.Files

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.1.0"

lazy val core = project
  .settings(
    name := "scalisp-core",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.typelevel" %% "cats-parse" % "0.3.6"
    ),
    Compile / resourceGenerators += Def.task {
      val s = streams.value
      val resourceDir = resourceManaged.value
      val baseDir = baseDirectory.value
      Files.createDirectories(resourceDir.asPath)
      val src = baseDir / ".." / "rosetta-lisp" / "boot.lisp"
      val dest = resourceDir / "boot.lisp"
      Files.copy(src.asPath, dest.asPath, StandardCopyOption.REPLACE_EXISTING)
      Seq(dest.asFile)
    }.taskValue
  )

lazy val cli = project
  .aggregate(core)
  .dependsOn(core)
  .settings(
    name := "scalisp",
    assembly / assemblyJarName := "scalisp.jar",
    assembly / mainClass := Some("scalisp.cli.Main")
  )
