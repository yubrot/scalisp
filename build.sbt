import java.nio.file._
import sbtcrossproject.{crossProject, CrossType}

lazy val scalisp =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Full)
    .settings(
      name := "scalisp",

      scalaVersion := "2.11.12",
      scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint"),

      libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0",

      publishArtifact := false,
    )
    .jvmSettings(
      assemblyJarName in assembly := "scalisp.jar",

      compile in Compile := (compile in Compile dependsOn embedBootCode).value,

      embedBootCode := {
        Files.createDirectories(Paths.get("scalisp/jvm/src/main/resources"))
        Files.copy(
          Paths.get("lispboot/boot.lisp"),
          Paths.get("scalisp/jvm/src/main/resources/boot.lisp"),
          StandardCopyOption.REPLACE_EXISTING)
      },
    )
    .jsSettings(
      artifactPath in (Compile, fastOptJS) := file("gh-pages/scalisp.js"),
      artifactPath in (Compile, fullOptJS) := file("gh-pages/scalisp.js"),
    )
    .nativeSettings(
      nativeMode := "release",
    )

lazy val scalispJVM = scalisp.jvm
lazy val scalispJS = scalisp.js
lazy val scalispNative = scalisp.native

val embedBootCode = taskKey[Unit]("embedBootCode")
