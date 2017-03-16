import java.nio.file._

lazy val scalisp = crossProject
  .settings(
    name := "scalisp",

    scalaVersion := "2.12.1",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint"),

    publishArtifact := false
  )
  .jvmSettings(
    assemblyJarName in assembly := "scalisp.jar",

    libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.2",

    compile in Compile := (compile in Compile dependsOn embedBootCode).value,

    embedBootCode := {
      Files.createDirectories(Paths.get("scalisp/jvm/src/main/resources"))
      Files.copy(
        Paths.get("lispboot/boot.lisp"),
        Paths.get("scalisp/jvm/src/main/resources/boot.lisp"),
        StandardCopyOption.REPLACE_EXISTING)
    }
  )
  .jsSettings(
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "0.4.2",

    artifactPath in (Compile, fastOptJS) := file("gh-pages/scalisp.js"),
    artifactPath in (Compile, fullOptJS) := file("gh-pages/scalisp.js")
  )

lazy val scalispJVM = scalisp.jvm
lazy val scalispJS = scalisp.js

val embedBootCode = taskKey[Unit]("embedBootCode")
