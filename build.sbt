import java.nio.file._

lazy val scalisp = crossProject
  .settings(
    name := "scalisp",

    scalaVersion := "2.12.1",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")
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

lazy val scalispJVM = scalisp.jvm

val embedBootCode = taskKey[Unit]("embedBootCode")
