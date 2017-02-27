import java.nio.file._

name := "scalisp"
assemblyJarName in assembly := "scalisp.jar"

scalaVersion := "2.12.1"
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.2"

val embedBootCode = taskKey[Unit]("embedBootCode")

compile in Compile := (compile in Compile dependsOn embedBootCode).value

embedBootCode := {
  Files.createDirectories(Paths.get("src/main/resources"))
  Files.copy(
    Paths.get("lispboot/boot.lisp"),
    Paths.get("src/main/resources/boot.lisp"),
    StandardCopyOption.REPLACE_EXISTING)
}
