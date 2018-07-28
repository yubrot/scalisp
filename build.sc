import mill._, scalalib._, scalajslib._, scalanativelib._
import ammonite.ops._

object scalisp extends Module {
  def coreSources = T.sources { pwd / "scalisp-core" }
  def ioSources = T.sources { pwd / "scalisp-io" }
  def jvmSources = T.sources { pwd / "scalisp-jvm" }
  def nativeSources = T.sources { pwd / "scalisp-native" }
  def jsSources = T.sources { pwd / "scalisp-js" }
  def lispbootSources = T.sources { pwd / "lispboot" / "boot.lisp" }

  trait Base extends ScalaModule {
    def scalaOptions = Seq("-deprecation", "-feature", "-unchecked", "-Xlint")
    def scalaVersion = "2.11.12"
    def ivyDeps = Agg(ivy"com.lihaoyi::fastparse::1.0.0")
  }

  object jvm extends Base {
    def mainClass = Some("scalisp.Main")
    def sources = T.sources { coreSources() ++ ioSources() ++ jvmSources() }
    def resources = T.sources { bootCodes() }

    def bootCodes = T {
      val resources = pwd / "resources"
      mkdir(resources)
      for (src <- lispbootSources()) cp.over(src.path, resources / src.path.last)
      Seq(PathRef(resources))
    }
  }

  object native extends Base with ScalaNativeModule {
    def scalaNativeVersion = "0.3.8"
    def mainClass = Some("scalisp.Main")
    def releaseMode = ReleaseMode.Release
    def sources = T.sources { coreSources() ++ ioSources() ++ nativeSources() }
  }

  object js extends Base with ScalaJSModule {
    def scalaJSVersion = "0.6.24"
    def sources = T.sources { coreSources() ++ jsSources() }

    def fastOpt = T { copyToGhPages(super.fastOpt()) }
    def fullOpt = T { copyToGhPages(super.fullOpt()) }

    def copyToGhPages(p: PathRef): PathRef = {
      val dest = pwd / "gh-pages" / "scalisp.js"
      cp.over(p.path, dest)
      PathRef(dest)
    }
  }
}
