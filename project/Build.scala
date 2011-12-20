import sbt._
import Keys._

object GDataScalaClient extends Build {

  lazy val gdataScalaClientProject =  Project("gdata-scala-client", file("."), settings = Defaults.defaultSettings ++ Seq(
      name := "gdata-scala-client",
      version := "0.3",
      organization := "com.google",
      crossScalaVersions := Seq("2.8.0", "2.8.1", "2.8.2", "2.9.0", "2.9.0-1", "2.9.1"),
      scalaSource in Compile <<= baseDirectory(_ / "src"),
      scalaSource in Test <<= baseDirectory(_ / "tests"),
      resourceDirectory in Test <<= baseDirectory(_ / "test-data"),
      libraryDependencies := Seq (
        "com.novocode" % "junit-interface" % "0.6" % "test",
        "emma" % "emma" % "2.1.5320" % "test"
      )
    )
  ) dependsOn(uri("git://github.com/mjanson/xml-test#0.3"))

}
