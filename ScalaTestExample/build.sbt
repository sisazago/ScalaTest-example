name := "mipago-functional-test"

scalaVersion := "2.11.7"

sbtVersion := "0.13.9.2"

scalacOptions ++= Opts.compile.encoding("UTF-8")

javacOptions ++= Seq("-encoding", "UTF-8")

testOptions in Test += Tests.Argument("-oD")

testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${t / "test-reports"}")
}

autoScalaLibrary := false

publishMavenStyle := true

externalPom()