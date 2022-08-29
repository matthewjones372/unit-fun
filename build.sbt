ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "practice"
  )

testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))

val zioV = "2.0.1"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio-test"          % zioV,
  "dev.zio" %% "zio-test-sbt"      % zioV % Test,
  "dev.zio" %% "zio-test-magnolia" % zioV % Test
)
