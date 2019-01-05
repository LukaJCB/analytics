
val analyticsVersion = "0.1.0"

val mesosVersion = "0.28.2"
val hadoopVersion = "3.1.0"
val circeVersion = "0.10.1"
val fs2Version = "1.0.0"

val pathToMesosLibs = "/usr/local/lib"

lazy val root = project.in(file("."))
    .settings(commonSettings)

lazy val commonSettings = Defaults.coreDefaultSettings ++
  basicSettings

lazy val basicSettings = Seq(
  name := "analytics",
  version := analyticsVersion,
  scalaVersion := "2.12.8",

  resolvers += "Typesafe Repository" at
    "http://repo.typesafe.com/typesafe/releases/",

  libraryDependencies ++= Seq(
    "org.apache.mesos" % "mesos" % mesosVersion,
    "org.apache.hadoop" % "hadoop-client" % hadoopVersion,
    "org.apache.hadoop" % "hadoop-hdfs" % hadoopVersion,
    "co.fs2" %% "fs2-core" % fs2Version,
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion
  ),

  scalacOptions := Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-Ypartial-unification",
    "-Ywarn-unused:implicits",
    "-Ywarn-unused:imports",
    "-Ywarn-unused:locals",
    "-Ywarn-unused:params",
    "-Ywarn-value-discard"
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6"),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.2"),
  javaOptions += "-Djava.library.path=%s:%s".format(
    sys.props("java.library.path"),
    pathToMesosLibs
  ),

  connectInput in run := true,

  fork in run := true,

  fork in Test := true
)