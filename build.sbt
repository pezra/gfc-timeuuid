gilt.GiltProject.jarSettings

name := "commons-timeuuid"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.1.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
  "com.datastax.cassandra" % "cassandra-driver-core" % "2.0.1" % "test",
  "com.netflix.astyanax" % "astyanax" % "1.56.48" % "test"
)
