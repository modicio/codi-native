name := "codi-native"
version := "0.2"
scalaVersion := "2.13.7"

val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe"  %% "circe-core"     % circeVersion,
  "io.circe"  %% "circe-generic"  % circeVersion,
  "io.circe"  %% "circe-parser"   % circeVersion
)