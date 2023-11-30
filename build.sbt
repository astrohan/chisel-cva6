//-------------------------------------------------
// Global setting
//-------------------------------------------------
val scalaTestVersion = "3.2.0"
val chiselVersion = "3.6.0"
val chiselTestVersion = "0.6.0"

lazy val chiselSettings = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % chiselVersion),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
)
lazy val chiselTestSetting = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chiseltest" % chiselTestVersion % "test"))

def freshProject(name: String, dir: File): Project = {
  Project(id = name, base = dir / "src")
    .settings(
      Compile / scalaSource := baseDirectory.value / "main" / "scala",
      Compile / resourceDirectory := baseDirectory.value / "main" / "resources"
    )
}

lazy val commonSettings = Seq(
  scalaVersion := "2.13.10",
  scalacOptions ++= Seq("-deprecation", "-unchecked"),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-native" % "4.0.6"),
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % scalaTestVersion % "test"),
  exportJars := true,
)
//-------------------------------------------------
// ~Global setting
//-------------------------------------------------


//-------------------------------------------------
// my project
//-------------------------------------------------
lazy val roma = (project in file("generators/roma"))
  .settings(libraryDependencies)
  .settings(chiselSettings, commonSettings)

lazy val cva6 = (project in file("."))
  .dependsOn(roma)
  .settings(libraryDependencies)
  .settings(chiselSettings, commonSettings)
  .settings(chiselTestSetting)
//-------------------------------------------------
