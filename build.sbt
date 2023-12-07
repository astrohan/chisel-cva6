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
  unmanagedBase := (cva6Root / unmanagedBase).value,
  allDependencies := {
    // drop specific maven dependencies in subprojects
    val dropDeps = Seq(("edu.berkeley.cs", "rocketchip"))
    allDependencies.value.filterNot { dep =>
      dropDeps.contains((dep.organization, dep.name))
    }
  },
  exportJars := true,
)
//-------------------------------------------------
// ~Global setting
//-------------------------------------------------


//-------------------------------------------------
// Rocket-chip dependencies (subsumes making RC a RootProject)
//-------------------------------------------------
val rocketChipDir = file("generators/rocket-chip")

lazy val cde = (project in file("tools/cde"))
  .settings(commonSettings)
  .settings(Compile / scalaSource := baseDirectory.value / "cde/src/chipsalliance/rocketchip")
  .settings(publishArtifact := false)

lazy val hardfloat  = (project in rocketChipDir / "hardfloat")
  .settings(commonSettings, chiselSettings)
  .settings(publishArtifact := false)

lazy val rocketMacros  = (project in rocketChipDir / "macros")
  .settings(commonSettings, chiselSettings)
  .settings(publishArtifact := false)

lazy val rocketchip = freshProject("rocketchip", rocketChipDir)
  .dependsOn(hardfloat, rocketMacros, cde)
  .settings(commonSettings, chiselSettings)

lazy val rocketLibDeps = (rocketchip / Keys.libraryDependencies)
//-------------------------------------------------
// ~Rocket-chip dependencies (subsumes making RC a RootProject)
//-------------------------------------------------

//-------------------------------------------------
// my project
//-------------------------------------------------
lazy val cva6Root = Project("cva6Root", file("."))

lazy val roma = (project in file("generators/roma"))
  .dependsOn(rocketchip)
  .settings(libraryDependencies ++= rocketLibDeps.value)
  .settings(chiselSettings, commonSettings)
  .settings(chiselTestSetting)

lazy val cva6 = (project in file("cva6"))
  .dependsOn(rocketchip, roma)
  .settings(libraryDependencies ++= rocketLibDeps.value)
  .settings(chiselSettings, commonSettings)
  .settings(chiselTestSetting)
//-------------------------------------------------
