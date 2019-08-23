lazy val root = project
  .in(file("."))
  .settings(
    name := "loom-effekt",
    description := "Effect Handlers with Loom",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := dottyLatestNightlyBuild.get
  )
