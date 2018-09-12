lazy val root = project
  .in(file("."))
  .settings(
    name := "cats-reflect",
    description := "Monadic Reflection for Cats",
    libraryDependencies ++= Seq(
      ("org.typelevel" % "cats-effect_2.12" % "1.0.0").withDottyCompat(scalaVersion.value),
      ("org.typelevel" % "cats-core_2.12" % "1.3.1").withDottyCompat(scalaVersion.value)
    ),
    version := "0.1.0-SNAPSHOT",
    scalaVersion := dottyLatestNightlyBuild.get
  )
