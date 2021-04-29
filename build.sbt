lazy val root = project
  .in(file("."))
  .settings(
    name := "cats-reflect",
    description := "Monadic Reflection for Cats",
    libraryDependencies ++= Seq(
      ("org.typelevel" % "cats-effect" % "3.1.0").cross(CrossVersion.for3Use2_13),
      ("org.typelevel" % "cats-core" % "2.3.0").cross(CrossVersion.for3Use2_13)
    ),
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.0.0-RC3"
  )
