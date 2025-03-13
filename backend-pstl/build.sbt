ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version      := "1.0-SNAPSHOT"

// Définition du module jschemadatagenerator avec ses dépendances
lazy val jschemadatagenerator = (project in file("modules/jschemadatagenerator"))
  .settings(
    name := "jschemadatagenerator",
    scalaVersion := "2.13.16",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core"    % "0.14.1",
      "io.circe" %% "circe-generic" % "0.14.1",
      "io.circe" %% "circe-parser"  % "0.14.1",
      "dk.brics" % "automaton"       % "1.12-4",
      "au.com.bytecode" % "opencsv"   % "2.4",
      "org.graalvm.sdk" % "graal-sdk" % "22.3.0",
      "io.github.serpro69" % "kotlin-faker" % "1.14.0",
      "com.networknt" % "json-schema-validator" % "1.0.87",
      "org.apache.commons" % "commons-text" % "1.10.0",
      "org.slf4j" % "slf4j-simple" % "1.5.6" % Test
    )
  )

// Définition du projet Play principal (backendPSTL) qui dépend de jschemadatagenerator
lazy val backendPSTL = (project in file("."))
  .enablePlugins(PlayScala)
  .dependsOn(jschemadatagenerator)
  .settings(
    name := "Backend-PSTL",
    scalaVersion := "2.13.16",
    version := "1.0-SNAPSHOT",
    // Pour démarrer Play, on utilise le point d'entrée du serveur Play
    mainClass := Some("play.core.server.ProdServerStart"),
    libraryDependencies ++= Seq(
      guice,
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % Test
    )
  )

dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-databind" % "2.14.2"
