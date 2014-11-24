
name          := "Fluid Canvas"

version       := "0.0.1"

scalacOptions := Seq("-unchecked", 
                     "-deprecation", 
                     "-encoding", "utf8", 
                     "-feature", 
                     "-language:implicitConversions")

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.3"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"