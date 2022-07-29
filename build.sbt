ThisBuild / scalaVersion := "3.1.0"

libraryDependencies += "com.twelvemonkeys.imageio" % "imageio" % "3.8.0"
libraryDependencies += "com.twelvemonkeys.imageio" % "imageio-jpeg" % "3.8.0"
libraryDependencies += "com.twelvemonkeys.imageio" % "imageio-psd" % "3.8.0"

libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1"

mainClass in (Compile, run) := Some("a8.tools.GR0")

mainClass in (Compile, packageBin) := Some("a8.tools.GR0")
