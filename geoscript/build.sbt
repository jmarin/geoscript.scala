name := "geoscript"

seq(com.github.retronym.SbtOneJar.oneJarSettings:_*)

libraryDependencies <+= scalaVersion { v =>
  "org.scala-lang" % "scala-swing" % v 
}

libraryDependencies <++= gtVersion { v => 
  Seq(
    "org.geotools" % "gt-main" % v,
    "org.geotools" % "gt-epsg-hsql" % v,
    "org.geotools" % "gt-shapefile" % v,
    "org.geotools" % "gt-render" % v,
    "org.geotools" % "gt-xml" % v,
    "org.geotools" % "gt-geojson" % v,
    "org.geotools.jdbc" % "gt-jdbc-postgis" % v,
    "org.geotools.jdbc" % "gt-jdbc-spatialite" % v
  )
}

libraryDependencies ++= 
  Seq(
    "javax.media" % "jai_core" % "1.1.3",
    "org.scala-tools.testing" %% "specs" % "[1.6,1.7)" % "test",
    "org.scala-tools.testing" %% "specs" % "[1.6,1.7)" % "test",
    "com.lowagie" % "itext" % "2.1.5"
  )

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"
