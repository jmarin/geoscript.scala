import sbt._, Keys._, Defaults.defaultSettings
import com.github.retronym.SbtOneJar

object GeoScript extends Build {
  lazy val gtVersion = 
    SettingKey[String]("gt-version", "Version number for GeoTools modules")

  
  def standardSettings = Seq (
    exportJars := true
  ) ++ Defaults.defaultSettings
 
  val meta =
    Seq[Setting[_]](
      organization := "org.geoscript",
      version := "0.7.3",
      gtVersion := "8-SNAPSHOT",
      scalaVersion := "2.9.1", 
      scalacOptions += "-deprecation"
    )

  val common = 
    Seq[Setting[_]](
      resolvers ++= Seq(
        "opengeo" at "http://repo.opengeo.org/",
        "osgeo" at "http://download.osgeo.org/webdav/geotools/"
      ),
      ivyXML <<= gtVersion ( v =>
        <dependencies>
          <exclude org="xml-apis" name="xml-apis-xerces"/>
          <exclude org="xml-apis" name="xml-apis"/>
        </dependencies>
      )
    ) ++ meta ++ defaultSettings ++ standardSettings ++ SbtOneJar.oneJarSettings

  lazy val root =
    Project("root", file(".")) aggregate(css, docs, examples, library)
  lazy val css = 
    Project("css", file("geocss"), settings = common)
  lazy val examples = 
    Project("examples", file("examples"), settings = common) dependsOn(library)
  lazy val library =
    Project("library", file("geoscript"), settings = standardSettings ++ SbtOneJar.oneJarSettings ++ common) dependsOn(css, dummy)
  lazy val dummy = 
    Project("dummy", file("dummy"), settings = meta ++ defaultSettings)
  lazy val docs = Project(
    "docs", file("docs"),
    settings = Seq(
      baseDirectory <<= thisProject(_.base),
      target <<= baseDirectory / "target",
      docDirectory <<= target / "doc",
      sphinxDir <<= docDirectory(_ / "sphinx"),
      sphinxSource <<= baseDirectory.identity,
      sphinxBuild := "sphinx-build",
      sphinxOpts := Nil,
      sphinx <<= (sphinxBuild, sphinxSource, sphinxDir, sphinxOpts) map (runSphinx),
      watchSources <<= (baseDirectory, target) map { (b, t) => (b ** "*") --- (t ** "*") get }
    ) ++ meta
  )

  lazy val sphinx = 
    TaskKey[java.io.File]("sphinx", "runs sphinx documentation generator")
  lazy val sphinxBuild = 
    SettingKey[String]("sphinx-build", "command to use when building sphinx")
  lazy val sphinxOpts =
    SettingKey[Seq[String]]("sphinx-opts", "options to pass to sphinx-build script")
  lazy val sphinxSource =
    SettingKey[java.io.File]("sphinx-source", "source directory for sphinx docs")
  lazy val sphinxDir =
    SettingKey[java.io.File]("sphinx-dir", "output directory for sphinx docs")

  def runSphinx(script: String, input: java.io.File, output: java.io.File, opts: Seq[String]) = {
    val cmd = Seq(script) ++ opts ++ Seq("-b", "html", "-d", 
      (output / "doctrees").getAbsolutePath,
      input.getAbsolutePath,
      (output / "html").getAbsolutePath)
    cmd ! ;
    output / "html"
  }
}
