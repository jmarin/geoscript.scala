package org.geoscript.geocss

import scala.collection.JavaConversions._
import java.io.StringReader

import org.geotools.{ styling => gt }
import gt.Style

object Benchmark {
  val Translator = new Translator()
  val tx = new org.geotools.styling.SLDTransformer()

  def main(args: Array[String]) {
    println("Warming up...")
    for (i <- 10 to 1 by -1) {
      val hardCss = makeInconvenientCSS(2)
      val worstCaseTime = time { 
        val stylesheet = CssParser.parse(new StringReader(hardCss)).get
        val style = Translator.css2sld(stylesheet)
        encodeSLD(style)
      }
      print("%d..." format i)
    }
    println(" ok!")

    // ok now test some stuff
    for (i <- 1 to 4) {
      val easyCss = makeConvenientCSS(i)
      val hardCss = makeInconvenientCSS(i)
      val bestCaseTime = { 
        val parseTime = 0 // time { CssParser.parse(new StringReader(easyCss)).get }
        val stylesheet = CssParser.parse(new StringReader(easyCss)).get
        val rewriteTime = time { Translator.css2sld(stylesheet) }
        val style = Translator.css2sld(stylesheet)
        val encodeTime = 0 // time { encodeSLD(style) }
        (parseTime, rewriteTime, encodeTime)
      }
      val worstCaseTime = { 
        val parseTime = 0 // time { CssParser.parse(new StringReader(hardCss)).get }
        val stylesheet = CssParser.parse(new StringReader(hardCss)).get
        val rewriteTime = time { Translator.css2sld(stylesheet) }
        val style = Translator.css2sld(stylesheet)
        val encodeTime = 0 // time { encodeSLD(style) }
        (parseTime, rewriteTime, encodeTime)
      }

      println("Easy: %s; Hard: %s" format (bestCaseTime, worstCaseTime))
    }
  }

  def makeConvenientCSS(n: Int): String =
    (for (i <- 1 to n)
      yield "[PROP = %d] { fill: black; }" format i
    ) mkString("\n")

  def makeInconvenientCSS(n: Int): String =
    (for (i <- 1 to n)
      yield "[PROP_%d = 0] { fill: black; }" format i
    ) mkString("\n")

  def encodeSLD(sld: gt.Style): String = {
    val out = new java.io.StringWriter()
    tx.transform(sld, out)
    out.toString()
  }

  def time(op: => Unit): Double = {
    val startTime = System.nanoTime()
    var i = 1000
    while ({i -= 1; i != 0}) {
      op
    }
    (System.nanoTime() - startTime) / 1e9d
  }
}
