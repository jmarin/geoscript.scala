package org.geoscript.style.io

import org.geoscript.style.Style
import org.geoscript.io.{ Format, Reader, Sink, Source, Writer }
import org.geotools.{ styling => gt }
import org.geotools.factory.CommonFactoryFinder.getStyleFactory

object SLD extends Format[Style] {
  def read(source: Source): Style = 
    source { in =>
      val chars = new java.io.InputStreamReader(in)
      val res = new gt.SLDParser(getStyleFactory(null), chars).readXML()(0)
      chars.close()
      new org.geoscript.style.WrappedSLD(res)
    }

  def write[T](style: Style, sink: Sink[T]): T =
    sink { out =>
      val tx = new org.geotools.styling.SLDTransformer
      tx.setIndentation(4)
      tx.transform(style.underlying, out)
    }
}
