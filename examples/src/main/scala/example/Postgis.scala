package org.geoscript
package example

import com.vividsolutions.jts.{ geom => jts }

object PostgisTest extends GeoScript { 
  def main(args: Array[String]) {
    val conflict = workspace.Postgis("database" -> "conflict")
    val fields = conflict.layer("conflictsite").schema.fields
    
    for (field <- fields) { println(field.name) } 

    val workSpaceTest = workspace.Postgis() 
    
    val test = workSpaceTest.create("test",
        feature.Field("name", classOf[String]),
        feature.Field("geom", classOf[jts.Geometry], projection.Projection("EPSG:4326"))
    )

    test += feature.Feature( 
      "name" -> "test",
      "geom" -> geometry.Point(43,74)
    ) 
  }
} 
