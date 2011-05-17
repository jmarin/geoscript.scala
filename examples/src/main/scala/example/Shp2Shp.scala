package org.geoscript
package example

import feature._, layer._, projection._

object Shp2Shp extends GeoScript {
  def main(args: Array[String]) {
    val List(sourcefile, destname, proj) = args.toList take 3
    val source = Shapefile(sourcefile)
    val destSchema = Schema(destname,
      source.schema.fields map {
        case (g: GeoField) => g.copyWith(projection = proj) // Field(g.name, g.binding, proj)
        case (f: Field) => f
      }
    )
    val dest = source.workspace.create(destSchema)
    dest ++= source.features map { f =>
      f.update(destSchema.geometry.name -> (f.geometry in proj))
    }
  }
}
