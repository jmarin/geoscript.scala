import net.sf.json._
import collection.JavaConversions._

import org.geoscript._
import GeoScript._
import geometry._
import projection._

object ReprojectJson {
  def fixupMap(map: JSONObject) {
    val fields = map.getJSONObject("fields")
    if ((fields has "center_lat") && (fields has "center_lon")) {
      val xy = Point(fields.getDouble("center_lon"), fields.getDouble("center_lat"))
        .in("EPSG:4326").transform("EPSG:3785")
      fields.remove("center_lat")
      fields.remove("center_lon")
      fields.put("center_x", xy.x)
      fields.put("center_y", xy.y)
      fields.put("projection", "EPSG:900913")
    }
  }

  def main(args: Array[String]) {
    val config = JSONSerializer.toJSON(io.Source.fromFile(args(0)).mkString)
      .asInstanceOf[JSONArray]

    config.foreach {
      case (m: JSONObject) if m.getString("model") == "maps.map" => fixupMap(m)
      case m => ()
    }

    val out = new java.io.FileWriter(args(0).replaceFirst("(\\.json)?$", ".fixed.json"))
    config.write(out)
    out.flush()
    out.close()
  }
}
