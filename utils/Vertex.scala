package utils

import scala.collection.mutable.ArrayBuffer
import java.awt.Point

object Vertex{
  val size = 20
  var n = 0
  def inc: Int = {n += 1; n-1}
}

class Vertex(val point: Point){
  var label = Vertex.inc
  override def toString = point.x+" "+point.y+" "+label.toString()
}