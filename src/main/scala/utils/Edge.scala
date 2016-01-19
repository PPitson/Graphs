package utils


class Edge(val start: Vertex, val end: Vertex, val weight:Int = 0){
  override def toString = start.label+" "+end.label+" "+weight
}