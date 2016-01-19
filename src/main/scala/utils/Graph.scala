package utils

import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import java.awt.Point

class Graph(var directed: Boolean, var weighted: Boolean, val vertices: ListBuffer[Vertex] = new ListBuffer[Vertex](),
            val edges: ListBuffer[Edge] = new ListBuffer[Edge](),
            val graph: ArrayBuffer[ListBuffer[Vertex]] = new ArrayBuffer[ListBuffer[Vertex]]()){
  
  def convert = {
    val result = new Array[List[Vertex]](graph.size)
    for (i <- 0 until graph.size) result(i) = graph(i).toList
    result
  }
  
  def moveLabels(removedLabel: Int) {
    Vertex.n -= 1
    for (v <- vertices){
      if (v.label > removedLabel) v.label -= 1
    }
  }
  def whichVertex(ex:Int, ey:Int, i: Int): Vertex = {
    if (i < vertices.size){
      val pt = vertices(i).point
      if (ex-pt.x>=0 && ex-pt.x<=Vertex.size && ey-pt.y>=0 && ey-pt.y<=Vertex.size) vertices(i)
      else whichVertex(ex, ey, i+1)
    }
    else null
  }
  
  def whichEdge(v1: Vertex, v2: Vertex, i: Int): Edge = {
    if (i < edges.size){
      val e = edges(i)
      if ((e.start == v1 && e.end == v2) || (!directed && e.start == v2 && e.end == v1)) e
      else whichEdge(v1, v2, i+1)
    }
    else null
  }
  
  def addVertex(ex: Int, ey:Int) {
    vertices += new Vertex(new Point(ex-Vertex.size/2, ey-Vertex.size/2))
    graph += ListBuffer[Vertex]()
  }
  
  def addEdge(v1: Vertex, v2: Vertex, weight: Int) {
    val newEdge = new Edge(v1, v2, weight)
		edges += newEdge
		graph(v1.label) += v2
		if (!directed) graph(v2.label) += v1
  }
  
  def removeVertex(v: Vertex) {
    vertices -= v
  	graph -= graph(v.label)
  	moveLabels(v.label)
  	for (e <- edges) if (e.start == v || e.end == v) edges -= e
  }
  
  def removeEdge(e: Edge, v1: Vertex, v2: Vertex) {
    edges -= e
		graph(v1.label) -= v2
		if (!directed) graph(v2.label) -= v1
  }
  
}