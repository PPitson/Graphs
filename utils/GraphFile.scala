package utils

object GraphFile {
  def saveToFile(g: Graph, pw: java.io.PrintWriter) {
    pw.println(g.directed.toString())
    pw.println(g.weighted.toString())
    pw.println(g.vertices.size)
    for (v <- g.vertices) pw.println(v)
    for (e <- g.edges) pw.println(e)
  }
  
  def parse(s: String): Graph = {
    val splitSource = s.split("\n")
    println(splitSource.length)
    for (i <- 0 until splitSource.size) splitSource(i) = splitSource(i).substring(0, splitSource(i).length-1)
    val g = new Graph(splitSource(0).toBoolean, splitSource(1).toBoolean)
    val endOfVertices = splitSource(2).toInt + 3
    for (i <- 3 until endOfVertices){
      val line = splitSource(i)
      val splitLine = line.split(" ")
      g.addVertex(splitLine(0).toInt, splitLine(1).toInt)
    }
    for (i <- endOfVertices until splitSource.size){
      val line = splitSource(i)
      val splitLine = line.split(" ")
      val vertex1 = g.vertices(splitLine(0).toInt)
      val vertex2 = g.vertices(splitLine(1).toInt)
      val weight = splitLine(2).toInt
      g.addEdge(vertex1, vertex2, weight)
    }
    g
  }
}