package algorithms

import utils._
import java.io.PrintWriter

object Transposition {
  def transposition(g: Graph): String = {
    if (!g.directed) return "returns same graph(because it's undirected)"
    val tg = new Graph(true, g.weighted)
    var x = 10
    var y = 10
    for (v <- g.vertices) tg.addVertex(v.point.x, v.point.y)
    for (e <- g.edges) tg.addEdge(e.end, e.start, e.weight)
    val pw = new java.io.PrintWriter("transposition.txt")
    GraphFile.saveToFile(tg, pw)
    pw.close()
    return "saved to file transposition.txt"
  }
}