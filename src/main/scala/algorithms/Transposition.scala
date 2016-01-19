package algorithms

import utils._
import scala.swing.FileChooser

object Transposition {

  def transposition(g: Graph): String = {
    if (g.vertices.size == 0) return ""
    if (!g.directed) return "returns same graph(because it's undirected)"
    val tg = new Graph(true, g.weighted)
    var x = 10
    var y = 10
    for (v <- g.vertices) tg.addVertex(v.point.x - Vertex.size/2, v.point.y - Vertex.size/2)
    for (e <- g.edges) tg.addEdge(e.end, e.start, e.weight)
    
    val chooser = new FileChooser
    chooser.peer.setDialogTitle("Save transposition of the graph")
    if (chooser.showSaveDialog(null) == FileChooser.Result.Approve) {
      val pw = new java.io.PrintWriter(chooser.selectedFile)
      GraphFile.saveToFile(tg, pw)
      pw.close()
    }
    return "saved to file " + chooser.selectedFile.toString()
  }
}