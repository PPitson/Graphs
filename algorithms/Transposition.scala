package algorithms

import utils._
import java.io.PrintWriter
import main.gui.saveGraph
import scala.swing.FileChooser

object Transposition {
  def transposition(g: Graph): String = {
    if (!g.directed) return "returns same graph(because it's undirected)"
    val tg = new Graph(true, g.weighted)
    var x = 10
    var y = 10
    for (v <- g.vertices) tg.addVertex(v.point.x, v.point.y)
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