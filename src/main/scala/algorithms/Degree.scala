package algorithms

import utils._

object Degree {
  def degree(g: Graph): Int = {
    val vertexDegrees = new Array[Int](g.vertices.size)
    for (e <- g.edges) {
      vertexDegrees(e.start.label) += 1
      vertexDegrees(e.end.label) += 1
    }

    def max(ar: Array[Int], i: Int, maxSoFar: Int): Int = {
      if (i < ar.size) max(ar, i+1, math.max(ar(i), maxSoFar))
      else maxSoFar
    }

    max(vertexDegrees, 0, 0)
  }

}