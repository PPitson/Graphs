

package algorithms
import utils._;

object FlloydWarshall {

  def calculate(g:Graph)= {
    var s = g.vertices.size;
    var d = Array.ofDim[Int](g.vertices.size, g.vertices.size);
    for (i <- 0 until s)
      for (j <- 0 until s) {
        d(i)(j) = 100000000;
        if (i == j)
          d(i)(j) = 0;
      }
    for (e <- g.edges) {
      d(e.start.label)(e.end.label) = e.weight;
      if (g.weighted == false)
        d(e.start.label)(e.end.label) += 1;
      if (g.directed == false) {
        d(e.end.label)(e.start.label) = e.weight;
        if (g.weighted == false)
          d(e.end.label)(e.start.label) += 1;
      }
    }
    for (t <- 0 until s)
      for (i <- 0 until s)
        for (j <- 0 until s)
          if (d(t)(j) > d(t)(i) + d(i)(j)) d(t)(j) = d(t)(i) + d(i)(j);

    var result = "";
    for (t <- 0 until s) {
      for (i <- 0 until s)
        result = result.concat(t + " - " + i + " => " + (if (d(t)(i) == 100000000) "INF" else d(t)(i)) + "\n");
      result = result + sys.props("line.separator");
    }
    result;
  }
}