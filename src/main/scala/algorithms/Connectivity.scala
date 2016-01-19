package algorithms

import utils._
import java.util.Stack

object Connectivity {

  def isConnected(g: Graph) : Boolean = {
    if (g.vertices.size == 0) return false
    val visited = new Array[Boolean](g.vertices.size)
    val st = new Stack[Int]()
    var visitedCount = 0
    st.push(0)
    visited(0) = true
    while (!st.isEmpty()){
      val label = st.pop()
      visitedCount += 1
      for (e <- g.edges){
        if (e.start.label == label && !visited(e.end.label)){
            visited(e.end.label) = true
            st.push(e.end.label)
        }
        else if (e.end.label == label && !visited(e.start.label)){
            visited(e.start.label) = true
            st.push(e.start.label)
        }
      }
    }
    visitedCount == g.vertices.size
  }
}
