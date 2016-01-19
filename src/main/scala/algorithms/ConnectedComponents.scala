package algorithms

import utils._
import java.util.Stack

object ConnectedComponents {

  def compute(g: Graph): String = {
    val visited = new Array[Int](g.vertices.size)
    val st = new Stack[Int]()
    var componentNumber = 0
    for (i <- 0 until g.vertices.size){
      if (visited(i) == 0) {
        componentNumber += 1
        st.push(i)
        visited(i) = componentNumber
        while (!st.isEmpty()) {
          val label = st.pop()
          for (e <- g.edges){
            if (e.start.label == label && visited(e.end.label) == 0){
              visited(e.end.label) = componentNumber
              st.push(e.end.label)
            }
            else if (e.end.label == label && visited(e.start.label) == 0){
              visited(e.start.label) = componentNumber
              st.push(e.start.label)
            }
          }
        }
      }
    }
    var result = "\n"
    for (i <- 1 to componentNumber){
      result += "#" + i + ": "
      for (j <- 0 until g.vertices.size) if(visited(j) == i) result += j + " "
      result += "\n"
    }
    result
  }

  def DFSstack(label: Int, visited: Array[Boolean], st: Stack[Int], g: Graph): Unit = {
    visited(label) = true
    for (e <- g.edges) if (e.start.label == label && !visited(e.end.label)) DFSstack(e.end.label, visited, st, g)
    st.push(label)
  }

  var result = "\n"

  def DFSconcatenate(label: Int, visited: Array[Boolean], g: Graph): Unit = {
    visited(label) = true
    result += label + " "
    for (e <- g.edges) if (e.end.label == label && !visited(e.start.label)) DFSconcatenate(e.start.label, visited, g)
  }

  def computeStrong(g: Graph): String = {
    var visited = new Array[Boolean](g.vertices.size)
    val st = new Stack[Int]()
    for (i <- 0 until g.vertices.size) if (!visited(i)) DFSstack(i,visited,st,g)
    visited = new Array[Boolean](g.vertices.size)
    var componentNumber = 0
    while (!st.isEmpty()){
      val label = st.pop()
      if (!visited(label)){
        componentNumber += 1
        result += "#" + componentNumber + ": "
        DFSconcatenate(label,visited,g)
        result += "\n"
      }
    }
    result
  }
}
