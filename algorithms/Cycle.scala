package algorithms

import utils._

object Cycle {
  def DFS(g: Graph,color:Array[Boolean],u: Int):Boolean={
    color(u)=true;
    for(i <-g.graph(u)){
      if(color(i.label)==false)
        DFS(g,color,i.label);
      if(color(i.label)==true)
        true;
    }
    false;
  }
  def isCyclic(g: Graph): Boolean = {
    val Supp=new Support;
    var color: Array[Boolean] = new Array[Boolean](g.vertices.size)
    for(j<-1 until g.vertices.size){
      if(DFS(g,color,j))
        true;
      Supp.clearColor(color);
    }
    false;
  }
}