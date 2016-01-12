package algorithms

import utils._

object Cycle {
  def DFS(g: Graph,color:Array[Int],u: Int):Boolean={
    color(u)=1;
    var result=false;
    
    for(i <-g.graph(u)){
      if(color(i.label)==1 && result==false)
        result=DFS(g,color,i.label);
      else if(color(i.label)==1)
        result=true;
    }
    result;
  }
  def isCyclic(g: Graph): Boolean = {
    var result=false;
    val Supp=new Support;
    var color: Array[Int] = new Array[Int](g.vertices.size);
    Supp.clearColor(color);
    for(j<-1 until g.vertices.size){
      if(g.directed)
        Supp.clearColor(color);
      if(DFS(g,color,j))
        result=true;
      
    }
    result;
  }
}