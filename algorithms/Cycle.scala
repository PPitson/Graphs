package algorithms

import utils._

object Cycle {
  def DFS(g: Graph,color:Array[Boolean],u: Int):Boolean={
    color(u)=true;
    var result=false;
    
    for(i <-g.graph(u)){
       println(u+" "+i.label+" "+color(i.label),result);
      if(color(i.label)==false && result==false)
        result=DFS(g,color,i.label);
      else(color(i.label)==true)
        result=true;
    }
    result;
  }
  def isCyclic(g: Graph): Boolean = {
    println("Start");
    var result=false;
    val Supp=new Support;
    var color: Array[Boolean] = new Array[Boolean](g.vertices.size)
    for(j<-1 until g.vertices.size){
      if(DFS(g,color,j))
        result=true;
      Supp.clearColor(color);
      
    }
    result;
  }
}