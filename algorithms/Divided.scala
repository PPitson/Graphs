package algorithms
import utils._

object Divided {
  def DFSDIV(g:Graph,color:Array[Int],u:Int,col:Int):Boolean={
    var result=true;
    color(u)=col;
    for(i<-g.graph(u);if(result)){
     // println(u+" "+i.label+" "+color(u)+" "+color(i.label)+" "+result);
      if(color(i.label)==0) 
        result=DFSDIV(g,color,i.label,col%2+1);
      else if(color(i.label)==color(u))
        result=false;
      
    }
    result;
    
    
  }
  def isDivided(g: Graph): Boolean={
  //  println("start");
    var result=true;
    val Supp=new Support;
    var color: Array[Int] = new Array[Int](g.vertices.size)
    Supp.clearColor(color);
    for(i<-g.vertices){
      if(g.directed)
        Supp.clearColor(color);
      if(color(i.label)==0)
        if(!DFSDIV(g,color,i.label,1))
            result=false;
      
    }
    result;
  }
}