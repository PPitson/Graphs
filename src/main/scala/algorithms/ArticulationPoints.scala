package algorithms
import utils._

object ArticulationPoints {

  var in=0;
  var root=0;
  var curroot=0;

  def DFSARTICULATE(g:Graph,low:Array[Int],time:Array[Int],color:Array[Int],ans:Array[Int],u:Int,res:Array[Int]):Unit={
    color(u)=1;
    in=in+1;
    time(u)=in;
    low(u)=in;
    for(i<-g.graph(u)){
      if(i.label!=ans(u)){
        if(color(i.label)==0){
          if(u==curroot)
            root+=1;
          ans(i.label)=u;
           DFSARTICULATE(g,low,time,color,ans,i.label,res);
          if(low(i.label)>=time(u) && u!=curroot)  
              res(u)=1;
            if(low(u)>low(i.label))
              low(u)=low(i.label);
        }
        if(color(i.label)==1){
          if(low(u)>time(i.label))
            low(u)=time(i.label)
        }
      }
    }
    color(u)=2;
  }

  def compute(g:Graph) ={
    var color: Array[Int] = new Array[Int](g.vertices.size);
    var low: Array[Int] = new Array[Int](g.vertices.size);
    var time: Array[Int] = new Array[Int](g.vertices.size);
    var ans: Array[Int] = new Array[Int](g.vertices.size);
    var res: Array[Int] = new Array[Int](g.vertices.size);
    var Supp=new Support;
    Supp.clearColor(color);
    Supp.clearColor(time);
    Supp.clearColor(low);
    Supp.clearColor(ans);
    Supp.clearColor(res);
    
    for(i<-0 until g.vertices.size){
      if(color(i)==0)
      {
        curroot=i;
        root=0;
        DFSARTICULATE(g,low,time,color,ans,i,res);
        if(root>1)
          res(curroot)=1;
      }
    }
    var L="\n";
    for(i<-0 until res.size){
      if(res(i)==1)
        L=L.concat(i.toString()+"\n");
    }
    L
  }
}