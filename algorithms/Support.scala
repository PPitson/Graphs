package algorithms

class Support {
  def clearColor(color:Array[Boolean])={
    for(i <-0 until color.length)
      color(i)=false;
  }
}