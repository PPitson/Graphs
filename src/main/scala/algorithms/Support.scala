package algorithms

class Support {

  def clearColor(color:Array[Int])={
    for(i <-0 until color.length)
      color(i)=0;
  }
}