package main

import scala.swing._
import scala.swing.event._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import java.awt.{Graphics2D, Color, geom, Point, BasicStroke}
import java.awt.event._

import utils.Vertex
import utils.Edge
import utils.Graph
import utils.GraphFile._

import algorithms._


object gui extends SimpleSwingApplication{
  var movingPoint: Point = null
  var lineStart : Point = null
  var dragging = false
  var chosenVertices = true
  var firstClick = true
  var vertex1: Vertex = null
  var vertex2: Vertex = null
  val directedBox = new CheckBox("directed")
  val weightedBox = new CheckBox("weighted")
  
  var g = new Graph(false, false)
  
  def clearEverything {
    g.vertices.clear()
    g.edges.clear()
    g.graph.clear()
    Vertex.n = 0
    directedBox.enabled = true
  	weightedBox.enabled = true
  	directedBox.selected = false
  	weightedBox.selected = false
  	resultsArea.text = ""
    drawingPanel.repaint()
  }
  
  def askForWeight: Int = {
    var weight = 0
    if (weightedBox.selected){
      def isAllDigits(x: String) = x forall Character.isDigit
      val input = Dialog.showInput(null, "Weight: ", "Type in weight", Dialog.Message.Plain, Swing.EmptyIcon, Nil, "0")
		  if (input != None && input.get.length > 0 && isAllDigits(input.get)) weight = input.get.toInt
		}
    weight
  }
  
  def enableCheckBoxes {
    if (g.edges.size == 0){
  		directedBox.enabled = true
  		weightedBox.enabled = true
  	}
  }
  
  def drawFromFile(s: String) {
    clearEverything
    g = parse(s)
    directedBox.selected = g.directed
    weightedBox.selected = g.weighted
    directedBox.enabled = false
  	weightedBox.enabled = false
  	resultsArea.text = ""
    drawingPanel.repaint()
  }

  
  def openGraph {
    val chooser = new FileChooser
    if (chooser.showOpenDialog(null) == FileChooser.Result.Approve) {
      val source = scala.io.Source.fromFile(chooser.selectedFile)
      drawFromFile(source.mkString)
      source.close()
    }
  }
  
  def saveGraph {
    val chooser = new FileChooser
    if (chooser.showOpenDialog(null) == FileChooser.Result.Approve) {
      val pw = new java.io.PrintWriter(chooser.selectedFile)
      saveToFile(g, pw)
      pw.close()
    }
  }
  
  val drawingPanel = new Panel {
     background = Color.WHITE
     listenTo(this, mouse.clicks, mouse.moves)
     reactions += {
          
       case e: MouseDragged => 
         if (movingPoint != null) {
           movingPoint.x=e.point.x
           movingPoint.y=e.point.y
           repaint()
         }
                 
       case e: MousePressed => 
         val ex=e.point.x
  		   val ey=e.point.y
  					  
  		   if (e.peer.getButton == MouseEvent.BUTTON1){
  			   if (chosenVertices){
             val v = g.whichVertex(ex, ey, 0)
               if (v != null){
                 movingPoint = v.point
      				   dragging = true
               }
               else g.addVertex(ex, ey)
  			   }
  			   else{
  				   lineStart = new Point(ex, ey)
  				   movingPoint = new Point(ex, ey)
  				   dragging = true
  				 }
  		   }
  			 else if (e.peer.getButton == MouseEvent.BUTTON3){
  					if(chosenVertices){
    				  val v = g.whichVertex(ex, ey, 0)
    				  if (v != null) g.removeVertex(v)
  					}
  					else{
  					  if(firstClick){
  					    vertex1 = g.whichVertex(ex, ey, 0)
  					    firstClick = false
  					  }
  					  else{
  					    vertex2 = g.whichVertex(ex, ey, 0)
  					    firstClick = true
  					    if (vertex1 != null && vertex2 != null){
  					      val edge = g.whichEdge(vertex1, vertex2, 0)
  					      if (edge != null){
  					        g.removeEdge(edge, vertex1, vertex2) 
  					        enableCheckBoxes
  					      }
  					    }
  					  }
  					 }
  			   }
           repaint()
       // end of case of mouse pressed
            
       case e: MouseReleased => 
         if (lineStart != null){
           vertex1 = g.whichVertex(lineStart.x, lineStart.y, 0)
           vertex2 = g.whichVertex(movingPoint.x, movingPoint.y, 0)
           if (vertex1 !=null && vertex2 != null){
             if (g.whichEdge(vertex1, vertex2, 0) != null) Dialog.showMessage(null, "Multigraphs are not allowed.", "Multigraph error", 
                                                       Dialog.Message.Error)
             else{
               val weight = askForWeight
               g.addEdge(vertex1, vertex2, weight)
               directedBox.enabled = false
  	           weightedBox.enabled = false
             }
           }
           lineStart = null
           repaint()
         }
         movingPoint = null
         dragging = false
        
       } // end of reactions
     
     def drawArrow(graphics: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int) {
       val headSize = 8
       val dx = x2 - x1
       val dy = y2 - y1
       val angle = math.atan2(dy, dx)
       val len = (math.sqrt(dx*dx + dy*dy)).toInt
       var at = geom.AffineTransform.getTranslateInstance(x1, y1)
       at.concatenate(geom.AffineTransform.getRotateInstance(angle))
       graphics.transform(at)                
       graphics.fillPolygon(Array(len, len-headSize, len-headSize, len),
                     Array(0, -headSize, headSize, 0), 4)                            
       graphics.rotate(-angle)
       at = geom.AffineTransform.getTranslateInstance(-x1, -y1)
       graphics.transform(at)
     }
     
     override def paintComponent(graphics: Graphics2D){
       super.paintComponent(graphics)
       graphics.setColor(Color.BLACK)
       if (lineStart != null){
         graphics.drawLine(lineStart.x, lineStart.y, movingPoint.x, movingPoint.y)
         if (g.directed) drawArrow(graphics, lineStart.x, lineStart.y, (lineStart.x + movingPoint.x)/2, 
             (lineStart.y + movingPoint.y)/2)
       }
       for (e <- g.edges){
         val startx = e.start.point.x+Vertex.size/2
         val starty = e.start.point.y+Vertex.size/2
         val endx = e.end.point.x+Vertex.size/2
         val endy = e.end.point.y+Vertex.size/2
         if (e.start == e.end) graphics.drawOval(startx, starty - Vertex.size, Vertex.size, Vertex.size)
         else {
           graphics.drawLine(startx, starty, endx, endy)
           if (g.directed) drawArrow(graphics, startx, starty, (startx + endx)/2, (starty + endy)/2)
           if (g.weighted) graphics.drawString(e.weight.toString(), (startx + endx)/2-10, (starty + endy)/2-10)
         }
       }
       for (v <- g.vertices){
         graphics.setColor(Color.BLUE)
         graphics.fillOval(v.point.x, v.point.y, Vertex.size, Vertex.size)
         graphics.setColor(Color.WHITE)
         var offsetx = (0.4 * Vertex.size).toInt
         if (v.label > 9) offsetx = (0.2 * Vertex.size).toInt
         val offsety = (0.75 * Vertex.size).toInt
         graphics.drawString(v.label.toString(), v.point.x+offsetx, v.point.y+offsety)
       }
     }
    
   }
  
  val instructionsArea = new TextArea {
    editable = false
    //maximumSize = new Dimension(200,800)
    columns = 15
    background = Color.WHITE
    text = "Welcome to graph GUI app. First, choose your graph's type. If you wanted to change it, you'd have to delete "+
    "all edges of graph, so it's better to do it now. Then, start creating your own graph by adding new vertices and edges."
    lineWrap = true
    wordWrap = true
  }
  
  val resultsArea = new TextArea{
    editable = false
    wordWrap = true
    lineWrap = true
    columns = 15
    background = Color.LIGHT_GRAY
    text = "Results will be here"
 }

  def top = new MainFrame {
    title = "Some sick title"
    
    import BorderPanel.Position._
    contents = new BorderPanel {
      
      layout += drawingPanel -> Center
      
      layout += new BoxPanel(Orientation.Vertical){
        val vertexButton = new Button("Add/remove/move a vertex")
        val edgeButton= new Button("Add/remove an edge")
        val clearButton = new Button("Clear")
        val label = new Label("graph's properties")   
        val computeButton = new Button("Compute")
        contents += vertexButton
        contents += new Label(" ")
        contents += edgeButton
        contents += new Label(" ")
        contents += clearButton
        contents += new Label(" ")
        contents += label
        contents += directedBox
        contents += weightedBox
        contents += new Label(" ")
        contents += new Label(" ")
        contents += computeButton
        contents += new Label(" ")
        listenTo(vertexButton, edgeButton, clearButton, directedBox, weightedBox, computeButton)
        reactions += {
          case ButtonClicked(`vertexButton`) => 
             chosenVertices = true
             instructionsArea.text = "Click left button on drawing panel to create a vertex, right click to remove. "+
             "Left click and hold to move existing vertex to another place"
            
          case ButtonClicked(`edgeButton`) => 
             chosenVertices = false
             instructionsArea.text = "To add an edge, click left button on the first vertex and then drag the edge to the"+
             "second vertex. To remove an edge, click right button on vertices which are connected by the edge you want "+
             "to remove."
            
          case ButtonClicked(`clearButton`) =>
             val res = g.convert
             clearEverything

          case ButtonClicked(`directedBox`) => g.directed = directedBox.selected
          case ButtonClicked(`weightedBox`) => g.weighted = weightedBox.selected
          case ButtonClicked(`computeButton`) => resultsArea.text = "cyclic: " + Cycle.isCyclic(g).toString()
        }
        
      } -> West
      
      layout += instructionsArea -> South
      
      
      layout += new BoxPanel(Orientation.Vertical) {
        val resultsLabel = new Label("Results")  
        contents += resultsLabel
        contents += resultsArea
      } -> East
    }
    menuBar = new MenuBar{
      contents += new Menu("File"){
        contents += new MenuItem(Action("Open graph"){openGraph})
        contents += new MenuItem(Action("Save graph"){saveGraph})
      }
    }
    size = new Dimension(1200,800)
    centerOnScreen
  }
}