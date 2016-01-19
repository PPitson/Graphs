package gui

import scala.swing._
import scala.swing.event._
import java.awt.{Graphics2D, Color, geom, Point}
import java.awt.event._

import utils.Vertex
import utils.Graph
import utils.GraphFile._

import algorithms._


object GUI extends SimpleSwingApplication{
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
    g.weighted = false
    g.directed = false
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
      def isValid(s: String) = s.forall(c => c.isDigit || c == '.' || c == '-')
      val input = Dialog.showInput(null, "Weight: ", "Type in weight", Dialog.Message.Plain, Swing.EmptyIcon, Nil, "0")
		  if (input != None && input.get.length > 0 && isValid(input.get)){
		    if (input.get contains ".") weight = input.get.toDouble.toInt
		    else weight = input.get.toInt
		  }
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
    if (chooser.showSaveDialog(null) == FileChooser.Result.Approve) {
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
             else if (vertex1 == vertex2) Dialog.showMessage(null, "Loops are not allowed.", "Multigraph error", 
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
       val len = math.sqrt(dx*dx + dy*dy).toInt
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
         graphics.drawLine(startx, starty, endx, endy)
         if (g.directed) drawArrow(graphics, startx, starty, (startx + endx)/2, (starty + endy)/2)
         if (g.weighted) graphics.drawString(e.weight.toString, (startx + endx)/2-10, (starty + endy)/2-10)
       }
       for (v <- g.vertices){
         graphics.setColor(Color.BLUE)
         graphics.fillOval(v.point.x, v.point.y, Vertex.size, Vertex.size)
         graphics.setColor(Color.WHITE)
         var offsetx = (0.4 * Vertex.size).toInt
         if (v.label > 9) offsetx = (0.2 * Vertex.size).toInt
         val offsety = (0.75 * Vertex.size).toInt
         graphics.drawString(v.label.toString, v.point.x+offsetx, v.point.y+offsety)
       }
     }
    
   }
  
  val instructionsArea = new TextArea {
    editable = false
    columns = 15
    rows = 3
    background = Color.WHITE
    text = "Welcome to graph GUI app. First, choose your graph's properties. You should do it before adding any edges. "+
    "Then, get creative and create your own graph or open graph which you previously saved."
    lineWrap = true
    wordWrap = true
  }
  
  val resultsArea = new TextArea{
    editable = false
    wordWrap = true
    lineWrap = true
    columns = 15
    background = Color.LIGHT_GRAY
    text = ""
 }
 

   
  def top = new MainFrame {
    title = "Graphs"
    
    import BorderPanel.Position._
    contents = new BorderPanel {
      
      layout += drawingPanel -> Center
      
      layout += new BoxPanel(Orientation.Vertical){
        val vertexButton = new Button("Add/remove/move a vertex")
        val edgeButton= new Button("Add/remove an edge")
        val clearButton = new Button("Clear")
        val label = new Label("Graph's properties")
        val whatToCompute = new Label("What to compute:")
        val sizeCheck = new CheckBox("Size")
        val orderCheck = new CheckBox("Order")
        val isCyclicCheck = new CheckBox("Cyclicity")
        val isBipartiteCheck = new CheckBox("Bipartition")
        val isConnectedCheck = new CheckBox("Connectivity")
        val transpositionCheck = new CheckBox("Transposition")
        val degreeCheck = new CheckBox("Degree")
        val componentsCheck = new CheckBox("Connected components")
        val articulationPointsCheck = new CheckBox("Articulation points")
        val bridgesCheck = new CheckBox("Bridges")
        val flloydCheck = new CheckBox("Floyd-Warshall")
        val selectAll = new CheckBox("Select all")
        val computeButton = new Button("Compute")
        contents += vertexButton
        contents += new Separator(Orientation.Vertical){peer.setMaximumSize(new Dimension(0,10))}
        contents += edgeButton
        contents += new Separator(Orientation.Vertical){peer.setMaximumSize(new Dimension(0,10))}
        contents += clearButton
        contents += new Separator(Orientation.Vertical){peer.setMaximumSize(new Dimension(0,10))}
        contents += label
        contents += directedBox
        contents += weightedBox
        contents += new Separator(Orientation.Vertical){peer.setMaximumSize(new Dimension(0,10))}
        contents += new Separator(Orientation.Vertical){peer.setMaximumSize(new Dimension(0,10))}
        contents += whatToCompute
        contents += sizeCheck
        contents += orderCheck
        contents += isCyclicCheck
        contents += isBipartiteCheck
        contents += isConnectedCheck
        contents += transpositionCheck
        contents += degreeCheck
        contents += componentsCheck
        contents += articulationPointsCheck
        contents += bridgesCheck
        contents += flloydCheck
        contents += new Separator(Orientation.Vertical){peer.setMaximumSize(new Dimension(0,5))}
        contents += selectAll
        contents += computeButton
        contents += new Separator(Orientation.Vertical){peer.setMaximumSize(new Dimension(0,10))}
        listenTo(vertexButton, edgeButton, clearButton, directedBox, weightedBox, selectAll, computeButton)
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
            
          case ButtonClicked(`clearButton`) => clearEverything
          case ButtonClicked(`directedBox`) => g.directed = directedBox.selected
          case ButtonClicked(`weightedBox`) => g.weighted = weightedBox.selected
          case ButtonClicked(`selectAll`) =>
            sizeCheck.selected = selectAll.selected
            orderCheck.selected = selectAll.selected
            isCyclicCheck.selected = selectAll.selected
            isBipartiteCheck.selected = selectAll.selected
            isConnectedCheck.selected = selectAll.selected
            transpositionCheck.selected = selectAll.selected
            degreeCheck.selected = selectAll.selected
            componentsCheck.selected = selectAll.selected
            articulationPointsCheck.selected = selectAll.selected
            bridgesCheck.selected = selectAll.selected
            flloydCheck.selected = selectAll.selected
          case ButtonClicked(`computeButton`) =>
            resultsArea.text = ""
            if (sizeCheck.selected) resultsArea.text += "Size: " + g.edges.size + "\n\n"
            if (orderCheck.selected) resultsArea.text += "Order: " + g.vertices.size + "\n\n"
            if (isCyclicCheck.selected) resultsArea.text += "Cyclic: " + Cycle.isCyclic(g).toString + "\n\n"
            if (isBipartiteCheck.selected) resultsArea.text += "Bipartite: " + Divided.isDivided(g).toString + "\n\n"
            if (isConnectedCheck.selected) resultsArea.text += "Connected: " + Connectivity.isConnected(g).toString + "\n\n"
            if (transpositionCheck.selected) resultsArea.text += "Transposition: " + Transposition.transposition(g) + "\n\n"
            if (degreeCheck.selected) resultsArea.text += "Degree: " + Degree.degree(g) + "\n\n"
            if (componentsCheck.selected){
              if (g.directed) resultsArea.text += "Strong connected components: " + ConnectedComponents.computeStrong(g) + "\n"
              else resultsArea.text += "Connected components: " + ConnectedComponents.compute(g) + "\n"
            }
            if (articulationPointsCheck.selected) resultsArea.text += "Articulation points: " + ArticulationPoints.compute(g) + "\n"
            if (bridgesCheck.selected) resultsArea.text += "Bridges: " + Bridges.compute(g) + "\n"
            if (flloydCheck.selected) resultsArea.text += "Flloyd-Warshall results: " + "\n" + FlloydWarshall.calculate(g) + "\n"
        }
        
      } -> West
      
      layout += instructionsArea -> North
      
      
      layout += new BoxPanel(Orientation.Vertical) {
        val resultsLabel = new Label("Results")  
        contents += resultsLabel
        contents += new ScrollPane(resultsArea)

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