#tag Class
Protected Class PlotCanvas
Inherits DesktopCanvas
	#tag Event
		Sub Opening()
		  XAxis = New PlotAxis
		  YAxis = New PlotAxis
		End Sub
	#tag EndEvent

	#tag Event
		Sub Paint(g As Graphics, areas() As Rect)
		  ClearTheGraph(g)
		  If OKToDraw Then
		    If GridObjects <> Nil Then
		      g.DrawObject(GridObjects)
		      If CurveObjects <> Nil Then g.DrawObject(CurveObjects)
		    End If
		    g.DrawObject(XAxis)
		    g.DrawObject(YAxis)
		    
		    
		  ElseIf GridObjects <> Nil Then
		    g.DrawObject(GridObjects)
		  End If
		  
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub AddDataToPlot(TheData As PlotData)
		  // This is the main method for defining the plot. It accepts
		  // a PlotData object, adds it to whatever data we might already
		  // have, defines all the graph characteristics needed to plot the
		  // whole set of data, and then constructs the shapes needed
		  // to draw the graph.
		  
		  // If we actually have new data, add it in
		  If theData <> Nil Then TheCurves.Add(TheData)
		  
		  // Clear the TitleShape in case the title has been changed
		  TheTitleShape = Nil
		  
		  // Start over with grid objects
		  GridObjects = New Group2D
		  
		  // DefineGraphRectangle actually defines the size of the
		  // graph rectangle and constructs the axis objects and title.
		  // If some error occurred, set up an error message as a
		  // grid object and display it.
		  DefineGraphRectangle
		  If Not OKToDraw Then // if we have encountered an error
		    // construct a new object to display an error message
		    Var errorShape As New TextShape
		    errorShape.Text = ErrorMessage
		    errorShape.X = Width/2
		    errorShape.Y = Height/2
		    GridObjects.AddObject(errorShape)
		    Refresh // display the message
		    Return // we are done here
		  End If
		  
		  // Draw the rectangle bordering the graph
		  Var theRectShape As New RectShape
		  theRectShape.X = 0.5*(XOfOrigin + XOfTopRight)
		  theRectShape.Y = 0.5*(YOfOrigin + YOfTopRight)
		  theRectShape.Width = XOfTopRight - XOfOrigin
		  theRectShape.Height = YOfOrigin - YOfTopRight
		  theRectShape.FillColor = Color.White
		  theRectShape.BorderColor = Color.Black
		  theRectShape.BorderOpacity = 100
		  GridObjects.AddObject(theRectShape)
		  
		  // Add in the title object
		  If TheTitleShape <> Nil Then GridObjects.AddObject(TheTitleShape)
		  
		  // Construct the actual grid objects
		  If DrawGrid Then BuildTheGrid
		  
		  // Finally add the actual curves (if we have any)
		  CurveObjects = New Group2D
		  If TheCurves.LastIndex > -1 Then
		    For i As Integer = 0 to TheCurves.LastIndex
		      AddTheCurve(TheCurves(i))
		    Next
		    // offset the curves according to the origin definition
		    CurveObjects.X = XOfOrigin
		    CurveObjects.Y = YOfOrigin
		  End If
		  
		  // Finally, make sure the graph gets displayed
		  Refresh
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub AddTheCurve(TheCurve As PlotData)
		  // This method constructs the curve for the plot data supplied
		  // and adds it to the CurveObjects group.
		  
		  Var start As Integer = TheCurve.GetStart
		  Var stop As Integer = TheCurve.GetStop
		  Var theColor As Color = TheCurve.LineColor
		  Var theWidth As Double = TheCurve.LineWidth
		  Var xPixelValues() As Double = XAxis.GetPixArrayFor(TheCurve.GetXArray, start, stop)
		  Var yPixelValues() As Double = YAxis.GetPixArrayFor(TheCurve.GetYArray, start, stop)
		  Var thisCurve As New Group2D
		  If xPixelValues <> Nil And yPixelValues <> Nil And xPixelValues.LastIndex > -1 Then
		    Var xStart As Double = xPixelValues(0)
		    Var yStart As Double = yPixelValues(0)
		    For i As Integer = 1 to xPixelValues.LastIndex
		      Var line As New CurveShape
		      line.Order = 0
		      line.BorderColor = theColor
		      line.BorderWidth = theWidth
		      line.X = xStart
		      line.Y = yStart
		      line.X2 = xPixelValues(i)
		      line.Y2 = yPixelValues(i)
		      thisCurve.AddObject(line)
		      xStart = line.X2
		      yStart = line.Y2
		    Next
		    CurveObjects.AddObject(thisCurve)
		  End If
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub BuildTheGrid()
		  // This method constructs the grid lines for the grid.
		  // It assumes that the grid lines have been fully defined.
		  
		  Var gridOffset As Double = 1.0 // inset for grid lines
		  
		  Var theTickPositions() As Double = XAxis.GetTickPositions
		  If theTickPositions <> Nil Then
		    For i As Integer = 0 to theTickPositions.LastIndex
		      Var x As Double = theTickPositions(i) + XOfOrigin + gridOffset
		      Var theGridLine As New CurveShape
		      theGridLine.Order = 0
		      theGridLine.BorderColor = Color.LightGray
		      theGridLine.X = x
		      theGridLine.X2 = x
		      theGridLine.Y = YOfTopRight + gridOffset
		      theGridLine.Y2 = YOfOrigin - gridOffset
		      GridObjects.AddObject(theGridLine)
		    Next
		  End If
		  theTickPositions = YAxis.GetTickPositions
		  If theTickPositions <> Nil Then
		    For i As Integer = 0 to theTickPositions.LastIndex
		      Var y As Double = YOfOrigin - theTickPositions(i)
		      Var theGridLine As New CurveShape
		      theGridLine.Order = 0
		      theGridLine.BorderColor = Color.LightGray
		      theGridLine.X = XOfOrigin + gridOffset
		      theGridLine.X2 = XOfTopRight - gridOffset
		      theGridLine.Y = y
		      theGridLine.Y2 = y
		      GridObjects.AddObject(theGridLine)
		    Next
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ClearPlotData()
		  TheCurves.RemoveAll
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub ClearTheGraph(g As Graphics)
		  g.DrawingColor = Color.White
		  g.FillRectangle(0,0,me.Width,me.Height)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub DefineGraphRectangle()
		  // This method does the hard work of deciding how big the graphics
		  // rectangle is going to be. It examines the title and the axes to decide
		  // how much space needs to be reserved inside the frame for these items.
		  // In the process, it also populates the axis objects so that they can be drawn.
		  // If everything works out, we will set the OKToDraw flag.
		  
		  OKToDraw = False  // we don't know yet whether it is going to work out
		  
		  // The first task is to define the maxima and minima. These are the default
		  // values if there is no data.
		  Var xMin As Double = 0.0
		  Var xMax As Double = 1.0
		  Var yMin As Double = 0.0
		  Var yMax As Double = 1.0
		  
		  // If there is some data, then scan through the data for maxima and minima.
		  If TheCurves.LastIndex > -1 Then
		    Var theCurve As PlotData = TheCurves(0)
		    xMin = theCurve.GetXMin
		    xMax = theCurve.GetXMax
		    yMin = theCurve.GetYMin
		    yMax = theCurve.GetYMax
		    Var xMinText As String = xMin.ToString
		    Var xMaxText As String = xMax.ToString
		    Var yMinText As String = yMin.ToString
		    Var yMaxText As String = yMax.ToString
		    
		    For i As Integer = 1 To theCurves.LastIndex
		      theCurve = theCurves(i)
		      xMin = Min(xMin, theCurve.GetXMin)
		      xMax = Max(xMax, theCurve.GetXMax)
		      yMin = Min(yMin, theCurve.GetYMin)
		      yMax = Max(yMax, theCurve.GetYMax)
		    Next
		    
		  End If
		  
		  // If the difference between the max and the min is zero,
		  // come up with something reasonable.
		  
		  if xMax = 0.0 and xMin = 0.0 Then // if they are both zero
		    xMax = 1.0
		    xMin = 0.0
		  ElseIf xMax = xMin Then // if they are the same but not zero
		    xMax = 1.5*xMax  // make the maximum somewhat bigger
		    xMin = 0.5*xMin  // and the minimum somewhat smaller
		    If xMax < xMin Then  // but if the max is actually smaller
		      Var temp As Double = xMax // because both are negative
		      xMax = xMin // swap the max and min values
		      xMin = temp
		    End if
		  End if
		  
		  // do the same for y
		  if yMax = 0.0 and yMin = 0.0 Then
		    yMax = 1.0
		    yMin = 0.0
		  ElseIf xMax = xMin Then
		    yMax = 1.5*yMax
		    yMin = 0.5*yMin
		    If yMax < yMin Then
		      Var temp As Double = yMax
		      yMax = yMin
		      yMin = temp
		    End if
		  End if
		  
		  YOfTopRight = MarginTop  // Reserve space at the top for the margin
		  
		  // Now we will rebuild the title shape in case it has changed.
		  If TheTitle.IsEmpty Then  // if we have no title
		    TheTitleShape = Nil  // then make shape Nil
		  Else // otherwise, construct the title objects
		    TheTitleShape = New StyledTextShape
		    TheTitleShape.SetFont(TitleFont, TitleFontSize)
		    TheTitleShape.SetText(TheTitle)
		    // We also will adjust the top margin
		    YOfTopRight = YOfTopRight + TheTitleShape.Height + TitleOffset
		  End If
		  
		  // Now, calculate the height of the x axis and use that to set the y-value
		  // of the origin. We can do this without actually defining that axis.
		  YOfOrigin = me.Height - XAxis.PredictXAxisHeight(xMin, xMax) - MarginBottom
		  
		  // This allows us to actually calculate the length of the y axis.
		  Var yLength As Double = YOfOrigin - YOfTopRight
		  
		  // If this is less than 100, we can't go on
		  If yLength < 100.0 Then
		    ErrorMessage = "Not enough space for the y axis."
		    Return
		  End If
		  
		  // If we get here, go ahead and create the y axis objects.
		  YAxis.AssembleYAxis(yMin, yMax, yLength)
		  
		  // Now that we know the YAxis height, we can define the
		  // ends of the XAxis and thus its length. Again, if the resulting
		  // length is less than 100, then we can't go on, but otherwise
		  // we can assemble the XAxis objects.
		  XOfOrigin = MarginLeft + YAxis.GetHeight
		  XOfTopRight = Width - MarginRight
		  Var xLength As Double = XOfTopRight - XOfOrigin
		  If xLength < 100.0 Then
		    ErrorMessage = "Not enough space for the x axis."
		    Return
		  End If
		  XAxis.AssembleXAxis(xMin, xMax, xLength)
		  
		  // Offset the axes according to the origin definitions
		  XAxis.X = XOfOrigin
		  XAxis.Y = YOfOrigin
		  YAxis.X = XOfOrigin
		  YAxis.Y = YOfOrigin
		  
		  // Offset the title to display correctly
		  If TheTitleShape <> Nil Then
		    TheTitleShape.X = 0.5*(XOfOrigin + XOfTopRight)
		    TheTitleShape.Y = YOfTopRight - TitleOffset
		  End If
		  
		  // If we get here, we have succeeded!
		  OKToDraw = True
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXAxisLabel(LabelText As String, FontName As String = "Same", FontSize As Double = 0.0, BoldFace As Integer = -1)
		  // This method sets up all the parameters associated with the axis label. Entering
		  // "Same" for either or both the first two parameters or zero for the third will preserve
		  // previously set values for those parameters while allowing one to change parameters
		  // further down the list. All the parameters except for the LabelText have defaults
		  // defined in the PlotAxis class.
		  
		  If LabelText <> "Same" Then XAxis.AxisLabelText = LabelText
		  If FontName <> "Same" And FontName <> "" Then XAxis.AxisLabelFont = FontName
		  If FontSize > 0.0 Then XAxis.AxisLabelFontSize = FontSize
		  If BoldFace > -1 Then XAxis.AxisLabelBold = (BoldFace > 0)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXAxisMinMinorTickPix(Value As Double)
		  // This property specifies the minimum number of pixels
		  // that we will allot between minor ticks.
		  
		  XAxis.MinPixPerTick = Value
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXAxisNiceEndsTo(Flag As Boolean)
		  // This method sets a flag that tells the axis builder to have the axis
		  // begin and end with a major tick value instead of the exact minimum
		  // and maximum. The default here is True.
		  XAxis.AxisPadding = Flag
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXAxisOffsets(TheAxisLabelOffset As Double, TheTickLabelOffset As Double, ThePowerLabelOffset As Double)
		  // This method sets the lengths of the various offsets for an axis. In each case,
		  // the offset specifies the white space between that item and the next item closer
		  // to the axis line. Submitting a negative value preserves the previous setting of the
		  // offset, allowing one to make changes to some values but not others. Default
		  // values are established in the PlotAxis class.
		  
		  If TheAxisLabelOffset >= 0.0 Then XAxis.AxisLabelOffset = TheAxisLabelOffset
		  If TheTickLabelOffset >= 0.0 Then XAxis.TickLabelOffset =TheTickLabelOffset
		  If ThePowerLabelOffset >= 0.0 Then XAxis.PowerLabelOffset = ThePowerLabelOffset
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXAxisTickFormat(FontName As String = "Same", FontSize As Double = 0.0)
		  // This method sets up all the parameters associated with the axis label. Entering
		  // "Same" for either or both the first two parameters or zero for the third will preserve
		  // previously set values for those parameters while allowing one to change parameters
		  // further down the list. All the parameters except for the LabelText have defaults
		  // defined in the PlotAxis class.
		  
		  If FontName <> "Same" And FontName <> "" Then XAxis.AxisLabelFont = FontName
		  If FontSize > 0.0 Then XAxis.AxisLabelFontSize = FontSize
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXAxisTickSizes(Major As Double, Minor As Double)
		  // This method sets the lengths of the major and minor tick lines.
		  // A negative value preserves a previous value (so you can set one
		  // without changing the other) and a zero value turns off the display
		  // of those kind of tick marks. Default values are defined in the
		  // PlotAxis class.
		  
		  If Major >= 0.0 Then XAxis.MajorTickHeight = Major
		  If Minor >= 0.0 Then XAxis.MinorTickHeight = Minor
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetXIncludeZeroTo(Flag As Boolean)
		  XAxis.IncludeZero = Flag
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYAxisLabel(LabelText As String, FontName As String = "Same", FontSize As Double = 0.0, BoldFace As Integer = -1)
		  // This method sets up all the parameters associated with the axis label. Entering
		  // "Same" for either or both the first two parameters or zero for the third will preserve
		  // previously set values for those parameters while allowing one to change parameters
		  // further down the list. All the parameters except for the LabelText have defaults
		  // defined in the PlotAxis class.
		  
		  If LabelText <> "Same" Then YAxis.AxisLabelText = LabelText
		  If FontName <> "Same" And FontName <> "" Then YAxis.AxisLabelFont = FontName
		  If FontSize > 0.0 Then YAxis.AxisLabelFontSize = FontSize
		  If BoldFace > -1 Then YAxis.AxisLabelBold = (BoldFace > 0)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYAxisMinMinorTickPix(Value As Double)
		  // This property specifies the minimum number of pixels
		  // that we will allot between minor ticks.
		  
		  YAxis.MinPixPerTick = Value
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYAxisNiceEndsTo(Flag As Boolean)
		  // This method sets a flag that tells the axis builder to have the axis
		  // begin and end with a major tick value instead of the exact minimum
		  // and maximum. The default here is True.
		  YAxis.AxisPadding = Flag
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYAxisOffsets(TheAxisLabelOffset As Double, TheTickLabelOffset As Double, ThePowerLabelOffset As Double)
		  // This method sets the lengths of the various offsets for an axis. In each case,
		  // the offset specifies the white space between that item and the next item closer
		  // to the axis line. Submitting a negative value preserves the previous setting of the
		  // offset, allowing one to make changes to some values but not others. Default
		  // values are established in the PlotAxis class.
		  
		  If TheAxisLabelOffset >= 0.0 Then YAxis.AxisLabelOffset = TheAxisLabelOffset
		  If TheTickLabelOffset >= 0.0 Then YAxis.TickLabelOffset =TheTickLabelOffset
		  If ThePowerLabelOffset >= 0.0 Then YAxis.PowerLabelOffset = ThePowerLabelOffset
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYAxisTickFormat(FontName As String = "Same", FontSize As Double = 0.0)
		  // This method sets up all the parameters associated with the axis label. Entering
		  // "Same" for either or both the first two parameters or zero for the third will preserve
		  // previously set values for those parameters while allowing one to change parameters
		  // further down the list. All the parameters except for the LabelText have defaults
		  // defined in the PlotAxis class.
		  
		  If FontName <> "Same" And FontName <> "" Then YAxis.AxisLabelFont = FontName
		  If FontSize > 0.0 Then YAxis.AxisLabelFontSize = FontSize
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYAxisTickSizes(Major As Double, Minor As Double)
		  // This method sets the lengths of the major and minor tick lines.
		  // A negative value preserves a previous value (so you can set one
		  // without changing the other) and a zero value turns off the display
		  // of those kind of tick marks. Default values are defined in the
		  // PlotAxis class.
		  
		  If Major >= 0.0 Then YAxis.MajorTickHeight = Major
		  If Minor >= 0.0 Then YAxis.MinorTickHeight = Minor
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetYIncludeZeroTo(Flag As Boolean)
		  YAxis.IncludeZero = Flag
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h21
		Private CurveObjects As Group2D
	#tag EndProperty

	#tag Property, Flags = &h0
		DrawGrid As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ErrorMessage As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private GridObjects As Group2D
	#tag EndProperty

	#tag Property, Flags = &h0
		MarginBottom As Double = 20.0
	#tag EndProperty

	#tag Property, Flags = &h0
		MarginLeft As Double = 20.0
	#tag EndProperty

	#tag Property, Flags = &h0
		MarginRight As Double = 20.0
	#tag EndProperty

	#tag Property, Flags = &h0
		MarginTop As Double = 20.0
	#tag EndProperty

	#tag Property, Flags = &h21
		Private OKToDraw As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h21
		Private TheCurves() As PlotData
	#tag EndProperty

	#tag Property, Flags = &h0
		TheTitle As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private TheTitleShape As StyledTextShape
	#tag EndProperty

	#tag Property, Flags = &h0
		TitleFont As String = "System"
	#tag EndProperty

	#tag Property, Flags = &h0
		TitleFontSize As Double = 18
	#tag EndProperty

	#tag Property, Flags = &h0
		TitleOffset As Double = 15.0
	#tag EndProperty

	#tag Property, Flags = &h21
		Private XAxis As PlotAxis
	#tag EndProperty

	#tag Property, Flags = &h21
		Private XOfOrigin As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private XOfTopRight As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private YAxis As PlotAxis
	#tag EndProperty

	#tag Property, Flags = &h21
		Private YOfOrigin As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private YOfTopRight As Double
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Width"
			Visible=true
			Group="Position"
			InitialValue="100"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Height"
			Visible=true
			Group="Position"
			InitialValue="100"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockLeft"
			Visible=true
			Group="Position"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockTop"
			Visible=true
			Group="Position"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockRight"
			Visible=true
			Group="Position"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockBottom"
			Visible=true
			Group="Position"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TabIndex"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TabPanelIndex"
			Visible=false
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TabStop"
			Visible=true
			Group="Position"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AllowAutoDeactivate"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Backdrop"
			Visible=true
			Group="Appearance"
			InitialValue=""
			Type="Picture"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Enabled"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Tooltip"
			Visible=true
			Group="Appearance"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="AllowFocusRing"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Visible"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AllowFocus"
			Visible=true
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AllowTabs"
			Visible=true
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Transparent"
			Visible=true
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TitleFont"
			Visible=false
			Group="Behavior"
			InitialValue="System"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="TitleFontSize"
			Visible=false
			Group="Behavior"
			InitialValue="18"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="MarginLeft"
			Visible=false
			Group="Behavior"
			InitialValue="20.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="MarginRight"
			Visible=false
			Group="Behavior"
			InitialValue="40.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="MarginTop"
			Visible=false
			Group="Behavior"
			InitialValue="20.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="MarginBottom"
			Visible=false
			Group="Behavior"
			InitialValue="20.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TitleOffset"
			Visible=false
			Group="Behavior"
			InitialValue="15.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TheTitle"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="DrawGrid"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
