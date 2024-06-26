#tag Class
Protected Class PlotAxis
Inherits Group2D
	#tag Method, Flags = &h21
		Private Sub AddLabels(TheTickLabelHeight As Double, TheTickPower As Integer)
		  // Tis method adds the axis and power labels to the group. The anchor for the 
		  // StyledTextShape group2D is the group's baseline and horizontal center.
		  // The AxisLabelOffset and PowerLabelOffset are meant to specify the 
		  // white space between the bottom of the major tick labels and the
		  // label in question, whose location is specified by Height.
		  
		  // If one calls this with theTickLabelHeight set to 0, then we can use the same
		  // code to predict the height of the x axis, even before we have "defined" that
		  // axis. That way, the predicted height should always agree with the actual height.
		  // But that height will only apply to the x axis.
		  
		  Var DoingXPrediction As Boolean = TheTickLabelHeight = 0.0
		  
		  Height = 0.0  // We don't know what the height is yet.
		  
		  Var tickHeight As Double
		  If MajorTickHeight > 0.0 Then
		    tickHeight = MajorTickHeight
		  ElseIf TickHeight > 0.0 Then
		    tickHeight = MinorTickHeight
		  End If
		  If DoingXPrediction Then
		    Var theLabel As New StyledTextShape
		    theLabel.SetFont(TickLabelFont, TickLabelFontSize)
		    theLabel.SetText("0.00")
		    Height = tickHeight + theLabel.Height + TickLabelOffset
		  Else
		    Height = tickHeight + TheTickLabelHeight + TickLabelOffset
		  End If
		  
		  Var axisLabelHeight As Double = 0.0
		  Var powerLabelHeight As Double = 0.0
		  
		  If Not AxisLabelText.IsEmpty Then
		    Var theLabel As New StyledTextShape // create a new styled text shape
		    theLabel.SetFont(AxisLabelFont, AxisLabelFontSize) // set its font characteristics
		    theLabel.Bold = AxisLabelBold
		    theLabel.SetText(AxisLabelText) // this actually creates the label group
		    axisLabelHeight = AxisLabelOffSet + theLabel.Height
		    If Not DoingXPrediction Then // if we are not simply predicting, then we'll add in the label
		      If YAxis Then
		        theLabel.Rotation = -1.57079632679 // rotate the label counterclockwise 90°
		        theLabel.Y = -MyLength/2  // center vertically on the axis
		        theLabel.X = -Height - AxisLabelOffSet // baseline is facing the axis,
		        // so we do not need to add in the height
		      Else 
		        // center the text horizontally on the axis
		        theLabel.X = MyLength/2
		        theLabel.Y = Height + axisLabelHeight // locate the label's baseline
		      End If
		      AddObject(theLabel) // add the label into the axis group
		    End If
		  End If
		  
		  If TheTickPower <> 0 Then
		    Var theLabel As New StyledTextShape // create a new styled text shape
		    theLabel.SetFont(TickLabelFont, TickLabelFontSize)
		    theLabel.SetText("×10^{" + TickPower.ToString + "}")
		    powerLabelHeight = PowerLabelOffset + theLabel.Height
		    If Not DoingXPrediction Then
		      If YAxis Then
		        theLabel.Y = TickLabelFontSize/2 + PowerLabelOffset + theLabel.Height - MyLength
		        theLabel.X = -TickLabelOffset - theLabel.Width/2 - tickHeight
		      Else
		        theLabel.X = MyLength // this should put it right under the last label
		        theLabel.Y = Height + powerLabelHeight
		      End If
		      AddObject(theLabel)
		    End If
		  End If
		  
		  // for a Y axis, the power label is very unlikely to make any difference
		  If YAxis Then
		    Height = Height + axisLabelHeight  // so we ignore it
		  Else
		    Height = Height + Max(axisLabelHeight, powerLabelHeight)
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub AddMajorTick(XPos As Double)
		  // This method defines and adds a major tick mark and label at the location specified.
		  
		  Var line As New CurveShape // define a new line
		  line.Order = 0 // a straight line
		  If YAxis Then
		    line.Y = -XPos
		    line.Y2 = -XPos
		    line.X = 0.0
		    line.X2 = -MajorTickHeight
		  Else
		    line.X = XPos // at the tick location
		    line.X2 = XPos
		    line.Y = 0.0
		    line.Y2 = MajorTickHeight // with height equal to the MajorTickHeight
		  End If
		  AddObject(line) // add the line to the collection of shapes
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub AddMajorTickLabel(XPos As Double, Label As String)
		  // This method defines and adds a major tick label at the location specified.
		  
		  Var tickLabel As New TextShape  // (no need for a StyledTextShape here)
		  tickLabel.FontName = TickLabelFont // set font
		  tickLabel.FontSize = TickLabelFontSize // set font size
		  tickLabel.Text = Label // set the text of the label
		  Var tickShift As Double = 0.0
		  If MajorTickHeight > 0.0 Then
		    tickShift = MajorTickHeight
		  Elseif MinorTickHeight > 0.0 Then
		    tickShift = MinorTickHeight
		  End If
		  
		  If YAxis Then // if we are drawing a y axis, we want the right sides of the tick text to be aligned
		    tickLabel.HorizontalAlignment = TextShape.Alignment.Right
		    tickLabel.VerticalAlignment = TextShape.Alignment.Center
		    tickLabel.Y = -XPos  // this will put the vertical center at the appropriate y-position
		    tickLabel.X = -(TickLabelOffset + tickShift)  // and the right edge at the offset
		  Else // if we are drawing an x axis, we want the label horizontally centered
		    tickLabel.HorizontalAlignment = TextShape.Alignment.Center
		    tickLabel.VerticalAlignment = TextShape.Alignment.Top // and vertically offset from the top
		    tickLabel.X = XPos // center position
		    tickLabel.Y = TickLabelOffset + tickShift // top position
		  End If
		  AddObject(tickLabel) // finally, add the label into the axis group
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub AddMinorTick(XPos As Double)
		  // This method defines and adds a minor tick mark at the x-location specified
		  Var line As New CurveShape // define a new line
		  line.Order = 0 // a straight line
		  If YAxis Then
		    line.Y = -XPos
		    line.Y2 = -XPos
		    line.X = 0.0
		    line.X2 = -MinorTickHeight
		  Else
		    line.X = XPos // at the tick location
		    line.X2 = XPos
		    line.Y = 0.0
		    line.Y2 = MinorTickHeight // with height equal to the MinorTickHeight
		  End If
		  AddObject(line)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub AssembleAxis()
		  // This removes all objects: we will rebuild everything from scratch
		  While Count > 0
		    RemoveObjectAt(Count-1)
		  Wend
		  TickPositions.ResizeTo(-1)  // Clear out the major tick positions array.
		  
		  // We first define the deltas between major and minor ticks using the MinorTickIndex property.
		  // nMinorPerMajor is the number of minor ticks per major tick. tFactor is the external factor
		  // by which we multiply the displayed tick values to get the actual tick value.
		  // tFormat is the format string we will use to display the ticks
		  
		  Var minorTickDelta As Double = GetTickDelta4Index(MinorTickIndex)
		  Var majorTickDelta As Double = GetTickDelta4Index(MinorTickIndex + 2) // Increase the index for wider spacing
		  Var nMinorPerMajor As Integer = Round(majorTickDelta / minorTickDelta)
		  Var tFactor As Double = Pow(10, TickPower)
		  Var tFormat As String = GetTickFormat(majorTickDelta)
		  
		  // nMin*minorTickDelta*tFactor is the full value of the first tick
		  // nMax*minorTickDelta*tFactor is the same for the last.
		  // nextMajor is the index of the next major tick. The 0.05
		  // is a fudge that makes sure that even if the double-precision
		  // calculations end up being just a bit off, a tick mark that is
		  // supposed to be "at" the axis minimum or maximum is not missed
		  
		  Var nMin As Integer = Ceiling(MyMinValue / (minorTickDelta * tFactor) - 0.05)
		  Var nMax As Integer = Floor(MyMaxValue / (minorTickDelta * tFactor) + 0.05)
		  Var nextMajor As Integer = nMinorPerMajor * Ceiling(MyMinValue / (majorTickDelta * tFactor) - 0.05)
		  
		  // We need to know the height of the tick label in order to offset the axis label correctly.
		  // tLabelHeight will store the height of the tick-mark labels
		  Var tLabelHeight As Double = GetTickLabelHeight(tFormat)
		  
		  // We will first add the axis and power labels. This method also sets the Height property.
		  X = 0.0
		  Y = 0.0
		  AddLabels(tLabelHeight, TickPower)
		  
		  // Now we are ready for the loop that adds the tick marks.
		  // The anchor point for each axis is at the origin of the graph.
		  
		  For n As Integer = nMin To nMax
		    // get the position of this particular tick along the axis
		    Var x As Double = (n * minorTickDelta * tFactor - MyMinValue) * Value2Pixels
		    
		    If n = nextMajor Then
		      // if this is a major tick, update TickPositions (for help drawing the grid)
		      // and add in the major tick and its label (if allowed).
		      If n > nMin And n < nMax Then TickPositions.Add(x) // store tick positions for drawing the grid
		      If MajorTickHeight > 0.0 Then AddMajorTick(x)
		      AddMajorTickLabel(x, Format(n * minorTickDelta, tFormat))
		      nextMajor = nextMajor + nMinorPerMajor  // update the major tick index
		    Else // otherwise, we will draw the minor ticks if allowed
		      If MinorTickHeight > 0.0 Then AddMinorTick(x)
		    End If
		  Next  // next tick position.
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub AssembleXAxis(MinValue As Double, MaxValue As Double, Length As Double)
		  DefineAxis(MinValue, MaxValue, Length)
		  AssembleAxis
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub AssembleYAxis(MinValue As Double, MaxValue As Double, Length As Double)
		  DefineAxis(MinValue, MaxValue, Length)
		  YAxis = True
		  AssembleAxis
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function CalcTickPower(MinVal As Double, MaxVal As Double) As Integer
		  Var maxMag As Double = Max(Abs(MinVal), Abs(MaxVal))
		  Var thePower As Integer
		  If maxMag < 10000.0 and maxMag > 0.01 Then
		    thePower = 0
		  Else
		    thePower = 3*Floor(Log(maxMag)/Log(1000.0) + 0.00001)
		  End If
		  Return thePower
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub DefineAxis(MinValue As Double, MaxValue As Double, Length As Double)
		  // This method defines crucial aspects of an axis
		  // by specifying the values corresponding to its ends,
		  // deciding on a tick size, and setting various paraemeters
		  // needed to draw the axis and return meaningful
		  // coordinates. The parameters correspond to the
		  // maximum and minimum values to be displayed along
		  // the axis, and the length of the axis in pixels.
		  
		  Var tempMax As Double = MaxValue
		  Var tempMin As Double = MinValue
		  
		  // Check to see whether we are supposed to include zero
		  If IncludeZero Then
		    If MaxValue < 0 Then
		      tempMax = 0.0
		    ElseIf MinValue > 0.0 Then
		      tempMin = 0.0
		    End If
		  End If
		  
		  // This is the range the axis needs to span
		  Var range As Double = tempMax - tempMin
		  
		  Value2Pixels = 0.0 // A nonzero value indicates a successful definition.
		  
		  // First check that we have a meaningful width.
		  
		  If Length > 0 and range > 0 Then
		    
		    MyLength = Length  // store the length variable for later
		    
		    // Compute the TickPower property of the axis. This is the
		    // power of 10 that multiplies the the tick value displayed
		    // on the graph to yield the actual tick value, and is always
		    // a multiple of 3.
		    
		    TickPower = CalcTickPower(tempMin, tempMax)
		    Var tFactor As Double = Pow(10,TickPower)
		    
		    // Choose the major tick size. We want at least MinPixPerTick
		    // pixels between minor tick marks on the graph canvas. To
		    // defined the minor tick to be initially MinPixPerTick pixels
		    // wide, and then adjust it to the next larger tick size that is
		    // 1, 2, or 5 times some integer power of 10. A "TickIndex"
		    // is an integer that specifies the tick size through the
		    // TickSize function.
		    
		    Var minorTickDelta As Double = range*MinPixPerTick/Length
		    MinorTickIndex = Ceiling(3*Log(minorTickDelta/tFactor)/Log(10) - 0.15)
		    Var majorTickDelta As Double = GetTickDelta4Index(MinorTickIndex+2)
		    
		    If Not YAxis Then
		      Var theFormat As String = GetTickFormat(majorTickDelta)
		      Var theWidth As Double = GetTickLabelWidth(theFormat)
		      If theWidth > 4*MinPixPerTick Then
		        Var newMinPixPerTick As Double = theWidth*1.1/4
		        minorTickDelta = range*newMinPixPerTick/Length
		        MinorTickIndex = Ceiling(3*Log(minorTickDelta/tFactor)/Log(10) - 0.15)
		        majorTickDelta = GetTickDelta4Index(MinorTickIndex+2)
		      End If
		    End If
		    
		    // Depending on the value of the AxisPadding property, we
		    // now set the actual beginning and final values for the axis.
		    
		    If AxisPadding Then
		      MyMaxValue = majorTickDelta*Ceiling(MaxValue/(majorTickDelta*tFactor) - 0.000001)*tFactor
		      MyMinValue = majorTickDelta*Floor(MinValue/(majorTickDelta*tFactor) + 0.000001)*tFactor
		    Else
		      MyMaxValue = MaxValue
		      MyMinValue = MinValue
		    End If
		    
		    // This finally gives the conversion factor for converting values to pixels.
		    // This defines whether the definition was successful or not.
		    Value2Pixels = Length/(MyMaxValue - MyMinValue)
		    
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetHeight() As Double
		  Return Height
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetLength() As Double
		  Return MyLength
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetMaxValue() As Double
		  Return MyMaxValue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetMinValue() As Double
		  Return MyMinValue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetPixArrayFor(Values() As Double, Start As Integer, Stop As Integer) As Double()
		  Var yFactor As Double = 1.0
		  If YAxis Then yFactor = -1.0
		  Var pixArray() As Double
		  pixArray.ResizeTo(Stop - Start)
		  For i As Integer = Start To Stop
		    pixArray(i-Start) = (Values(i) - MyMinValue)*Value2Pixels*yFactor
		  Next
		  Return pixArray
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetPixFor(Value As Double) As Double
		  Var pix As Double = (Value - MyMinValue)*Value2Pixels
		  If YAxis Then pix = -pix
		  Return pix
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function GetTickDelta4Index(N As Integer) As Double
		  // This method returns an exact tick size when given an integer that is roughly
		  // 3 times the base-ten log of a rough estimate of the tick size.
		  
		  Var nman As Integer  // integer specifying which tick mantissa to choose
		  Var npow As Integer // specifies the tick power
		  Var digit As Double // the actual mantissa value
		  
		  if N >= 0 then
		    nman = N mod 3
		    npow = N\3
		  else
		    nman = (N mod 3) + 3
		    npow = N\3 - 1
		  end if
		  
		  Select case nman
		  Case 0
		    digit = 1.0
		  Case 1
		    digit = 2.0
		  Case 2
		    digit = 5.0
		  Case 3
		    digit = 10.0
		  end select
		  Return digit*Pow(10, npow)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function GetTickFormat(MajorTickDelta As Double) As String
		  // This method provides a format string for formatting the major
		  // ticks on the x or y axes. Note that this method assumes
		  // that the major tick size will be >= 0.0001,
		  // which is true for all the methods calling this method.
		  
		  If MajorTickDelta >= 1 Then
		    Return "-#0"
		  ElseIf MajorTickDelta >= 0.1 Then
		    Return "-#0.0"
		  ElseIf MajorTickDelta >= 0.01 Then
		    Return "-#0.00"
		  ElseIf MajorTickDelta >= 0.001 Then
		    Return "-#0.000"
		  Elseif MajorTickDelta >= 0.0001 Then
		    Return "-#0.0000"
		  Elseif MajorTickDelta >= 1e-5 Then
		    Return "-#0.00000"
		  Elseif MajorTickDelta >= 1e-6 Then
		    Return "-#0.000000"
		  Elseif MajorTickDelta >= 1e-7 Then
		    Return "-#0.0000000"
		  Elseif MajorTickDelta >= 1e-8 Then
		    Return "-#0.00000000"
		  Elseif MajorTickDelta >= 1e-9 Then
		    Return "-#0.000000000"
		  Elseif MajorTickDelta >= 1e-10 Then
		    Return "-#0.0000000000"
		  Else
		    Return "-#0.00000000000"
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function GetTickLabelHeight(TickFormat As String) As Double
		  // We are going to define the height of major tick labels by constructing a
		  // StyledTextShape object of the format and examining its height and width
		  // parameters. We want to actually calculate the maximum value, so we
		  // will try both extremes.
		  
		  Var tFactor As Double = 1.0
		  If TickPower <> 0 Then tFactor = Pow(10, TickPower)
		  Var testLabel As New StyledTextShape
		  testLabel.SetFont(TickLabelFont, TickLabelFontSize) // set the fonts
		  testLabel.SetText(Format(-Abs(MyMaxValue) / tFactor, TickFormat)) // set the text for this value
		  Var theWidth As Double = testLabel.Width
		  Var theHeight As Double = testLabel.Height
		  testLabel.SetText(Format(-Abs(MyMinValue) / tFactor, TickFormat))
		  
		  // if this is the y axis, then we are going to turn the labels sideways,
		  // so the "height" we need to reserve is actually the string's width.
		  
		  If YAxis Then
		    Return Max(theWidth, testLabel.Width)
		  Else
		    Return Max(theHeight, testLabel.Height)
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function GetTickLabelWidth(TickFormat As String) As Double
		  // We are going to define the height of major tick labels by constructing a
		  // StyledTextShape object of the format and examining its height and width
		  // parameters. We want to actually calculate the maximum value, so we
		  // will try both extremes.
		  
		  Var tFactor As Double = 1.0
		  If TickPower <> 0 Then tFactor = Pow(10, TickPower)
		  Var testLabel As New StyledTextShape
		  testLabel.SetFont(TickLabelFont, TickLabelFontSize) // set the fonts
		  testLabel.SetText(Format(-Abs(MyMaxValue) / tFactor, TickFormat)) // set the text for this value
		  Var theWidth As Double = testLabel.Width
		  Var theHeight As Double = testLabel.Height
		  testLabel.SetText(Format(-Abs(MyMinValue) / tFactor, TickFormat))
		  
		  // if this is the y axis, then we are going to turn the labels sideways,
		  // so the "height" we need to reserve is actually the string's width.
		  
		  If YAxis Then
		    Return Max(theHeight, testLabel.Height)
		  Else
		    Return Max(theWidth, testLabel.Width)
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTickPositions() As Double()
		  Return TickPositions
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsYAxis() As Boolean
		  Return YAxis
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function PredictXAxisHeight(XMin As Double, XMax As Double) As Double
		  // This method predicts the height of the x axis before it is formally defined.
		  // (This only works if we are actually dealing with an x axis.)
		  
		  // First, estimate the TickPower so that we can see if there is a power label
		  Var theTickPower As Double = CalcTickPower(XMin, XMax)
		  
		  // Now, make a gesture toward adding both the axis label and power label
		  // without actually adding them. (The first argument being zero signals this.)
		  AddLabels(0, theTickPower)
		  
		  // Finally return the Height that results from this calculation
		  Return Height
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		AxisLabelBold As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		AxisLabelFont As String = "System"
	#tag EndProperty

	#tag Property, Flags = &h0
		AxisLabelFontSize As Double = 14
	#tag EndProperty

	#tag Property, Flags = &h0
		AxisLabelOffset As Double = 8.0
	#tag EndProperty

	#tag Property, Flags = &h0
		AxisLabelText As String
	#tag EndProperty

	#tag Property, Flags = &h0
		AxisPadding As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Height As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IncludeZero As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		MajorTickHeight As Double = 5.0
	#tag EndProperty

	#tag Property, Flags = &h0
		MinorTickHeight As Double = 3.0
	#tag EndProperty

	#tag Property, Flags = &h21
		Private MinorTickIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		MinPixPerTick As Double = 7.0
	#tag EndProperty

	#tag Property, Flags = &h21
		Private MyLength As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private MyMaxValue As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private MyMinValue As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		PowerLabelOffset As Double = 1.0
	#tag EndProperty

	#tag Property, Flags = &h0
		TickLabelFont As String = "System"
	#tag EndProperty

	#tag Property, Flags = &h0
		TickLabelFontSize As Double = 12
	#tag EndProperty

	#tag Property, Flags = &h0
		TickLabelOffset As Double = 4.0
	#tag EndProperty

	#tag Property, Flags = &h21
		Private TickPositions() As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private TickPower As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Value2Pixels As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private YAxis As Boolean
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="BorderOpacity"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FillOpacity"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="BorderColor"
			Visible=false
			Group="Behavior"
			InitialValue="&h000000"
			Type="Color"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="BorderWidth"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Count"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FillColor"
			Visible=false
			Group="Behavior"
			InitialValue="&h000000"
			Type="Color"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Rotation"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Scale"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="X"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Y"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Double"
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
			Name="TickLabelFont"
			Visible=false
			Group="Behavior"
			InitialValue="System"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="TickLabelFontSize"
			Visible=false
			Group="Behavior"
			InitialValue="12"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="MinPixPerTick"
			Visible=false
			Group="Behavior"
			InitialValue="7.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AxisPadding"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Boolean"
			EditorType="Enum"
			#tag EnumValues
				"0 - NiceValue"
				"1 - NoPadding"
				"2 - IncludeZero"
			#tag EndEnumValues
		#tag EndViewProperty
		#tag ViewProperty
			Name="AxisLabelText"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="AxisLabelFont"
			Visible=false
			Group="Behavior"
			InitialValue="System"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="AxisLabelFontSize"
			Visible=false
			Group="Behavior"
			InitialValue="14"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IncludeZero"
			Visible=false
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TickLabelOffset"
			Visible=false
			Group="Behavior"
			InitialValue="8.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AxisLabelOffset"
			Visible=false
			Group="Behavior"
			InitialValue="8.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="MajorTickHeight"
			Visible=false
			Group="Behavior"
			InitialValue="5.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="MinorTickHeight"
			Visible=false
			Group="Behavior"
			InitialValue="3.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PowerLabelOffset"
			Visible=false
			Group="Behavior"
			InitialValue="4.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AxisLabelBold"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
