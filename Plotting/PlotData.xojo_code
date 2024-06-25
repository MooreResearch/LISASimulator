#tag Class
Protected Class PlotData
	#tag Method, Flags = &h0
		Function GetStart() As Integer
		  Return StartIndex
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetStop() As Integer
		  Return StopIndex
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetXArray() As Double()
		  Return XValues
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetXMax() As Double
		  Return XMax
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetXMin() As Double
		  Return XMin
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetYArray() As Double()
		  Return YValues
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetYMax() As Double
		  Return YMax
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetYMin() As Double
		  Return YMin
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub RecalcateMaxMin()
		  XMin = 0.0
		  XMax = 1.0
		  YMin = 0.0
		  YMax = 1.0
		  
		  If XValues.LastIndex > -1 and YValues.LastIndex > -1 Then
		    
		    Var theXMin As Double = XValues(StartIndex)
		    Var theXMax As Double = XValues(StartIndex)
		    Var theYMin As Double = YValues(StartIndex)
		    Var theYMax As Double = YValues(StartIndex)
		    
		    For i As Integer = StartIndex + 1 To StopIndex
		      theXMin = Min(XValues(i), theXMin)
		      theXMax = Max(XValues(i), theXMax)
		      theYMin = Min(YValues(i), theYMin)
		      theYMax = Max(YValues(i), theYMax)
		    Next
		    
		    XMin = theXMin
		    XMax = theXMax
		    YMin = theYMin
		    YMax = theYMax
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetPlotArrays(TheXValues() as Double, TheYValues() as Double)
		  XValues = TheXValues
		  YValues = TheYValues
		  Var maxStopIndex As Integer = Min(XValues.LastIndex, YValues.LastIndex)
		  If StopIndex = 0 or StopIndex > maxStopIndex Then StopIndex = maxStopIndex
		  RecalcateMaxMin
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetPlotIndexRange(Start As Integer, Stop As Integer)
		  If Start < 0 Then 
		    StartIndex = 0
		  Else
		    StartIndex = Start
		  End If
		  If Stop <= 0 Or Stop > XValues.LastIndex Then
		    StopIndex = XValues.LastIndex
		  Else
		    StopIndex = Stop
		  End If
		  RecalcateMaxMin
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		LineColor As Color
	#tag EndProperty

	#tag Property, Flags = &h0
		LineWidth As Double = 1.0
	#tag EndProperty

	#tag Property, Flags = &h21
		Private StartIndex As Integer = 0
	#tag EndProperty

	#tag Property, Flags = &h21
		Private StopIndex As Integer = 0
	#tag EndProperty

	#tag Property, Flags = &h21
		Private XMax As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private XMin As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private XValues() As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private YMax As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private YMin As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private YValues() As Double
	#tag EndProperty


	#tag ViewBehavior
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
			Name="LineColor"
			Visible=false
			Group="Behavior"
			InitialValue="&c000000"
			Type="Color"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LineWidth"
			Visible=false
			Group="Behavior"
			InitialValue="1.0"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
