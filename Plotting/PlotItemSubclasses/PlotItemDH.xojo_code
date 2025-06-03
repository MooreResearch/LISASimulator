#tag Class
Protected Class PlotItemDH
Inherits PlotItemClass
	#tag Method, Flags = &h0
		Function GetIndexMax() As Integer
		  Return LastParamIndex
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetName() As String
		  If Index > -1 Then
		    Return "DHI(" + Index.ToString + ")"
		  Else
		    Return "DHI()"
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetValue() As Double
		  Return CS.DHI(Index)
		  
		End Function
	#tag EndMethod


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue=""
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
			Name="Name"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
