#tag Class
Protected Class PlotItemClass
	#tag Method, Flags = &h0
		Function GetIndexMax() As Integer
		  Return -1
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetName() As String
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetValue() As Double
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Reset()
		  Index = -1
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetContext(TheContext As CaseSupervisorClass)
		  CS = TheContext
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetIndex(TheIndex As Integer)
		  Index = TheIndex
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h1
		Protected CS As CaseSupervisorClass
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected Index As Integer = -1
	#tag EndProperty


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
