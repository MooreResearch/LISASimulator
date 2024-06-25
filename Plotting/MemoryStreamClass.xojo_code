#tag Class
Protected Class MemoryStreamClass
	#tag Method, Flags = &h0
		Sub Constructor(TheName As String, TheSize As Integer)
		  Name = TheName
		  Values.ResizeTo(TheSize)
		  Index = 0
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetData() As Double()
		  Return Values
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Write(TheValue As Double)
		  Values(Index) = TheValue
		  Index = Index + 1
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h21
		Private Index As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Name As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Values() As Double
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
	#tag EndViewBehavior
End Class
#tag EndClass
