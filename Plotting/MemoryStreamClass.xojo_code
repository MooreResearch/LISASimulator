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
		  // The last double we wrote was at Index - 1, so resize the array to be just that length
		  Values.ResizeTo(Index-1)
		  Return Values // return the array
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
	#tag EndViewBehavior
End Class
#tag EndClass
