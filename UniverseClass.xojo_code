#tag Class
Protected Class UniverseClass
	#tag Method, Flags = &h0
		Sub Constructor(R As Double)
		  Z = HubbleConstant*R
		  DZDR = HubbleConstant
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDZDR() As Double
		  return DZDR
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetZ() As Double
		  return Z
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		DZDR As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		HubbleConstant As Double = 2.2798e-18
	#tag EndProperty

	#tag Property, Flags = &h0
		Z As Double
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
			Name="HubbleConstant"
			Visible=false
			Group="Behavior"
			InitialValue="2.2798e-18"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
