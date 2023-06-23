#tag Class
Protected Class OrbitValuesClass
	#tag Property, Flags = &h0
		cosι As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DcosιDv0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DcosιDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DcosιDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DcosιDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DcosιDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DcosιDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DcosιDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DcosιDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDαDτDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDαDτDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDαDτDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDαDτDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDαDτDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDαDτDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDv0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDz As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDτ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		α As Double
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
			Name="α"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
