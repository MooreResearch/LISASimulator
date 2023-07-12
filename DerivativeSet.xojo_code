#tag Class
Protected Class DerivativeSet
	#tag Property, Flags = &h0
		Dh0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dz As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dλ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Value As Double
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
			Name="Dh0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass