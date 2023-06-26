#tag Class
Protected Class HCalculatorClass
	#tag Property, Flags = &h0
		HP0Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP1Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP1SOPCalculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP2Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP2SOCalculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP3Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP3SOCalculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX0Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX1Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX1SOCalculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX2Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX2SOCalculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX3Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX3SOCalculator As HNCalculator
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
