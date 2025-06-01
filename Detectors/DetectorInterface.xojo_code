#tag Interface
Protected Interface DetectorInterface
	#tag Method, Flags = &h0
		Function GetFPAndFX(Tau As Double) As Double()
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetName() As String
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetNoise(Freq As Double) As Double
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Initialize(Parameters As CaseInfoClass)
		  
		End Sub
	#tag EndMethod


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
End Interface
#tag EndInterface
