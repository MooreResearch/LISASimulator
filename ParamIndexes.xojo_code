#tag Module
Protected Module ParamIndexes
	#tag Method, Flags = &h0
		Function Derivative4Index(TheIndex As Integer) As String
		  // This method converts the name of a derivative denominator
		  // (as a string) to the corresponding index, as defined in this module
		  Select Case TheIndex
		  Case DlnM
		    Return "DlnM"
		  Case DlnR
		    Return "DlnR"
		  Case Dlnτc
		    Return "Dlnτc"
		  Case Dβ
		    Return "Dβ"
		  Case Dδ
		    Return "Dδ"
		  Case DΘ
		    Return "DΘ"
		  Case Dθ1
		    Return "Dθ1"
		  Case Dθ2
		    Return "Dθ2"
		  Case Dλ0
		    Return "Dλ0"
		  Case DΦ
		    Return "DΦ"
		  Case Dφ1
		    Return "Dφ1"
		  Case Dφ2
		    Return "Dφ2"
		  Case Dχ1
		    Return "Dχ1"
		  Case Dχ2
		    Return "Dχ2"
		  Case Dψ
		    Return "Dψ"
		  Else
		    Return "error"  // Error indication
		  End Select
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Index4Derivative(TheName As String) As Integer
		  // This method converts the name of a derivative denominator
		  // (as a string) to the corresponding index, as defined in this module
		  Select Case TheName
		  Case "DlnM"
		    Return DlnM
		  Case "DlnR"
		    Return DlnR
		  Case "Dlnτc"
		    Return Dlnτc
		  Case "Dβ"
		    Return Dβ
		  Case "Dδ"
		    Return Dδ
		  Case "DΘ"
		    Return DΘ
		  Case "Dθ1"
		    Return Dθ1
		  Case "Dθ2"
		    Return Dθ2
		  Case "Dλ0"
		    Return Dλ0
		  Case "DΦ"
		    Return DΦ
		  Case "Dφ1"
		    Return Dφ1
		  Case "Dφ2"
		    Return Dφ2
		  Case "Dχ1"
		    Return Dχ1
		  Case "Dχ2"
		    Return Dχ2
		  Case "Dψ"
		    Return Dψ
		  Else
		    Return -1  // Error indication
		  End Select
		End Function
	#tag EndMethod


	#tag Note, Name = About Parameter Indexes
		The D... indices are for derivatives and should reflect the physical ordering
		 of parameters working from the stars themselves outward to the detector:
		
		   Dχ1 = 0
		   Dθ1 = 1
		   Dφ1 = 2
		   Dχ2 = 3
		   Dθ2 = 4
		   Dφ2 = 5
		   Dδ = 6
		   Dlnτc = 7
		   Dλ0 = 8
		   DlnM = 9
		   Dβ = 10
		   Dψ = 11
		   DlnR = 12
		   DΘ = 13
		   DΦ = 14
		
		The LastSpinIndex = 7 represents the last parameter that affects the
		evolution of the spins. The LastParamIndex = 14 represents the last
		binary system parameter.
		
		The ix... indices are for reading in case information from the user interface
		in the RunWindow or from data files. These indices should follow the order
		in which the corresponding parameters appear in the user interface. The
		LastCaseIndex = 21 represents the last value to be read from the interface
		or file for a given case.
		
	#tag EndNote


	#tag Constant, Name = DlnM, Type = Double, Dynamic = False, Default = \"9", Scope = Public
	#tag EndConstant

	#tag Constant, Name = DlnR, Type = Double, Dynamic = False, Default = \"12", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Dlnτc, Type = Double, Dynamic = False, Default = \"7", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Dβ, Type = Double, Dynamic = False, Default = \"10", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Dδ, Type = Double, Dynamic = False, Default = \"6", Scope = Public
	#tag EndConstant

	#tag Constant, Name = DΘ, Type = Double, Dynamic = False, Default = \"13", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Dθ1, Type = Double, Dynamic = False, Default = \"1", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Dθ2, Type = Double, Dynamic = False, Default = \"4", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Dλ0, Type = Double, Dynamic = False, Default = \"8", Scope = Public
	#tag EndConstant

	#tag Constant, Name = DΦ, Type = Double, Dynamic = False, Default = \"14", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Dφ1, Type = Double, Dynamic = False, Default = \"2", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Dφ2, Type = Double, Dynamic = False, Default = \"2", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Dχ1, Type = Double, Dynamic = False, Default = \"0", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Dχ2, Type = Double, Dynamic = False, Default = \"3", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Dψ, Type = Double, Dynamic = False, Default = \"11", Scope = Public
	#tag EndConstant

	#tag Constant, Name = H0PLastIndex, Type = Double, Dynamic = False, Default = \"4", Scope = Public
	#tag EndConstant

	#tag Constant, Name = H0XLastIndex, Type = Double, Dynamic = False, Default = \"132", Scope = Public
	#tag EndConstant

	#tag Constant, Name = H1PLastIndex, Type = Double, Dynamic = False, Default = \"18", Scope = Public
	#tag EndConstant

	#tag Constant, Name = H1XLastIndex, Type = Double, Dynamic = False, Default = \"145", Scope = Public
	#tag EndConstant

	#tag Constant, Name = H2PLastIndex, Type = Double, Dynamic = False, Default = \"46", Scope = Public
	#tag EndConstant

	#tag Constant, Name = H2XLastIndex, Type = Double, Dynamic = False, Default = \"172", Scope = Public
	#tag EndConstant

	#tag Constant, Name = H3PLastIndex, Type = Double, Dynamic = False, Default = \"128", Scope = Public
	#tag EndConstant

	#tag Constant, Name = H3XLastIndex, Type = Double, Dynamic = False, Default = \"250", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixDet, Type = Double, Dynamic = False, Default = \"19", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixDetΘ0, Type = Double, Dynamic = False, Default = \"20", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixDetρ0, Type = Double, Dynamic = False, Default = \"22", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixDetΦ0, Type = Double, Dynamic = False, Default = \"21", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixDur, Type = Double, Dynamic = False, Default = \"16", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixID, Type = Double, Dynamic = False, Default = \"23", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixM, Type = Double, Dynamic = False, Default = \"0", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixPNA, Type = Double, Dynamic = False, Default = \"17", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixPNΨ, Type = Double, Dynamic = False, Default = \"18", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixR, Type = Double, Dynamic = False, Default = \"12", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixT, Type = Double, Dynamic = False, Default = \"2", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixβ, Type = Double, Dynamic = False, Default = \"10", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixδ, Type = Double, Dynamic = False, Default = \"1", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixΔT, Type = Double, Dynamic = False, Default = \"15", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixΘ, Type = Double, Dynamic = False, Default = \"13", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixθ1, Type = Double, Dynamic = False, Default = \"5", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixθ2, Type = Double, Dynamic = False, Default = \"8", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixλ0, Type = Double, Dynamic = False, Default = \"3", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixΦ, Type = Double, Dynamic = False, Default = \"14", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixφ1, Type = Double, Dynamic = False, Default = \"6", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixφ2, Type = Double, Dynamic = False, Default = \"9", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixχ1, Type = Double, Dynamic = False, Default = \"4", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixχ2, Type = Double, Dynamic = False, Default = \"7", Scope = Public
	#tag EndConstant

	#tag Constant, Name = ixψ, Type = Double, Dynamic = False, Default = \"11", Scope = Public
	#tag EndConstant

	#tag Constant, Name = LastCaseIndex, Type = Double, Dynamic = False, Default = \"23", Scope = Public
	#tag EndConstant

	#tag Constant, Name = LastParamIndex, Type = Double, Dynamic = False, Default = \"14", Scope = Public
	#tag EndConstant

	#tag Constant, Name = LastSpinIndex, Type = Double, Dynamic = False, Default = \"7", Scope = Public
	#tag EndConstant

	#tag Constant, Name = LastWaveTermIndex, Type = Double, Dynamic = False, Default = \"250", Scope = Public
	#tag EndConstant

	#tag Constant, Name = εDefault, Type = String, Dynamic = False, Default = \"1e-6", Scope = Public
	#tag EndConstant

	#tag Constant, Name = π, Type = Double, Dynamic = False, Default = \"3.141592653589793238", Scope = Public
	#tag EndConstant


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
End Module
#tag EndModule
