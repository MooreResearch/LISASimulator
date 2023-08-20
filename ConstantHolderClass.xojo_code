#tag Class
Protected Class ConstantHolderClass
	#tag Method, Flags = &h0
		Sub Constructor(P As CaseParametersClass, Sn As Double, χaL As Double, χsL As Double)
		  Sn20 = Sn  // store the square root of the noise
		  
		  // Initialize constants for velocity evolution
		  Var δ As Double = P.δ
		  Var η As Double = P.η
		  Var π As Double = P.π
		  
		  //Initialize constants for amplitude calculation
		  Var β As Double = P.β
		  Cβ = Cos(β)
		  Sβ = Sin(β)
		  C2β = Cos(2*β)
		  S2β = Sin(2*β)
		  C3β = Cos(3*β)
		  S3β = Sin(3*β)
		  C4β = Cos(4*β)
		  S4β = Sin(4*β)
		  C5β = Cos(5*β)
		  S5β = Sin(5*β)
		  Cβ2 = Cβ*Cβ
		  Sβ2 = Sβ*Sβ
		  Cβ3 = Cβ2*Cβ
		  Sβ3 = Sβ2*Sβ
		  
		  // Initialize constants for detector function calculation
		  DCCosΘ = Cos(P.Θ)
		  DCCos2Θ = Cos(2*P.Θ)
		  DCSinΘ = Sin(P.Θ)
		  DCSin2Θ = Sin(2*P.Θ)
		  DC2Φ = 2*P.Φ
		  DC2σ1 = 2*(0.75*P.π + P.ρ0)
		  DC2σ2 = DC2σ1 + 4*P.π/3.0
		  DCSinσ1x9= 9*Sin(DC2σ1-2*P.Φ)
		  DCSinσ1x9= 9*Sin(DC2σ2-2*P.Φ)
		  DCHalfSin2ψ = 0.5*Sin(2*P.ψ)
		  DCHalfCos2ψ = 0.5*Cos(2*P.ψ)
		  Var r3 As Double = Sqrt(3)
		  DC1 = r3/64
		  DC2 = 3/16
		  DC3 = 3*r3/64
		  
		  // Initialize constants for calculating the wave
		  H0PLastIndex = 4
		  H1PLastIndex = 18
		  H2PLastIndex = 46
		  H3PLastIndex = 128
		  H0XLastIndex = 132
		  H1XLastIndex = 145
		  H2XLastIndex = 172
		  H3XLastIndex = 247
		  
		  // Finally define term constants for amplitude calculation
		  F(0,0) =  -1.5-0.5*C2β
		  F(1,0) = -2*S2β
		  F(2,0) = 2*S2β
		  F(3,0) = -1.5-0.5*C2β
		  F(4,0) = -1.5*S2β*S2β
		  
		  F(132,0) = 4*Sβ
		  F(133,0) = -2*Cβ
		  F(134,0) = -4*Sβ
		  F(135,0) = -2*Cβ
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		DC1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DC2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DC2σ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DC2σ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DC2Φ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DC3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCCos2Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCCosΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCCosσ1x9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCCosσ2x9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCHalfCos2ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCHalfSin2ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCSin2Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCSinΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCSinσ1x9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCSinσ2x9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		H0PLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H0XLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H1PLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H1XLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H2PLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H2XLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H3PLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		H3XLastIndex As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Sn20 As Double
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
			Name="DC1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DC2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DC2σ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DC2σ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DC2Φ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DC3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCCos2Θ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCCosΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCCosσ1x9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCCosσ2x9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCHalfCos2ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCHalfSin2ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCSin2Θ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCSinΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCSinσ1x9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCSinσ2x9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H0PLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H0XLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H1PLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H1XLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H2PLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H2XLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H3PLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H3XLastIndex"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sn20"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
