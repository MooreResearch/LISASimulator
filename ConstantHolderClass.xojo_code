#tag Class
Protected Class ConstantHolderClass
	#tag Method, Flags = &h0
		Sub Constructor(P As CaseParametersClass, Sn As Double, χaL As Double, χsL As Double)
		  Sn20 = Sn  // store the square root of the noise
		  
		  // Initialize constants for velocity evolution
		  Var δ As Double = P.δ
		  Var η As Double = P.η
		  Var π As Double = P.π
		  Var γE As Double = 0.5772156649015328606
		  V0 = 32*η/5
		  V2 = -743/336 - 11*η/4
		  V3 = 4*π - 47*χsL/3 - δ*25*χaL/4
		  V4 = 34103/18144 + 13661*η/2016 + 59*η*η/18
		  V5 = (-5861/144 + 1001*η/12)*χsL + δ*(-809/84 + 281*η/8)*χaL _
		  + 4159*π/672 + 189*π*η/8
		  V6 = 16477322263.0/139708800 - 1712*γE/105 + 16*π*π/3 _
		  + (-56198689/217728 + 451*π*π/48)*η _
		  + 541*η*η/896 - 5605*η*η*η/2592 - 856*Log(32)/105
		  V6L = 856/105
		  V7 = π*(-4415/4032 + 358675*η/6048 + 91495*η*η/1512)
		  
		  // Initialize constants for spin evolution
		  // Set up some constants that will be useful for the spin evolution equations.
		  Ω0 = 0.75 + η/2.0 -0.75*δ
		  Ω2 = 9.0/16.0 + 1.25*η + η*η/24.0 + 0.675*δ*η +(-9.0/16.0 + 0.675*η)*δ
		  Ω4 = 27.0/32.0 + 3.0*η/16.0 - 105.0*η*η/32.0 - η*η*η/48.0 + (-27.0/32.0 + 39.0*η/8.0 - 5.0*η*η/32.0)*δ
		  L1 = (1.0 + δ)/(1.0 - δ)
		  L2 = (1.0 - δ)/(1.0 + δ)
		  L3 = 1.5 + η/6.0
		  L4 = 27.0/8.0 - 19.0*η/8.0 + η*η/24.0
		  
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
		C2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C3β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C4β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C5β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Cβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Cβ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Cβ3 As Double
	#tag EndProperty

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
		F(247,9) As Double
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
		L1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		L2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		L3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		L4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S3β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S4β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S5β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sn20 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sβ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sβ3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V6L As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ω0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ω2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ω4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ω6 As Double
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
			Name="L2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="L3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V6L"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V7"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Ω0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Ω2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Ω4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Ω6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Cβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C2β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C3β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C4β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C5β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S2β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S3β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S4β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S5β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sβ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sβ3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Cβ3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Cβ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
			Name="L1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="L4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
