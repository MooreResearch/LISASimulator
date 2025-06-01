#tag Class
Protected Class VCalculatorClass
	#tag Method, Flags = &h0
		Sub Constructor(Theτc As Double, Theδ As Double, Theχ1ℓ As Double, Theχ2ℓ As Double, Theλ0 As Double)
		  δ = Theδ
		  η = (1-δ*δ)*0.25
		  γE = 0.577215664901533
		  // loading coefficients
		  Var onepδ As Double = 1.0 + δ
		  Var onemδ As Double = 1.0 - δ
		  Var δ2 As Double = δ*δ
		  Var δ3 As Double = δ2*δ
		  Var δ4 As Double = δ3*δ
		  Var δ5 As Double = δ4*δ
		  Var η2 As Double = η*η
		  Var η3 As Double = η*η2
		  χ1ℓ = Theχ1ℓ
		  χ2ℓ = Theχ2ℓ
		  τc = Theτc
		  Var χpℓ As Double = χ1ℓ + χ2ℓ
		  Var χmℓ As Double = χ1ℓ - χ2ℓ
		  
		  // Expansion constants
		  B6 = -1712/315
		  β3 = (113/48*onepδ*onepδ + 25/4*η)*χ1ℓ + (113/48*onemδ*onemδ + 25/4*η)*χ2ℓ
		  β5 = ((31319/4032-1159/96*η)*onepδ*onepδ + 809/84*η-281/8*η2)*χ1ℓ _
		  +((31319/4032-1159/96*η)*onemδ*onemδ + 809/84*η-281/8*η2)*χ2ℓ
		  β6 = (75/8*onepδ*onepδ + 151/6*η)*π*χ1ℓ + (75/8*onemδ*onemδ + 151/6*η)*χ2ℓ*π
		  β7 = ((130325/3024-796069/8064*η+100019/3456*η2)*onepδ*onepδ _
		  + 1195759/18144*η-257023/1008*η2 + 2903/32*η3)*χ1ℓ _
		  +((130325/3024-796069/8064*η+100019/3456*η2)*onemδ*onemδ _
		  + 1195759/18144*η-257023/1008*η2 + 2903/32*η3)*χ2ℓ
		  
		  A0 = 96/5*η
		  A2 = -743/336 - 11/4*η
		  A3 = 4*π - β3
		  A4 = 34103/18144+13661/2016*η + 59/18*η2
		  A5 = (-4159/672 + 189/8*η)*π - β5
		  A6 = 16447322263.0/139708800 + 16/3*π*π-856/105*Log(16)-1712/105*γE - β6 _
		  +(451/48*π*π - 56198689/217728)*η + 541/896*η2 - 5605/2592*η3
		  A7 = -4415/4032*π + 358675/6048*π*η + 91495/1512*π*η2 - β7
		  
		  C2 = -A2/6
		  C3 = -A3/5
		  C4 = -A4/4 + 5/24*A2*A2
		  C5 = -A5/3 + 3/5*A2*A3
		  C6 = -A6/2 - 3/4*B6 + 23/24*A4*A2 + 12/25*A3*A3 - 67/144*A2*A2*A2
		  C7 = -A7 + 2*A5*A2 + 2*A4*A3 - 3*A3*A2*A2
		  
		  P0 = -3/(5*A0)
		  P2 = -5/3*A2
		  P3 = -5/2*A3
		  P4 = -5*A4+5*A2*A2
		  P5 = 5*A5-10*A3*A2
		  P6 = 5*A6-15*B6-10*A4*A2-5*A3*A3+5*A2*A2*A2
		  P7 = 5/2*A7 - 5*A5*A2 - 5*A4*A3 + 15/2*A3*A2*A2
		  
		  // Calculate the initial value of V (also initial values of DVDδ, DVDτc, DVDχ1ℓ, DVDχ2ℓ,
		  // Ψorb, Ψops (power series for Ψorb), and DΨorbDV)
		  V0 = VAtTime(0)
		  
		  // Calculate the phase constant and its derivatives
		  Ψc = -Ψorb + Theλ0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function VAtTime(τ As Double) As Double
		  // Calculate zeta powers
		  Var ζ As Double = (5/(256*η*(τc-τ)))^(1/8)
		  Var ζ2 As Double = ζ*ζ
		  Var ζ3 As Double = ζ2*ζ
		  Var ζ4 As Double = ζ3*ζ
		  Var ζ5 As Double = ζ4*ζ
		  Var ζ6 As Double = ζ5*ζ
		  Var ζ7 As Double = ζ6*ζ
		  Var ζ9 As Double = ζ7*ζ2
		  
		  // Calculate powers of V
		  V = ζ*(1 + C2*ζ2+ C3*ζ3 + C4*ζ4 + C5*ζ5 + (C6-1.5*B6*Log(ζ))*ζ6 + C7*ζ7)
		  V2 = V*V
		  V3 = V2*V
		  V4 = V3*V
		  V5 = V4*V
		  V6 = V5*V
		  V7 = V6*V
		  V8 = V7*V
		  V9 = V8*V
		  
		  LogV = Log(V)
		  If V0 > 0 Then LogVIV0 = Log(V/V0) // If V0 is not yet set, then leave LogVIV0 at default value of 0
		  Ψorb = P0/V5*(1 + P2*V2 + P3*V3 + P4*V4 + P6*V6 + P7*V7 + (P5*V5 + 15*B6*V6)*LogVIV0)
		  Return V
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function VDotForLastV() As Double
		  Return A0/3*V9*(1.0 + A2*V2 + A3*V3+ A4*V4 + A5*V5 + (A6+B6*LogV)*V6 + A7*V7)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ΨorbForLastV() As Double
		  Return Ψc + P0/V5*(1 + P2*V2 + P3*V3 + P4*V4 + P6*V6 + P7*V7 + (P5*V5 + 15*B6*V6)*LogVIV0)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ΨtailForLastV() As Double
		  Return -6*V3*LogVIV0
		End Function
	#tag EndMethod


	#tag Property, Flags = &h21
		Private A0 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private A2 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private A3 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private A4 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private A5 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private A6 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private A7 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private B6 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private C2 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private C3 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private C4 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private C5 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private C6 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private C7 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private LogV As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private LogVIV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private P0 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private P2 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private P3 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private P4 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private P5 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private P6 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private P7 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private V As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private V0 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private V2 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private V3 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private V4 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private V5 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private V6 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private V7 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private V8 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private V9 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private β3 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private β5 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private β6 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private β7 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private γE As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private δ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private η As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private τc As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Ψc As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Ψorb As Double
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
