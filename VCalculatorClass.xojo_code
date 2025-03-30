#tag Class
Protected Class VCalculatorClass
	#tag Method, Flags = &h0
		Sub Constructor(Theτc As Double, Theδ As Double, Theχ1ℓ As Double, Theχ2ℓ As Double, Theλ0 As Double)
		  δ = Theδ
		  η = (1-δ*δ)*0.25
		  π = 3.14159265358979324
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
		  
		  // Derivatives of these constants with respect to  δ (see notes dated 3/28/25 and 3/29/25)
		  Dβ3Dδ = 19*δ/12*χpℓ + 113/24*χmℓ
		  Dβ5Dδ = 38299/4032*χmℓ + 19657/1008*δ*χpℓ + 1159/64*δ2*χmℓ +79/24*δ3
		  Dβ6Dδ = 75/4*π*χmℓ - δ/12*π*χpℓ
		  Dβ7Dδ = 372805/9216*χmℓ + 4348969/41472*χpℓ*δ + 4076281/32256*χmℓ*δ2 _
		  + 19331/432*χpℓ*δ3 + 500095/27648*χmℓ*δ4 + 10819/4608*χpℓ*δ5
		  
		  DA0Dδ = -48*δ/5
		  DA2Dδ = 11*δ/8
		  DA3Dδ = - Dβ3dδ
		  DA4Dδ = -59*δ*η/18 - 13661*δ/4032
		  DA5Dδ = 189*δ*π/16 - Dβ5Dδ
		  DA6Dδ = -Dβ6Dδ - 451/48*π*π*δ + 224884945/1741824*δ - 6157/24192*δ3 + 5605/27648*δ5
		  DA7Dδ = 60185/1344*π*δ + 91495/6048*π*δ3 - Dβ7Dδ
		  
		  DC2Dδ = -DA2Dδ/6
		  DC3Dδ = -DA3Dδ/5
		  DC4Dδ = -DA4Dδ/4 + 5/12*A2*DA2Dδ
		  DC5dδ = -DA5Dδ/3 + 3/5*DA2Dδ*A3 + 3/5*DA3Dδ*A2
		  DC6Dδ = -DA6Dδ/2 + 23/24*DA4Dδ*A2 _
		  + 23/24*DA2Dδ*A4 + 24/25*A3*DA3Dδ- 67/48*A2*A2*DA2Dδ
		  DC7dδ =  -DA7Dδ + 2*DA5Dδ*A2 + 2*DA2Dδ*A5 + 2*DA4Dδ*A3 + 2*DA3Dδ*A4 _
		  - 3*DA3Dδ*A2*A2 -6*A3*A2*DA2Dδ
		  
		  DP0Dδ = 3/(5*A0*A0)*DA0Dδ
		  DP2Dδ = -5/3*DA2Dδ
		  DP3Dδ = -5/2*DA3Dδ
		  DP4Dδ = -5*DA4Dδ + 10*A2*DA2Dδ
		  DP5Dδ = 5*DA5Dδ - 10*DA3Dδ*A2 - 10*A3*DA2Dδ
		  DP6Dδ = 5*DA6Dδ - 10*DA4Dδ*A2 - 10*A4*DA2Dδ - 10*A3*DA3Dδ + 15*A2*A2*DA2Dδ
		  DP7Dδ = 2.5*DA7Dδ - 5*DA5Dδ*A2 - 5*A5*DA2Dδ - 5*DA4Dδ*A3 - 5*A4*DA3Dδ _
		  -7.5*DA3Dδ*A2*A2 - 15*A3*A2*DA2Dδ
		  
		  // Derivatives of these constants with respect to χ1ℓ
		  Dβ3Dχ1ℓ = 25*η/4 + 113*onepδ*onepδ/48
		  Dβ5Dχ1ℓ = (31319/4032 - 1159/96*η)*onepδ*onepδ + 809/84*η - 281/8*η2
		  Dβ6Dχ1ℓ = π*(151/6*η + 75/8*onepδ*onepδ)
		  Dβ7Dχ1ℓ = (130325/3024-796069/8064*η + 100019/3456*η2)*onepδ*onepδ _
		  + 1195759/18144*η - 257023/1008*η2 + 2903/32*η3
		  
		  DA3Dχ1ℓ = -Dβ3Dχ1ℓ
		  DA5Dχ1ℓ = -Dβ5Dχ1ℓ
		  DA6Dχ1ℓ = -Dβ6Dχ1ℓ
		  DA7Dχ1ℓ = -Dβ7Dχ1ℓ
		  
		  DC3Dχ1ℓ = -DA3Dχ1ℓ/5
		  DC5Dχ1ℓ = -DA5Dχ2ℓ/3 + 3/5*A2*DA3Dχ1ℓ 
		  DC6Dχ1ℓ = -0.5*DA6Dχ1ℓ + 24/25*A3*DA3Dχ1ℓ 
		  DC7Dχ1ℓ = -DA7Dχ1ℓ + 2*DA5Dχ1ℓ*A2 + 2*A4*DA3Dχ1ℓ  - 3*DA3Dχ1ℓ*A2*A2
		  
		  DP3Dχ1ℓ = -2.5*DA3Dχ1ℓ
		  DP5Dχ1ℓ = 5*DA5Dχ1ℓ - 10*DA3Dχ1ℓ*A2
		  DP6Dχ1ℓ = 5*DA6Dχ1ℓ - 10*A3*DA3Dχ1ℓ
		  DP7Dχ1ℓ = 2.5*DA7Dχ1ℓ - 5*DA5Dχ1ℓ*A2 - 5*A4*DA3Dχ1ℓ + 7.5*DA3Dχ1ℓ*A2*A2
		  
		  // Derivatives with respect to  χ2ℓ
		  Dβ3Dχ2ℓ = 25*η/4 + 113*onemδ*onemδ/48
		  Dβ5Dχ2ℓ = (31319/4032 - 1159/96*η)*onemδ*onemδ + 809/84*η - 281/8*η2
		  Dβ6Dχ2ℓ = π*(151/6*η + 75/8*onemδ*onemδ)
		  Dβ7Dχ2ℓ = (130325/3024-796069/8064*η + 100019/3456*η2)*onemδ*onemδ _
		  + 1195759/18144*η - 257023/1008*η2 + 2903/32*η3
		  
		  DA3Dχ2ℓ  = -Dβ3Dχ2ℓ
		  DA5Dχ2ℓ = -Dβ5Dχ2ℓ
		  DA6Dχ2ℓ = -Dβ6Dχ2ℓ
		  DA7Dχ2ℓ = -Dβ7Dχ2ℓ
		  
		  DC3Dχ2ℓ = -DA3Dχ2ℓ/5
		  DC5Dχ2ℓ = -DA5Dχ2ℓ/3 + 3/5*A2*DA3Dχ2ℓ 
		  DC6Dχ2ℓ = -0.5*DA6Dχ1ℓ + 24/25*A3*DA3Dχ2ℓ 
		  DC7Dχ2ℓ = -DA7Dχ2ℓ + 2*DA5Dχ2ℓ*A2 + 2*A4*DA3Dχ2ℓ  - 3*DA3Dχ2ℓ*A2*A2
		  
		  DP3Dχ2ℓ = -2.5*DA3Dχ2ℓ
		  DP5Dχ2ℓ = 5*DA5Dχ2ℓ - 10*DA3Dχ2ℓ*A2
		  DP6Dχ2ℓ = 5*DA6Dχ2ℓ - 10*A3*DA3Dχ2ℓ
		  DP7Dχ2ℓ = 2.5*DA7Dχ2ℓ - 5*DA5Dχ2ℓ*A2 - 5*A4*DA3Dχ2ℓ + 7.5*DA3Dχ2ℓ*A2*A2
		  
		  // Calculate the initial value of V (also initial values of DVDδ, DVDτc, DVDχ1ℓ, DVDχ2ℓ,
		  // Ψorb, Ψops (power series for Ψorb), and DΨorbDV)
		  V0 = VAtTime(0)
		  
		  // Calculate the phase constant and its derivatives
		  Ψc = -Ψorb + Theλ0
		  DΨcDδ = -DP0Dδ/V5*Ψops - DΨorbDV*DVDδ _
		  - P0/V5*(DP2Dδ*V2 + DP3Dδ*V3 +DP4Dδ*V4 + DP6Dδ*V6 + DP7Dδ*V7)
		  DΨcDχ1ℓ = -DΨorbDV*DVDχ1ℓ - P0/V5*(DP3Dχ1ℓ*V3 + DP6Dχ1ℓ*V6 + DP7Dχ1ℓ*V7)
		  DΨcDχ2ℓ = -DΨorbDV*DVDχ2ℓ - P0/V5*(DP3Dχ2ℓ*V3 + DP6Dχ2ℓ*V6 + DP7Dχ2ℓ*V7)
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DVDotDδForLastV() As Double
		  Return DA0Dδ/3*V9*(1 + A2*V2 + A3*V3 + A4*V4 + A5*V5 + (A6 + B6*Log(V))*V6 + A7*V7) _
		  + A0/3*V9*(DA2Dδ*V2 + DA3Dδ*V3 + DA4Dδ*V4 + DA5Dδ*V5 + DA6Dδ*V6 + DA7Dδ*V7) _
		  + DVDotDV*DVDδ
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DVDotDτcForLastV() As Double
		  Return DVDotDV*DVDτc
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DVDotDχ1ℓForLastV() As Double
		  Return A0/3*V9*(DA3Dχ1ℓ*V3 + DA5Dχ1ℓ*V5 + DA6Dχ1ℓ*V6 + DA7Dχ1ℓ*V7) + DVDotDV*DVDχ1ℓ
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DVDotDχ2ℓForLastV() As Double
		  Return A0/3*V9*(DA3Dχ2ℓ*V3 + DA5Dχ2ℓ*V5 + DA6Dχ2ℓ*V6 + DA7Dχ2ℓ*V7) + DVDotDV*DVDχ2ℓ
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DVDδForLastV() As Double
		  Return DVDδ
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DVDτcForLastV() As Double
		  Return DVDτc
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DVDχ1ℓForLastV() As Double
		  Return DVDχ1ℓ
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DVDχ2ℓForLastV() As Double
		  Return DVDχ2ℓ
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DΨorbDδForLastV() As Double
		  Return DΨcDδ + DP0Dδ/V5*Ψops + DΨorbDV*DVDδ _
		  + P0/V5*(DP2Dδ*V2 + DP3Dδ*V3 + DP4Dδ*V4 + DP5Dδ*V5*LogVIV0 + DP6Dδ*V6 + DP7Dδ*V7)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DΨorbDτcForLastV() As Double
		  Return DΨcDτc + DΨorbDV*DVDτc
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DΨorbDχ1ℓForLastV() As Double
		  Return DΨcDχ1ℓ + DΨorbDV*DVDχ1ℓ _
		  + P0/V5*(DP3Dχ1ℓ*V3 + DP5Dχ1ℓ*V5*LogVIV0 + DP6Dχ1ℓ*V6 + DP7Dχ1ℓ*V7)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DΨorbDχ2ℓForLastV() As Double
		  Return DΨcDχ2ℓ + DΨorbDV*DVDχ2ℓ _
		  + P0/V5*(DP3Dχ2ℓ*V3 + DP5Dχ2ℓ*V5*LogVIV0 + DP6Dχ2ℓ*V6 + DP7Dχ2ℓ*V7)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DΨtailDδForLastV() As Double
		  Return DΨtailDV*DVDδ
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DΨtailDτcForLastV() As Double
		  Return DΨtailDV*DVDτc
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DΨtailDχ1ℓForLastV() As Double
		  Return DΨtailDV*DVDχ1ℓ
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DΨtailDχ2ℓForLastV() As Double
		  Return DΨtailDV*DVDχ2ℓ
		End Function
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
		  
		  // Calculate derivatives of V
		  Var dζdτc As Double = -32/5*η*ζ9
		  Var dζdδ As Double = δ*ζ/(16*η)
		  Var dVdζ As Double = 1.0 +3*C2*ζ2 + 4*C3*ζ3 + 5*C4*ζ4 + 6*C5*ζ5 _
		  + 7*C6*ζ6*Dζdδ -10.5*B6*Log(ζ)*ζ6 -1.5*B6*ζ5+ 8*C7*ζ7
		  DVDδ = DC2Dδ*ζ3+ DC3Dδ*ζ4 + DC4Dδ*ζ5 + DC5Dδ*ζ6 + DC6Dδ*ζ7 + DC7Dδ*ζ7*ζ + dVdζ*dζdδ
		  DVDτc = dVdζ*dζdτc
		  DVDχ1ℓ = ζ*(DC3dχ1ℓ*ζ3 + DC5dχ1ℓ*ζ5 + DC6dχ1ℓ*ζ6 + DC7dχ1ℓ*ζ7)
		  DVDχ2ℓ = ζ*(DC3dχ2ℓ*ζ3 + DC5dχ2ℓ*ζ5 + DC6dχ2ℓ*ζ6 + DC7dχ2ℓ*ζ7)
		  LogV = Log(V)
		  LogVIV0 = Log(V/V0)
		  DΨtailDV = -6*V*V*(3*LogVIV0-1.0)
		  DVDotDV = A0/3*V8*(9 + 11*A2*V2 + 12*A3*V3 + 13*A4*V4 + 14*A5*V5 _
		  + 15*A6*V6 + 15*B6*Log(V)*V6 + B6*V5 + 16*A7*V7)
		  Ψops = 1 + P2*V2 + P3*V3 + P4*V4 + P6*V6 + P7*V7 + (P5*V5 + 15*B6*V6)*LogVIV0
		  Ψorb = P0/V5*Ψops
		  DΨorbDV = P0/V6*(-5 - 3*P2*V2 - 2*P3*V3 - P4*V4 + P6*V6 + 2*P7*V7 _
		  + P5*V5 + 15*B6*V6*(LogVIV0 + 1))
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
		Private DA0Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA2Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA3Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA3Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA3Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA4Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA5Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA5Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA5Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA6Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA6Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA6Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA7Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA7Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DA7Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC2Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC3Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC3Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC3Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC4Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC5Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC5Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC5Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC6Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC6Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC6Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC7Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC7Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC7Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP0Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP2Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP3Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP3Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP3Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP4Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP5Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP5Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP5Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP6Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP6Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP6Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP7Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP7Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DP7Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DVDotDV As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DVDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DVDτc As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DVDχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DVDχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ3Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ3Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ3Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ5Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ5Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ5Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ6Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ6Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ6Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ7Dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ7Dχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dβ7Dχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DΨcDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DΨcDτc As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DΨcDχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DΨcDχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DΨorbDV As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DΨtailDV As Double
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
		Private π As Double
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
		Private Ψops As Double
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
