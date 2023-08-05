#tag Class
Protected Class EvolverClass
	#tag Method, Flags = &h0
		Sub CalcDataAtMainStep(StepRatio As Double)
		  If StepRatio = 0.0 Then // if we are getting information about the current step,
		    VMN = VN
		    CosιMN = CosιN
		    αMN = αN
		    ΨrMN = ΨrN
		    χaMN.X = χaN.X
		    χaMN.Y = χaN.Y
		    χaMN.Z = χaN.Z
		    χsMN.X = χsN.X
		    χsMN.Y = χsN.Y
		    χsMN.Z = χsN.Z
		  Else // if we are interpolating between the current step and a future step,
		    // Get the interpolated values and return them
		    Var oneMinusRatio As Double = 1.0 - StepRatio
		    VMN = oneMinusRatio*VN + StepRatio*VP
		    CosιMN = oneMinusRatio*CosιN  + StepRatio*CosιP
		    αMN = oneMinusRatio*αN + StepRatio*αP
		    ΨrMN = oneMinusRatio*ΨrN +  + StepRatio*ΨrP
		    χaMN.X = oneMinusRatio*χaN.X + StepRatio*χaP.X
		    χaMN.Y = oneMinusRatio*χaN.Y + StepRatio*χaP.Y
		    χaMN.Z = oneMinusRatio*χaN.Z + StepRatio*χaP.Z
		    χsMN.X = oneMinusRatio*χsN.X + StepRatio*χsP.X
		    χsMN.Y = oneMinusRatio*χsN.Y + StepRatio*χsP.Y
		    χsMN.Z = oneMinusRatio*χsN.Z + StepRatio*χsP.Z
		  End If
		  
		  // Calculate trig functions based on iota
		  C(2) = CosιMN
		  S(2) = Sqrt(1.0 - CosιMN*CosιMN)
		  Var c1 As Double = Sqrt(0.5*(1+C(2)))
		  Var s1 As Double = Sqrt(0.5*(1-C(2)))
		  S(1) = s1
		  C(1) = c1
		  C(3) = CosιMN*c1 - S(2)*s1
		  S(3) = S(2)*c1 + CosιMN*s1
		  C(4) = 2*CosιMN*CosιMN-1.0
		  S(4) = 2*C(2)*S(2)
		  C(5) = C(4)*c1 - S(4)*s1
		  S(5) = S(4)*c1 + C(4)*s1
		  C(6) = C(5)*c1 - S(5)*s1
		  S(6) = S(5)*c1 + C(5)*s1
		  C(7) = C(6)*c1 - S(6)*s1
		  S(7) = S(6)*c1 + C(6)*s1
		  C(8) = C(7)*c1 - S(7)*s1
		  S(8) = S(7)*c1 + C(7)*s1
		  C(9) = C(8)*c1 - S(8)*s1
		  S(9) = S(8)*c1 + C(8)*s1
		  C(10) = C(9)*c1 - S(9)*s1
		  S(10) = S(9)*c1 + C(9)*s1
		  
		  // Calculate all the amplitudes
		  CalculateAmplitudes
		  
		  If IsBaseCase Then
		    // Calculate basic angle multiples for α and Ψr
		    Var c01 As Double = Cos(ΨrMN)
		    Var s01 As Double = Sin(ΨrMN)
		    Var c02 As Double = c01*c01 - s01*s01
		    Var s02 As Double = 2*c01*s01
		    Var c03 As Double = c02*c01 - s02*s01
		    Var s03 As Double = s02*c01 + c02*s01
		    Var c04 As Double = c03*c01 - s03*s01
		    Var s04 As Double = s03*c01 + c03*s01
		    Var c05 As Double = c04*c01 - s04*s01
		    Var s05 As Double = s04*c01 + c04*s01
		    
		    Var c10 As Double = Cos(αMN)
		    Var s10 As Double = Sin(αMN)
		    Var c20 As Double = c10*c10 - s10*s10
		    Var s20 As Double = 2*s10*c10
		    Var c30 As Double = c20*c10 - s20*s10
		    Var s30 As Double = s20*c10 - c20*s10
		    Var c40 As Double = c30*c10 - s30*s10
		    Var s40 As Double = s30*c10 - c30*s10
		    Var c50 As Double = c40*c10 - s40*s10
		    Var s50 As Double = s40*c10 - c40*s10
		    
		    // Calculate the oscillating factors
		    CalculateWaveFactors
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateAmplitudes()
		  // Now calculate all wave amplitudes
		  
		  // Amplitude factors for H0P
		  Var c1p3 As Double = C(1)*C(1)*C(1)
		  Var s1p3 As Double = S(1)*S(1)*S(1)
		  A(0) = (-1.5-0.5*Cβ(2))*c1p3*C(1)
		  A(1) = -2*Sβ(2)*c1p3*S(1)
		  A(2) =  2.0*s1p3*Sβ(2)*C(1)
		  A(3) = (-1.5-0.5*Cβ(2))*s1p3*S(1)
		  A(4) = -1.5*Sβ(2)*Sβ(2)*S(2)*S(2)
		  
		  // Amplitude factors for H0X
		  A(5) = 4*C(1)*Sβ(1)*s1p3
		  A(6) = -2*Cβ(1)*s1p3*S(1)
		  A(7) =  -4*c1p3*Sβ(1)*S(1)
		  A(8) = -2*Cβ(1)*c1p3*C(1)
		  
		  // Amplitude factors for H1P
		  '1 return Parameters.δ*(AP.C1^6)*(-45/32*AP.Sβ-9/32*AP.S3β)
		  '2 return Parameters.δ*(AP.C1^2)*(-175/256*AP.Sβ+AP.C2*(87/64*AP.Sβ-5/64*AP.S3β)+AP.C4*(-5/256*AP.Sβ+15/256*AP.S3β)+13/256*AP.S3β)
		  '3 return Parameters.δ*(AP.S1^2)*(175/256*AP.Sβ+AP.C2*(87/64*AP.Sβ-5/64*AP.S3β)+AP.C4*(5/256*AP.Sβ-15/256*AP.S3β)-13/256*AP.S3β)
		  '4 return Parameters.δ*(AP.C1^4)*(AP.S1^2)*(-5/32*AP.Sβ-AP.S3β/32)
		  '5 return Parameters.δ*(AP.C1^4)*(AP.S1^2)*(-45/32*AP.Sβ+AP.S3β*135/32)
		  '6 return Parameters.δ*(AP.C1^2)*(AP.S1^4)*(45/32*AP.Sβ-AP.S3β*135/32)
		  '7 return Parameters.δ*(AP.C1^2)*(AP.S1^4)*(5/32*AP.Sβ+AP.S3β/32)
		  '8 return Parameters.δ*(AP.S1^6)*AP.Sβ*(27/16+9/16*AP.C2β)
		  '9 return Parameters.δ*(45/16)*(AP.S2^3)*(AP.Cβ)*(AP.Sβ^2)
		  '10 return Parameters.δ*((-85/256*AP.Cβ-AP.Cβ*AP.C2β/128-AP.Cβ*AP.C2β*AP.C2/32-3/128*AP.Cβ*AP.C2β*AP.C4)*AP.S2-11/64*AP.Cβ*AP.S4-AP.Cβ*AP.S6/256)
		  '11 return Parameters.δ*((45/256*AP.Cβ+AP.Cβ*AP.C2β*81/128+AP.Cβ*AP.C2β*AP.C2*27/32+27/128*AP.Cβ*AP.C2β*AP.C4)*AP.S2+9/64*AP.Cβ*AP.S4+AP.Cβ*AP.S6*9/256)
		  '12 return Parameters.δ*((-85/256*AP.Cβ+AP.Cβ*AP.C2β*1/256)*AP.S2+(11/64*AP.Cβ+1/64*AP.Cβ*AP.C2β)*AP.S4-(1/256*AP.Cβ+3/256*AP.Cβ*AP.C2β)*AP.S6)
		  '13 return Parameters.δ*((45/256*AP.Cβ+AP.Cβ*AP.C2β*135/256)*AP.S2-(9/64*AP.Cβ+27/64*AP.Cβ*AP.C2β)*AP.S4+(9/256*AP.Cβ+27/256*AP.Cβ*AP.C2β)*AP.S6)
		  '14 return Parameters.δ*(1/64*AP.Cβ*AP.Sβ^2*AP.S2+5/64*AP.Cβ*AP.Sβ*AP.S6)
		  
		  // Ampitude factors for H1X
		  '1 return Parameters.δ*(-45/8)*AP.C1^2*AP.S2β*AP.S1^4
		  '2 return Parameters.δ*(9/2)*AP.C2β*AP.C1*AP.S1^5
		  '3 return Parameters.δ*(9/8)*AP.S2β*AP.S1^6
		  '4 return Parameters.δ*(-1/64*AP.Cβ*AP.Sβ+43/128*AP.Cβ*AP.C2*AP.Sβ-23/128*AP.C4*AP.S2β+5/256*Ap.C6*AP.S2β)
		  '5 return Parameters.δ*((-1-AP.C2β/4)*AP.C1+1/4*AP.C2β*AP.C1*AP.C2)*AP.S1^3 
		  '6 return Parameters.δ*(1/8)*AP.C1^2*AP.S2β*AP.S1^4
		  '7 return Parameters.δ*(1/2)*AP.Sβ^2*AP.S4
		  '8 return Parameters.δ*(AP.Cβ*AP.Sβ/64+43/128*AP.Cβ*AP.C2*AP.Sβ+23/128*AP.C4*AP.S2β+5/256*AP.C6*AP.S2β)
		  '9 return Parameters.δ*AP.S1*((-1-AP.C2β/4)*AP.C1^3-1/4*AP.C2β*AP.C1^3*AP.C2)
		  '10 return Parameters.δ*(-1/8)*(AP.C1^4*AP.S2β*AP.S1^2)
		  '11 return Parameters.δ*(45/8)*(AP.C1^4)*(AP.S2β)*(AP.S1^2)
		  '12 return Parameters.δ*(9/2)*AP.C2β*AP.C1^5*AP.S1
		  '13 return Parameters.δ*(-9/8)*AP.C1^6*AP.S2β
		  
		  // Amplitude factors for H2P
		  '1 return (59/16+5/2*AP.C2β-3/16*AP.C4β+(5/24-11/6*AP.C2β+7/24*AP.C4β)*AP.C2-(5/48+1/12*AP.C2β+7/48*AP.C4β)*AP.C4)*AP.C1^4+
		  '(-25/16-13/3*AP.C2β+9/16*AP.C4β+(-5/8+11/2*AP.C2β-7/8*AP.C4β)*AP.C2+(5/16+1/4*AP.C2β+7/16*AP.C4β)*AP.C4)*AP.η*AP.C1^4
		  '2 return (6+2*AP.C2β)*AP.η*AP.C1^8*AP.Sβ^2-(2+2/3*AP.C2β)*AP.C1^8*AP.Sβ^2
		  '3 return 32*(1/3-AP.η)*AP.Cβ^3*AP.C1^7*AP.Sβ*AP.S1
		  '4 return ((1/6*AP.C2β-5/6)*AP.S2β-2/3*AP.Cβ^2*AP.C2*AP.S2β+AP.η*((5/2-1/2*AP.C2β)*AP.S2β+2*AP.Cβ^2*AP.C2*AP.S2β))*AP.C1^5*AP.S1
		  '5 return (-(10/3+8/3*AP.C2β+14/3*AP.C4β)+AP.η*(10+8*AP.C2β+14*AP.C4β))*AP.C1^6*AP.S1^2
		  '6 return 1/2*(-(1+1/3*AP.C2β)+AP.η*(3+AP.C2β))*AP.C1^6*AP.Sβ^2*AP.S1^2
		  '7 return (8/3-56/3*AP.C2β+AP.η*(56*AP.C2β-8))*AP.C1^5*AP.S2β*AP.S1^3
		  '8 return AP.η*(AP.C1*(16/3*AP.S2β+31/4*AP.C2*AP.S2β+1/4*AP.C4*AP.S2β-19/16*AP.S4β)-7/8*AP.C3*AP.S4β-7/16*AP.C5*AP.S4β)*AP.S1^3
		  '+(AP.C1*(-6*AP.S2β-31/12*AP.C2*AP.S2β-1/12*AP.C4*AP.S2β+19/48*AP.S4β)+7/24*AP.C3*AP.S4β+7/48*AP.C5*AP.S4β)*AP.S1^3
		  '9 return (59/16+5/2*AP.C2β-3/16*AP.C4β-(5/24-11/6*AP.C2β+7/24*AP.C4β)*AP.C2-(5/48+1/12*AP.C2β+7/48*AP.C4β)*AP.C4)*AP.S1^4
		  '+AP.η*(-25/16-13/3*AP.C2β+9/16*AP.C4β+(5/8-11/2*AP.C2β+7/8*AP.C4β)*AP.C2+(5/16+1/4*AP.C2β+7/16*AP.C4β)*AP.C4)*AP.S1^4
		  '10 return (56/3*AP.C2β-8/3+AP.η*(8-56*AP.C2β))*AP.C1^3*AP.S2β*AP.S1^5
		  '11 return ((5/6-1/6*AP.C2β)*AP.S2β-2/3*AP.Cβ^2*AP.C2*AP.S2β+AP.η*((-5/2+1/2*AP.C2β)*AP.S2β+2*AP.Cβ^2*AP.C2*AP.S2β))*AP.C1*AP.S1^5
		  '12 return (-(10/3+8/3*AP.C2β+14/3*AP.C4β)+AP.η*(10+8*AP.C2β+14*AP.C4β))*AP.C1^2*AP.S1^6
		  '13 return (-(1/2+1/6*AP.C2β)+AP.η*(3/2+1/2*AP.C2β))*AP.C1^2*AP.Sβ^2*AP.S1^6
		  '14 return 32*(AP.η-1/3)*AP.Cβ^3*AP.C1*AP.Sβ*AP.S1^7
		  '15 return (AP.η*(6+2*AP.C2β)-(2+2/3*AP.C2β))*AP.Sβ^2*AP.S1^8
		  '16 return 1/32*(1/3*(349-25*AP.C2β)*AP.Sβ^2-(25+35*AP.C2β)*AP.C4*AP.Sβ^2)+AP.η*((25*AP.C2β-45)*AP.Sβ^2+(25+35*AP.C2β)*AP.C4*AP.Sβ^2)*AP.S2^2
		  '17 return 1/4*(AP.η*(25+35*AP.C2β)-1/3*(25-35*AP.C2β))*AP.Sβ^2*AP.S2^4
		  '18 return AP.C1^3*(6*AP.S2β-31/12*AP.C2*AP.S2β+1/12*AP.C4*AP.S2β-19/48*AP.S4β)*AP.S1+7/24*AP.C1^3*AP.S4β*AP.S3-7/48*AP.C1^3*AP.S4β*AP.S5
		  '+AP.η*(AP.C1^3*(-16/3*AP.S2β+31/4*AP.C2*AP.S2β-1/4*AP.C4*AP.S2β+19/16*AP.S4β)*AP.S1-7/8*AP.C1^3*AP.S4β*AP.S3+7/16*AP.C1^3*AP.S4β*AP.S5)
		  
		  '1 Return AP.χax*AP.Cβ*AP.C1^2-AP.χaz*AP.C1^2*AP.Sβ
		  '2 Return AP.χax*(AP.Cβ/2-AP.Cβ*AP.C2/2)-AP.χaz*AP.Sβ*AP.S1^2
		  '3 Return -AP.χay*AP.Cβ*AP.S1^2
		  '4 Return -AP.χay*AP.Sβ*AP.S2
		  '5 Return -AP.χay*AP.Cβ*AP.C1^2
		  '6 Return Parameters.δ*(AP.χsx*AP.Cβ*AP.C1^2-AP.χsz*AP.C1^2*AP.Sβ)
		  '7 Return Parameters.δ*(AP.χsx*(AP.Cβ/2-AP.Cβ*AP.C2/2)-AP.χsz*AP.Sβ*AP.S1^2)
		  '8 Return -Parameters.δ*(AP.χsy*AP.Cβ*AP.S1^2)
		  '9 Return Parameters.δ*(AP.χsy*AP.Sβ*AP.S2)
		  '10 Return -Parameters.δ*(AP.χsy*AP.Cβ*AP.S1^2)
		  
		  // Ampitude factors for H2X
		  '1 Return (4*AP.Sβ+28/3*AP.S3β-AP.η*(12*AP.Sβ+28*AP.S3β))*AP.C1^3*AP.S1^5
		  '2 Return (AP.η*(4*AP.Cβ+28*AP.C3β-(4/3*AP.Cβ+28/3*AP.C3β)))*AP.C1^2*AP.S1^6
		  '3 Return ((4/3*AP.Sβ-4*AP.S3β)+AP.η*(-4*AP.Sβ+12*AP.S3β))*AP.C1*AP.S1^7
		  '4 Return (8*AP.η-8/3)*AP.Cβ*AP.Sβ*AP.S1^8
		  '5 Return AP.C1*(-79/8*AP.Sβ+AP.C2*(3/4*AP.Sβ-19/12*AP.S3β)+AP.C4*(AP.Sβ/8+7/24*AP.S3β)-3/8*AP.S3β)*AP.S1^3
		  '+AP.η*AP.C1*(103/24*AP.Sβ-AP.C4*(3/8*AP.Sβ+7/8*AP.S3β)+9/8*AP.S3β+AP.C2*(-9/4*AP.Sβ+19/4*AP.S3β))*AP.S1^3
		  '6 Return (47/8*AP.Cβ+AP.C3β/8+(7/6*AP.Cβ+AP.C3β/6)*AP.C2-(AP.Cβ/24+7/24*AP.C3β)*AP.C4+AP.η*(-119/24*AP.Cβ
		  '-3/8*AP.C3β-(7/2*AP.Cβ+AP.C3β/2)*AP.C2+(AP.Cβ/8+7/8*AP.C3β)*AP.C4))*AP.S1^4
		  '7 Return (4/3*AP.Sβ-(1/3+AP.C2β)*AP.C2*AP.Sβ+AP.η*(-4*AP.Sβ+(1+3*AP.C2β)*AP.C2*AP.Sβ))*AP.C1*AP.S1^5
		  '8 Return (2*AP.η-2/3)*AP.Cβ*AP.C1^2*AP.Sβ^2*AP.S1^6
		  '9 Return (15/2*AP.η-5/2)*AP.Cβ*AP.C2*AP.Sβ^2*AP.S2^2
		  '10 Return AP.C1^3*(79/8*AP.Sβ+AP.C2*(3/4*AP.Sβ-19/12*AP.S3β)-AP.C4*(AP.Sβ/8+7/24*AP.S3β)+3/8*AP.S3β)*AP.S1
		  '+AP.η*AP.C1^3*(-103/24*AP.Sβ+AP.C4*(3/8*AP.Sβ+7/8*AP.S3β)-9/8*AP.S3β+AP.C2*(-9/4*AP.Sβ+19/4*AP.S3β))*AP.S1
		  '11 Return AP.C1^4*(47/8*AP.Cβ+AP.C3β/8-(7/6*AP.Cβ+AP.C3β/6)*AP.C2-(AP.Cβ/24+7/24*AP.C3β)*AP.C4)+AP.η*AP.C1^4*(-119/24*AP.Cβ
		  '-3/8*AP.C3β+(7/2*AP.Cβ+AP.C3β/2)*AP.C2+(AP.Cβ/8+7/8*AP.C3β)*AP.C4)
		  '12 Return (-4/3*AP.Sβ-(1/3+AP.C2β)*AP.C2*AP.Sβ+AP.η*(4*AP.Sβ+(1+3*AP.C2β)*AP.C2*AP.Sβ))*AP.C1^5*AP.S1
		  '13 Return (2*AP.η-2/3)*AP.Cβ*AP.C1^6*AP.Sβ^2*AP.S1^2
		  '14 Return (AP.η*(12*AP.Sβ+28*AP.S3β)-(4*AP.Sβ+28/3*AP.S3β))*AP.C1^5*AP.S1^3
		  '15 Return (AP.η*(4*AP.Cβ+28*AP.C3β)-(4/3*AP.Cβ+28/3*AP.C3β))*AP.C1^6*AP.S1^2
		  '16 Return (8/3+8*AP.C2β-AP.η*(8+24*AP.C2β))*AP.C1^7*AP.Sβ*AP.S1
		  '17 Return (8*AP.η-8/3)*AP.Cβ*AP.C1^8*AP.Sβ^2
		  
		  '1 Return AP.χay*(1/2+AP.C2/2)
		  '2 Return AP.χay*AP.S1^2
		  '3 Return AP.χax*(AP.Cβ^2/2-AP.Cβ^2*AP.C2/2)+AP.χaz*(-AP.Cβ*AP.Sβ/2+AP.Cβ*AP.C2*AP.Sβ/2)
		  '4 Return AP.χax*AP.Cβ*AP.Sβ*AP.S2-AP.χaz*AP.Sβ^2*AP.S2
		  '5 Return AP.χax*(AP.Cβ^2/2+AP.Cβ^2*AP.C2/2)+AP.χaz*(-AP.Cβ*AP.Sβ/2-AP.Cβ*AP.C2*AP.Sβ/2)
		  '6 Return Parameters.δ*(AP.χsy*(1/2+AP.C2/2))
		  '7 Return Parameters.δ*(AP.χsy*AP.S1^2)
		  '8 Return Parameters.δ*(AP.χsx*(AP.Cβ^2/2-AP.Cβ^2*AP.C2/2)+AP.χsz*(-AP.Cβ*AP.Sβ/2+AP.Cβ*AP.C2*AP.Sβ/2))
		  '9 Return Parameters.δ*(AP.χsx*AP.Cβ*AP.Sβ*AP.S2-AP.χsz*AP.Sβ^2*AP.S2)
		  '10 Return Parameters.δ*(AP.χsx*(AP.Cβ^2/2+AP.Cβ^2*AP.C2/2)+AP.χsz*(-AP.Cβ*AP.Sβ/2-AP.Cβ*AP.C2*AP.Sβ/2))
		  
		  // Ampitude factors for H3P
		  '1 Return -(3*π+π*AP.C2β)*AP.C1^4
		  '2 Return -4*π*AP.C1^3*AP.S2β*AP.S1
		  '3 Return 4*π*AP.C1*AP.S2β*AP.S1^3
		  '4 Return -(3*π+π*AP.C2β)*AP.S1^4
		  '5 Return -3*π*AP.Sβ^2*AP.S2^2
		  '6 Return Parameters.δ*(AP.η*(625/128+625/384*AP.C2β)-(625/256+625/768*AP.C2β))*AP.C1^10*AP.Sβ^3
		  '7 Return Parameters.δ*(AP.η*AP.C1^2*(-7449/16384*AP.Sβ-331/32768*AP.S3β+AP.C4*(337/12288*AP.Sβ-47/8192*AP.S3β-21/8192*AP.S5β)
		  '+AP.C8*(7/49152*AP.Sβ+7/32768*AP.S3β-35/32768*AP.S5β)+AP.C6*(-59/6144*AP.Cβ-91/4096*AP.S3β+7/4096*AP.S5β)+AP.C2*(1873/2048*AP.Sβ
		  '+19/4096*AP.S3β+35/12288*AP.S5β)-155/98304*AP.S5β)+AP.C1^2*(43723/98304*AP.Sβ-9653/65536*AP.S3β+AP.C2*(-10675/12288*AP.Sβ
		  '+1901/8192*AP.S3β-35/24576*AP.S5β)+AP.C6*(59/12288*AP.Sβ+91/8192*AP.S3β-7/8192*AP.S5β)+AP.C8*(-7/98304*AP.Sβ-7/65536*AP.S3β
		  '+35/65536*AP.S5β)+AP.C4*(1103/24576*AP.Sβ-2833/16384*AP.S3β+21/16384*AP.S5β)+155/196608*AP.S5β))
		  '8 Return Parameters.δ*(AP.C1^6*(39249/8192*AP.Sβ+38331/16384*AP.S3β-AP.C4*(1701/8192*AP.Sβ+3159/16384*AP.S3β+3645/16384*AP.S5β)
		  '+AP.C2*(2403/2048*AP.Sβ-6399/4096*AP.S3β+2187/4096*AP.S5β)-5751/16384*AP.S5β)+AP.η*AP.C1^6*(-4689/4096*AP.Sβ-24507/8192*AP.S3β
		  '+AP.C2*(-2403/1024*AP.Sβ+6399/2048*AP.S3β-2187/2048*AP.S5β)+AP.C4*(1701/4096*AP.Sβ+3159/8192*AP.S3β+3645/8192*AP.S5β)+5751/8192*AP.S5β))
		  '9 Return Parameters.δ*((11875/768*AP.Cβ+3125/768*AP.C3β-AP.η*(11875/384*AP.Cβ+3125/384*AP.C3β))*AP.C1^9*AP.Sβ^2*AP.S1)
		  '10 Return Parameters.δ*(((-351/256*AP.Cβ+243/256*AP.Cβ*AP.C2β)*AP.Sβ^2-(567/256*AP.Cβ+405/256*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2
		  '+AP.η*((351/128*AP.Cβ-243/128*AP.Cβ*AP.C2β)*AP.Sβ^2+(567/128*AP.Cβ+405/128*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2))*AP.C1^7*AP.S1)
		  '11 Return Parameters.δ*((AP.η*(243/128+81/128*AP.C2β)-(243/256+81/256*AP.C2β))*AP.C1^8*AP.Sβ^3*AP.S1^2)
		  '12 Return Parameters.δ*((-43723/98304*AP.Sβ+9653/65536*AP.S3β+AP.C2*(-10675/12288*AP.Sβ+1901/8192*AP.S3β-35/24576*AP.S5β)
		  '+AP.C4*(-1103/24576*AP.Sβ+2833/16384*AP.S3β-21/16384*AP.S5β)+AP.C6*(59/12288*AP.Sβ+91/8192*AP.S3β-7/8192*AP.S5β)
		  '+AP.C8*(7/98304*AP.Sβ+7/65536*AP.S3β-35/65536*AP.S5β)-155/196608*AP.S5β)*AP.S1^2+AP.η*(7449/16384*AP.Sβ+331/32768*AP.S3β
		  '+AP.C8*(-7/49152*AP.Sβ-7/32768*AP.S3β+35/32768*AP.S5β)+AP.C6*(-59/6144*AP.Sβ-91/4096*AP.S3β+7/4096*AP.S5β)+AP.C4*(-337/12288*AP.Sβ
		  '+47/8192*AP.S3β+21/8192*AP.S5β)+AP.C2*(1873/2048*AP.Sβ+19/4096*AP.S3β+35/12288*AP.S5β)+155/98304*AP.S5β)*AP.S1^2)
		  '13 Return Parameters.δ*(AP.C1^4*(1675/4096*AP.Sβ+825/8192*AP.S3β-AP.C4*(7/4096*AP.Sβ+13/8192*AP.S3β+15/8192*AP.S5β)
		  '+AP.C2*(27/1024*AP.Sβ-151/2048*AP.S3β+3/2048*AP.S5β)-13/8192*AP.S5β)*AP.S1^2+AP.η*AP.C1^4*(245/2048*AP.Sβ-57/4096*AP.S3β
		  '+AP.C2*(-27/512*AP.Sβ+151/1024*AP.S3β-3/1024*AP.S5β)+AP.C4*(7/2048*AP.Sβ+13/4096*AP.S3β+15/4096*AP.S5β)+13/4096*AP.S5β)*AP.S1^2)
		  '14 Return Parameters.δ*((AP.η*(4375/512*AP.Sβ+8125/1024*AP.S3β+9375/1024*AP.S5β)-(4375/1024*AP.Sβ+8125/2048*AP.S3β+9375/2048*AP.S5β))*AP.C1^8*AP.S1^2)
		  '15 Return Parameters.δ*(AP.C1^4*(20475/4096*AP.Sβ-149391/8192*AP.S3β+AP.C2*(2187/1024*AP.Sβ+10017/2048*AP.S3β-1701/2048*AP.S5β)
		  '+7371/8192*AP.S5β+AP.C4*(-567/4096*AP.Sβ-1701/8192*AP.S3β+8505/8192*AP.S5β))*AP.S1^2+AP.η*AP.C1^4*(-3195/2048*AP.Sβ+45711/4096*AP.S3β
		  '+AP.C4*(567/2048*AP.Sβ+1701/4096*AP.S3β-8505/4096*AP.S5β)-7371/4096*Ap.S5β+AP.C2*(-2187/512*AP.Sβ-10017/1024*AP.S3β+1701/1024*AP.S5β))*AP.S1^2)
		  '16 Return Parameters.δ*((4375/384*AP.Cβ+625/256*AP.C3β+3125/256*AP.C5β-AP.η*(4375/192*AP.Cβ+625/128*AP.C3β+3125/128*AP.C5β))*AP.C1^7*AP.S1^3)
		  '17 Return Parameters.δ*(AP.C1^5*((-37/384*AP.Cβ+1/384*AP.Cβ*AP.C2β)*AP.Sβ^2-(7/384*AP.Cβ+5/384*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^3
		  '+AP.η*AP.C1^5*((37/192*AP.Cβ-1/192*AP.Cβ*AP.C2β)*AP.Sβ^2+(7/192*AP.Cβ+5/192*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^3)
		  '18 Return Parameters.δ*((AP.η*(1/64+1/192*AP.C2β)-(1/128+1/384*AP.C2β))*AP.C1^6*AP.Sβ^3*AP.S1^4)
		  '19 Return Parameters.δ*(AP.η*AP.C1^2*(-245/2048*AP.Sβ+57/4096*AP.S3β-AP.C4*(7/2048*AP.Sβ+13/4096*AP.S3β+15/4096*AP.S5β)
		  '+AP.C2*(-27/512*AP.Sβ+151/1024*AP.S3β-3/1024*AP.S5β)-13/4096*AP.S5β)*AP.S1^4+AP.C1^2*(-1675/4096*AP.Sβ-825/8192*AP.S3β+AP.C2*(27/1024*AP.Sβ
		  '-151/2048*AP.S3β+3/2048*AP.S5β)+AP.C4/4096*(7*AP.Sβ+13*AP.S3β+15*AP.S5β)*AP.S1^4))
		  '20 Return Parameters.δ*(4375*AP.η*AP.C1^6*(1/768*AP.Sβ+1/512*AP.S3β-5/512*AP.S5β)*AP.S1^4+4375*AP.C1^6*(-1/1536*AP.Sβ-1/1024*AP.S3β+5/1024*AP.S5β)*AP.S1^4)
		  '21 Return Parameters.δ*(AP.C1^2*(-20475/4096*AP.Sβ+149391/8192*AP.S3β+AP.C4/4096*(567*AP.Sβ+1701/2*AP.S3β-8505/2*AP.S5β)
		  '+AP.C2/2048*(4374*AP.Sβ+10017*AP.S3β-1701*AP.S5β)-7371/8192*AP.S5β)*AP.S1^4+AP.η*AP.C1^2*(3195/2048*AP.Sβ-45711/4096*AP.S3β
		  '+7371/4096*AP.S5β+AP.C2*(-2187/512*AP.Sβ-10017/1024*AP.S3β+1701/1024*AP.S5β)+AP.C4*(-567/2048*AP.Sβ-1701/4096*AP.S3β+8505/4096*AP.S5β))*AP.S1^4)
		  '22 Return Parameters.δ*(AP.η*AP.C1^3*((37/192*AP.Cβ-AP.Cβ*AP.C2β/192)*AP.Sβ^2-(7/192+5/192*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5
		  '+AP.C1^3*((-37/384*AP.Cβ+AP.Cβ*AP.C2β/384)*AP.Sβ^2+(7/384*AP.Cβ+5/384*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5)
		  '23 Return Parameters.δ*((1/128+1/384*AP.C2β-AP.η*(1/64+1/192*AP.C2β))*AP.C1^4*AP.Sβ^3*AP.S1^6)
		  '24 Return Parameters.δ*(AP.η*((14067/4096+4689/1024*AP.C2β-5751/4096*AP.C4β)*AP.Sβ+(-297/1024+1053/256*AP.C2β
		  '-2187/1024*AP.C4β)*AP.C2*AP.Sβ-(5103/4096+1701/1024*AP.C2β+3645/4096*AP.C4β)*AP.C4*AP.Sβ)*AP.S1^6+((-55539/8192-8145/2048*AP.C2β
		  '+5751/8192*AP.C4β)*AP.Sβ+(297/2048-1053/512*AP.C2β+2187/2048*AP.C4β)*AP.C2*AP.Sβ+(5103/8192+1701/2048*AP.C2β+3645/8192*AP.C4β)*AP.C4*AP.Sβ)*AP.S1^6)
		  '25 Return Parameters.δ*(AP.C1^4*(4375/1536*AP.Sβ+4375/1024*AP.S3β-21875/1024*AP.S5β)*AP.S1^6+AP.η*AP.C1^4*(-4375/768*AP.Sβ-4375/512*AP.S3β+21875/512*AP.S5β)*AP.S1^6)
		  '26 Return Parameters.δ*((4375/384*AP.Cβ+625/256*AP.C3β+3125/256*AP.C5β-AP.η*(4375/192*AP.Cβ+625/128*AP.C3β+3125/128*AP.C5β))*AP.C1^3*AP.S1^7)
		  '27 Return Parameters.δ*(AP.η*AP.C1*((351/128*AP.Cβ-243/128*AP.Cβ*AP.C2β)*AP.Sβ^2-(567/128*AP.Cβ+405/128*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7
		  '+AP.C1*((-351/256*AP.Cβ+243/256*AP.Cβ*AP.C2β)*AP.Sβ^2+(567/256*AP.Cβ+405/256*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7)
		  '28 Return Parameters.δ*((243/256-81/256*AP.C2β-AP.η*(243/128+81/128*AP.C2β))*AP.C1^2*AP.Sβ^3*AP.S1^8)
		  '29 Return Parameters.δ*((4375/1024*AP.Sβ+8125/2048*AP.S3β+9375/2048*AP.S5β-AP.η*(4375/512*AP.Sβ+8125/1024*AP.S3β+9375/1024*AP.S5β))*AP.C1^2*AP.S1^8)
		  '30 Return Parameters.δ*((11875/768*AP.Cβ+3125/768*AP.C3β-AP.η*(11875/384*AP.Cβ+3125/384*AP.C3β))*AP.C1*AP.Sβ^2*AP.S1^9)
		  '31 Return Parameters.δ*((625/256+625/768*AP.C2β-AP.η*(625/128+625/384*AP.C2β))*AP.Sβ^3*AP.S1^10)
		  '32 Return Parameters.δ*(AP.η*((10197/2048*AP.Cβ-3969/2048*AP.Cβ*AP.C2β)*AP.Sβ^2-(1701/2048*AP.Cβ+5103/2048*AP.Cβ*AP.C2β)*AP.C4*AP.Sβ^2)*AP.S2^3
		  '+((-44757/4096*AP.Cβ+3969/4096*AP.Cβ*AP.C2β)*AP.Sβ^2+(1701/4096*AP.Cβ+5103/4096*AP.Cβ*AP.C2β)*AP.C4*AP.Sβ^2)*AP.S2^3)
		  '33 Return Parameters.δ*((21875/4096*AP.Cβ+13125/4096*AP.C3β-AP.η*(21875/2048*AP.Cβ+13125/2048*AP.C3β))*AP.Sβ^2*AP.S2^5)
		  '34 Return Parameters.δ*((-37071/16384*AP.Cβ*AP.C2β+AP.Cβ*(-7641/8192+567/32768*AP.C4β)-(10917/8192*AP.Cβ+2835/1024*AP.Cβ*AP.C2β)*AP.C2
		  '+(-10089/16384*AP.Cβ+135/8192*AP.Cβ*AP.C2β)*AP.C4+513/8192*AP.Cβ*AP.C6+5167/32768*AP.Cβ*AP.C8)*AP.S2-81/8192*AP.Cβ*AP.C4β*AP.S4
		  '+1053/65536*AP.Cβ*AP.C4β*AP.S6+(2565/32768*AP.C3β+729/32768*AP.C5β)*AP.S8+(243/131072*AP.C3β+1215/131072*AP.C5β)*AP.S10
		  '+AP.η*((5967/8192*AP.Cβ*AP.C2β+AP.Cβ*(2457/4096-567/16384*AP.C4β)+(4005/4096*AP.Cβ+243/512*AP.Cβ*AP.C2β)*AP.C2+(6633/8192*AP.Cβ
		  '-5319/4096*AP.Cβ*AP.C2β)*AP.C4-513/4096*AP.Cβ*AP.C6-567/16384*AP.Cβ*AP.C8)*AP.S2+81/4096*AP.Cβ*AP.C4β*AP.S4-1053/32768*AP.Cβ*AP.C4β*AP.S6
		  '-(2565/16384*AP.C3β+729/16384*AP.C5β)*AP.S8-(243/65536*AP.C3β+1215/65536*AP.C5β)*AP.S10))
		  '35 Return Parameters.δ*((-18603/8192*AP.Cβ*AP.C2β+AP.Cβ*(-20475/32768+567/32768*AP.C4β))*AP.S2+(2835/2048*AP.Cβ*AP.C2β
		  '+AP.Cβ*(5715/8192+81/8192*AP.C4β))*AP.S4+(135/16384*AP.Cβ*AP.C2β+AP.Cβ*(-20745/65536+1053/65536*AP.C4β))*AP.S6-(513/16384*AP.Cβ
		  '+2565/32768*AP.C3β+729/32768*AP.C5β)*AP.S8+(567/65536*AP.Cβ+243/131072*AP.C3β+1215/131072*AP.C5β)*AP.S10+AP.η*((5643/4096*AP.Cβ*AP.C2β
		  '+AP.Cβ*(3195/16384-567/16384*AP.C4β))*AP.S6+(513/8192*AP.Cβ+2565/16384*AP.C3β+729/16384*AP.C5β)*AP.S8-(567/32768*AP.Cβ+243/65536*AP.C3β+1215/65536*AP.C5β)*AP.S10))
		  '36 Return Parameters.δ*((319/24576*AP.Cβ*AP.C2β+AP.Cβ*(871/4096+AP.C4β/49152)+(933/4096*AP.Cβ+133/1536*AP.Cβ*AP.C2β)*AP.C2+(625/24576*AP.Cβ
		  '+211/4096*AP.Cβ*AP.C2β)*AP.C4-11/12288*AP.Cβ*AP.C6-7/49152*AP.Cβ*AP.C8)*AP.S2-AP.Cβ*AP.C4β*AP.S4/12288+AP.Cβ*AP.C4β*AP.S6/32768-(45/16384*AP.C3β
		  '+AP.C5β/16384)*AP.S8-(AP.C3β/65536+5*AP.C5β/65536)*AP.S10+AP.η*((257/12288*AP.Cβ*AP.C2β-AP.Cβ*(1493/6144+AP.C4β/24576)+(-1391/6144+11/768*AP.Cβ*AP.C2β)*AP.C2
		  '+(-49/12288*AP.Cβ+77/2048*AP.Cβ*AP.C2β)*AP.C4+11/6144*AP.Cβ*AP.C6+7/24576*AP.Cβ*AP.C8)*AP.S2+AP.Cβ*AP.C4β*AP.S4/6144-AP.Cβ*AP.C4β*AP.S6/16384+(45/8192*AP.C3β
		  '+AP.C5β/8192)*AP.S8+(AP.C3β/32768+5/32768*AP.C5β)*AP.S10))
		  '37 Return Parameters.δ*((-157/12288*AP.Cβ*AP.C2β+AP.Cβ*(9287/49152+AP.C4β/49152))*AP.S2+(-133/3072*AP.Cβ*AP.C2β+AP.Cβ*(-1405/12288+AP.C4β/12288))*AP.S4
		  '+(211/8192*AP.Cβ*AP.C2β+AP.Cβ*(419/32768+AP.C4β/32768))*AP.S6+(11/24576*AP.Cβ+45/16384*AP.C3β+AP.C5β/16384)*AP.S8-(7/98304*AP.Cβ
		  '+AP.C3β/65536+5*AP.C5β/65536)*AP.S10+AP.η*((13/6144*AP.Cβ*AP.C2β-AP.Cβ*(5923/24576+AP.C4β/24576))*AP.S2+(-11/1536*AP.Cβ*AP.C2β
		  '+AP.Cβ*(701/6144-AP.C4β/6144))*AP.S4+(77/4096*AP.Cβ*AP.C2β-AP.Cβ*(35/16384+AP.C4β/16384))*AP.S6-(11/12288*AP.Cβ+45/8192*AP.C3β+AP.C5β/8192)*AP.S8
		  '+(7/49152*AP.Cβ+AP.C3β/32768+5/32768*AP.C5β)*AP.S10))
		  '38 Return Parameters.δ*((-341/8192*AP.Cβ+AP.Cβ*AP.C2β/8192)*AP.Sβ^2*AP.S2+(-3411/16384*AP.Cβ+7/16384*AP.Cβ*AP.C2β)*AP.Sβ^2*AP.S6+(35/32768*AP.Cβ
		  '+21/32768*AP.C3β)*AP.Sβ^2*AP.S10+AP.η*((-43/4096*AP.Cβ-AP.Cβ*AP.C2β/4096)*AP.Sβ^2*AP.S2+(-429/8192*AP.Cβ+7/8192*AP.Cβ*AP.C2β)*AP.Sβ^2*AP.S6
		  '+(-35/16384*AP.Cβ-21/16384*AP.C3β)*AP.Sβ^2*AP.S10))
		  
		  '1 Return AP.χsx*(2*AP.Cβ*AP.C2^2*AP.Sβ-AP.η*AP.Cβ*AP.C2^3*AP.Sβ)
		  '2 Return AP.χsz*(AP.η*AP.C1^4*(-5/2-7/2*AP.C2β+(1/2+AP.C2β/6)*AP.C2)+AP.C1^4*(-3-AP.C2β+(5+5/3*AP.C2β)*AP.C4))
		  '+AP.χsx*(AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β)-AP.η*AP.C1^4*(19/6*AP.S2β+1/3*AP.C2*AP.S2β))
		  '3 Return AP.χsx*(AP.η*(1/2+AP.C2β/6)*AP.C1^5*AP.S1+(5+5/3*AP.C2β)*AP.C1^5*AP.S1)
		  '4 Return AP.χsx*(AP.η*(1/2+AP.C2β/6)*AP.C1*AP.S1^5+(5+5/3*AP.C2β)*AP.C1*AP.S1^5)
		  '5 Return AP.χsx*(AP.η*AP.C1^3*(-17/4+79/12*AP.C2β+(-1/4+7/12*AP.C2β)*AP.C2)*AP.S1+AP.C1^3*(3/2-13/6*AP.C2β+(-5/2+35/6*AP.C2β)*AP.C2)*AP.S1)
		  '+AP.χsz*(AP.η*AP.C1^3*(-7*AP.S2β+2/3*AP.C2*AP.S2β)*AP.S1+AP.C1^3*(-2*AP.S2β+20/3*AP.C2*AP.S2β)*AP.S1)
		  '6 Return AP.χsx*(AP.C1*(3/2-13/6*AP.C2β+(5/2-35/6*AP.C2β)*AP.C2)*AP.S1^3+AP.η*AP.C1*(-17/4+79/12*AP.C2β+(1/4-7/12*AP.C2β)*AP.C2)*AP.S1^3)
		  '+AP.χsz*(-AP.C1*(2*AP.S2β+20/3*AP.C2*AP.S2β)*AP.S1^3-AP.η*AP.C1*(7*AP.S2β+2/3*AP.C2*AP.S2β)*AP.S1^3)
		  '7 Return AP.χsz*(AP.η*(5/2+7/2*AP.C2β+(1/2+AP.C2β/6)*AP.C2)*AP.S1^4+(3+AP.C2β+(5+5/3*AP.C2β)*AP.C2)*AP.S1^4)+AP.χsx*(-(7/3*AP.S2β+10/3*AP.C2*AP.S2β)*AP.S1^4
		  '+AP.η*(19/6*AP.S2β-1/3*AP.C2*AP.S2β)*AP.S1^4)
		  '8 Return AP.χsz*(-3+3/2*AP.η)*AP.C2*AP.Sβ^2*AP.S2^2
		  '9 Return AP.χsx*(3/4+AP.C2β/4-AP.η*(3/8+AP.C2β/8))*AP.S2^3
		  '10 Return AP.χsx*(10/3+1/3*AP.η)*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2+AP.χsz*(5+AP.η/2)*AP.C2*AP.Sβ^2*AP.S2^2
		  '11 Return AP.χsz*(3/2+AP.C2β/2-AP.η*(3/4+AP.C2β/4))*AP.C2*AP.S2^2+AP.χsx*(AP.η/2-1)*AP.C2*AP.S2β*AP.S2^2
		  '12 Return AP.χsx*(-11/16*AP.C2β*AP.S2-3/4*AP.S2^3-7/16*AP.C2β*AP.S6+AP.η*(11/32*AP.C2β*AP.S2+3/8*AP.S2^3+7/32*AP.C2β*AP.S6))
		  '+AP.χsz*(AP.S2β*AP.S2/2-AP.S2β*AP.S6/2+AP.η*(-1/4*AP.S2β*AP.S2+1/4*AP.S2β*AP.S6))
		  '13 Return AP.χsy*((15/8-3/8*AP.C2β+(9/8-5/8*AP.C2β)*AP.C4)*AP.S2+AP.η*(-15/16+3/16*AP.C2β+(-9/16+5/16*AP.C2β)*AP.C4)*AP.S2)
		  '14 Return AP.χsy*(AP.η-2)*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2
		  '15 Return AP.χsy*(3/4+AP.C2β/4-AP.η*(3/8+AP.C2β/8))*AP.S2^3
		  '16 Return AP.χsy*(AP.C1*(5/2-11/6*AP.C2β+(15/2-25/6*AP.C2β)*AP.C2)*AP.S1^3+AP.η*AP.C1*(1/4-31/12*AP.C2β+(3/4-5/12*AP.C2β)*AP.C2)*AP.S1^3)
		  '17 Return AP.χsy*(-(7/3*AP.S2β+10/3*AP.C2*AP.S2β)*AP.S1^4-AP.η*(5/6*AP.S2β+1/3*AP.C2*AP.S2β)*AP.S1^4)
		  '18 Return AP.χsy*(5+5/3*AP.C2β+AP.η*(1/2+AP.C2β/6))*AP.C1*AP.S1^5
		  '19 Return -AP.χsy*(1/3+11/6*AP.η)*AP.Cβ*AP.Sβ*AP.S2^2
		  '20 Return AP.χsy*(AP.η*AP.C1^3*(1/4-31/12*AP.C2β)+(-3/4+5/12*AP.C2β)*AP.C2)*AP.S1+AP.C1^3*(5/2-11/6*AP.C2β+(-15/2+25/6*AP.C2β)*AP.C2*AP.S1)
		  '21 Return AP.χsy*(AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β)+AP.η*AP.C1^4*(5/6*AP.S2β-1/3*AP.C2*AP.S2β))
		  '22 Return AP.χsy*(AP.η*(1/2+AP.C2β/6)+5+5/3*AP.C2β)*AP.C1^5*AP.S1
		  '23 Return 2*Parameters.δ*AP.χax*AP.Cβ*AP.C2^3*AP.Sβ
		  '24 Return Parameters.δ*(AP.χaz*AP.C1^4*(-3-AP.C2β+(5+5/3*AP.C2β)*AP.C2)+AP.χax*AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β))
		  '25 Return Parameters.δ*AP.χax*(5+5/3*AP.C2β)*AP.C1^5*AP.S1
		  '26 Return Parameters.δ*AP.χax*(5+5/3*AP.C2β)*AP.C1*AP.S1^5
		  '27 Return Parameters.δ*(AP.χax*(3/2-13/6*AP.C2β+(-5/2+35/6*AP.C2β)*AP.C2)+AP.χaz*(-2*AP.S2β+20/3*AP.C2*AP.S2β))*AP.C1^3*AP.S1
		  '28 Return Parameters.δ*(AP.χax*(3/2-13/6*AP.C2β+(5/2-35/6*AP.C2β)*AP.C2)+AP.χaz*(-2*AP.S2β-20/3*AP.C2*AP.S2β))*AP.C1*AP.S1^3
		  '29 Return Parameters.δ*(AP.χaz*(3+AP.C2β+(5+5/3*AP.C2β)*AP.C2)*AP.S1^4-AP.χax*(7/2*AP.S2β+10/3*AP.C2*AP.S2β)*AP.S1^4)
		  '30 Return -3*Parameters.δ*AP.χaz*AP.C2*AP.Sβ^2*AP.S2^2
		  '31 Return Parameters.δ*AP.χax*(3/4+AP.C2β/4)*AP.S2^3
		  '32 Return Parameters.δ*(10/3*AP.χax*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2+5*AP.χaz*AP.C2*AP.Sβ^2*AP.S2^2)
		  '33 Return Parameters.δ*AP.χaz*(3/2*AP.C2β/2)*AP.C2*AP.S2^2-AP.χax*AP.C2*AP.S2β*AP.S2^2
		  '34 Return Parameters.δ*(AP.χax*(-11/16*AP.C2β*AP.S2-3/4*AP.S2^3-7/16*AP.C2β*AP.S6)+AP.χaz*(AP.S2β*AP.S2/2-AP.S2β*AP.S6/2))
		  '35 Return Parameters.δ*(AP.χay*(15/8-3/8*AP.C2β+(9/8-5/8*AP.C2β)*AP.C4)*AP.S2)
		  '36 Return -2*Parameters.δ*AP.χay*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2
		  '37 Return Parameters.δ*AP.χay*(3/4+AP.C2β/4)*AP.S2^3
		  '38 Return Parameters.δ*AP.χay*AP.C1*(5/2-11/6*AP.C2β+(15/2-25/6*AP.C2β)*AP.C2)*AP.S1^3
		  '39 Return Parameters.δ*AP.χay*(-7/3*AP.S2β-10/3*AP.C2*AP.S2β)*AP.S1^4
		  '40 Return Parameters.δ*AP.χay*(5+5/3*AP.C2β)*AP.C1*AP.S1^5
		  '41 Return -1/3*Parameters.δ*AP.χay*AP.Cβ*AP.Sβ*AP.S2^2
		  '42 Return Parameters.δ*AP.χay*AP.C1^3*(5/2-11/6*AP.C2β+(-15/2+25/6*AP.C2β)*AP.C2)*AP.S1
		  '43 Return Parameters.δ*AP.χay*AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β)
		  '44 Return Parameters.δ*AP.χay*(5+5/3*AP.C2β)*AP.C1^5*AP.S1
		  
		  //Amplitude factors for H3X
		  '1 Return 8*π*AP.C1*AP.Sβ*AP.S1^3
		  '2 Return -4*π*AP.Cβ*AP.S1^4
		  '3 Return -8*π*AP.C1^3*AP.Sβ*AP.S1
		  '4 Return -4*π*AP.Cβ*AP.C1^4
		  '5 Return Parameters.δ*(AP.C1^4*(-4375/384*AP.S2β-4375/256*AP.S4β)*AP.S1^6+AP.η*AP.C1^4*(4375/192*AP.S2β+4375/128*AP.S4β)*AP.S1^6)
		  '6 Return Parameters.δ*(625/96*AP.C2β+625/32*AP.C4β-AP.η*(625/48*AP.C2β+625/16*AP.C4β))*AP.C1^3*AP.S1^7
		  '7 Return Parameters.δ*(-625/256*AP.S2β+5625/512*AP.S4β+AP.η*(625/48*AP.S2β-5625/256*AP.S4β))*AP.C1^2*AP.S1^8
		  '8 Return Parameters.δ*(625/96+625/48*AP.C2β-AP.η*(625/48+625/24*AP.C2β))*AP.C1*AP.Sβ^2*AP.S1^9
		  '9 Return Parameters.δ*(625/192-625/96*AP.η)*AP.Cβ*AP.Sβ^3*AP.S1^10
		  '10 Return Parameters.δ*(AP.η*AP.C1^2*(-4923/512*AP.S2β+AP.C2*(459/128*AP.S2β-2079/256*AP.S4β)-945/1024*AP.S4β
		  '+AP.C4*(567/512*AP.S2β+1701/1024*AP.S4β))*AP.S1^4+AP.C1^2*(22203/1024*AP.S2β-AP.C4*(567/1024*AP.S2β+1701/2048*AP.S4β)+945/2048*AP.S4β
		  '+AP.C2*(-459/256*AP.S2β+2079/512*AP.S4β))*AP.S1^4)
		  '11 Return Parameters.δ*(AP.η*AP.C1*(27/16+1233/128*AP.C2β+27/128*AP.C4β+(27/8+27/16*AP.C2β+27/16*AP.C4β)*AP.C2
		  '-(81/128*AP.C2β+243/128*AP.C4β)*AP.C4)*AP.S1^5+AP.C1*(-27/32-4689/256*AP.C2β-27/256*AP.C4β-(27/16+27/32*AP.C2β
		  '+27/32*AP.C4β)*AP.C2+(81/256*AP.C2β+243/256*AP.C4β)*AP.C4)*AP.S1^5)
		  '12 Return Parameters.δ*(AP.η*((4761/1024-1377/1024*AP.C2β)*AP.S2β+(837/256-621/256*AP.C2β)*AP.C2*AP.S2β+(243/1024-2187/1024*AP.C2β)*AP.C4*AP.S2β)*AP.S1^6
		  '+((-11673/2048+1377/2048*AP.C2β)*AP.S2β+(-837/512+621/512*AP.C2β)*AP.C2*AP.S2β+(-243/2048+2187/2048*AP.C2β)*AP.C4*AP.S2β)*AP.S1^6)
		  '13 Return Parameters.δ*(AP.η*AP.C1*((81/32-27/16*AP.C2β)*AP.Sβ^2-(81/32+81/16*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7+AP.C1*((-81/64+27/32*AP.C2β)*AP.Sβ^2
		  '+(81/64+81/32*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7)
		  '14 Return Parameters.δ*(81/64-81/32*AP.η)*AP.Cβ*AP.C1^2*AP.Sβ^3*AP.S1^8
		  '15 Return Parameters.δ*(683/16384*AP.Cβ*AP.Sβ+(557/4096-11/12288*AP.C2β)*AP.C4*AP.S2β+(-1719/32768+91/32768*AP.C2β)*AP.C6*AP.S2β
		  '-1/16384*AP.Cβ*AP.S3β+AP.C2*(-10511/49152*AP.Cβ*AP.Sβ+173/49152*AP.Cβ*AP.S3β)+AP.η*(85/8192*AP.Cβ*AP.Sβ+(-679/6144+11/6144*AP.C2β)*AP.C4*AP.S2β
		  '-(201/16384+91/16384*AP.C2β)*AP.C6*AP.S2β+1/8192*AP.Cβ*AP.S3β+AP.C2*(6031/24576*AP.Cβ*AP.Sβ-173/24576*AP.Cβ*AP.S3β)
		  '-AP.C10*(7/49152*AP.S2β+7/32768*AP.S4β)+AP.C8*(-37/24576*AP.S2β+91/16384*AP.S4β))+AP.C8*(37/49152*AP.S2β-91/32768*AP.S4β)+AP.C10*(7/98304*AP.S2β+7/65536*AP.S4β))
		  '16 Return Parameters.δ*(AP.η*(19/512*AP.C4β*AP.C3+9/512*AP.C4β*AP.C5+AP.C1*(-11/16-35/128*AP.C2β+79/1536*AP.C4β+(1/32-37/256*AP.C2β)*AP.C2
		  '+(1/32+3/128*AP.C2β)*AP.C4-1/768*AP.C2β*AP.C6)-1/512*AP.C4β*AP.C7)*AP.S1^3+(-19/1024*AP.C4β*AP.C3-9/1024*AP.C4β*AP.C5+AP.C1*(19/32-23/768*AP.C2β
		  '-79/3072*AP.C4β-(1/64+347/512*AP.C2β)*AP.C2-(1/64+3/256*AP.C2β)*AP.C4+1/1536*AP.C2β*AP.C6)+1/1024*AP.C4β*AP.C7)*AP.S1^3)
		  '17 Return Parameters.δ*(AP.C1^2*(-355/1024*AP.S2β-AP.C2*(13/256*AP.S2β+11/512*AP.S4β)+AP.C4*(-1/1024*AP.S2β+9/2048*AP.S4β)
		  '-5/2048*AP.S4β)*AP.S1^4+AP.η*AP.C1^2*(-29/512*AP.S2β+AP.C4*(1/512*AP.S2β-9/1024*AP.S4β)+AP.C2*(13/128*AP.S2β+11/256*AP.S4β)+5/1024*AP.S4β)*AP.S1^4)
		  '18 Return Parameters.δ*(AP.η*AP.C1^3*((7/48+1/24*AP.C2β)*AP.Sβ^2-(1/48+1/24*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5+AP.C1^3*(-(7/96+1/48*AP.C2β)*AP.Sβ^2
		  '+(1/96+1/48*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5)
		  '19 Return Parameters.δ*(1/96-1/48*AP.η)*AP.Cβ*AP.C1^4*AP.Sβ^3*AP.S1^6
		  '20 Return Parameters.δ*((-77/256+1/256*AP.Cβ)*AP.Sβ^2*AP.S4+(5/512+7/512*AP.C2β)*AP.Sβ^2*AP.S8+AP.η*((45/128-1/128*AP.C2β)*AP.Sβ^2*AP.S4
		  '-(5/256-7/256*AP.C2β)*AP.Sβ^2*AP.S8))
		  '21 Return Parameters.δ* (135/64+189/64*AP.C2β-AP.η*(135/32+189/32*AP.C2β))*AP.C2*AP.Sβ^2*AP.S2^3
		  '22 Return Parameters.δ* (-683/16384*AP.Cβ*AP.Sβ+(-557/4096+11/12288*AP.C2β)*AP.C4*AP.S2β+(-1719/32768+91/32768*AP.C2β)*AP.C6*AP.S2β
		  '+AP.Cβ*AP.Sβ/16384+AP.C2*(-10511/49152*AP.Cβ*AP.Sβ+173/49152*AP.Cβ*AP.S3β)+AP.η*(-85/8192*(AP.Cβ)*AP.Sβ+(679/6144-11/6144*AP.C2β)*AP.C4*AP.S2β
		  '-(201/16384+91/16384*AP.C2β)*AP.C6*AP.S2β-AP.Cβ*AP.S3β/8192+AP.C2*(6031/24576*AP.Cβ*AP.Sβ-173/24576*AP.Cβ*AP.S3β)+AP.C8*(37/24576*AP.S2β-91/16384*AP.S4β)
		  '-AP.C10*(7/49152*AP.S2β+7/32768*AP.S4β))+AP.C10*(7/98304*AP.S2β+7/65536*AP.S4β)+AP.C8*(-37/49152*AP.S2β+91/32768*AP.S4β))
		  '23 Return Parameters.δ*(AP.C1^3*(19/32-23/768*AP.C2β-79/3072*AP.C4β+(1/64+347/512*AP.C2β)*AP.C2-(1/64+3/256*AP.C2β)*AP.C4-AP.C2β*AP.C6/1536)*AP.S1
		  '+19*AP.C4β*AP.C1^3*AP.S3/1024-9*AP.C4β*AP.C1^3*AP.S5/1024-AP.C4β*AP.C1^3*AP.S7/1024+AP.η*(AP.C1^3*(-11/16-35/128*AP.C2β+79/1536*AP.C4β
		  '+(-1/32+37/256*AP.C2β)*AP.C2+(1/32+3/128*AP.C2β)*AP.C4+1/768*AP.C2β*AP.C6)*AP.S1-19/512*AP.C4β*AP.C1^3*AP.S3+9/512*AP.C4β*AP.C1^3*AP.S5+1/512*AP.C4β*AP.C1^3*AP.S7))
		  '24 Return Parameters.δ*(AP.η*AP.C1^4*(4923/512*AP.S2β+AP.C4*(567/1024*AP.S2β+1701/2048*AP.S4β)-945/2048*AP.S4β+AP.C2*(-459/256*AP.S2β+2079/512*AP.S4β))*AP.S1^2)
		  '25 Return Parameters.δ*(AP.η*AP.C1^5*(27/16+1233/128*AP.C2β+27/128*AP.C4β-(27/8+27/16*AP.C2β+27/16*AP.C4β)*AP.C2-(81/128*AP.C2β+243/128*AP.C4β)*AP.C4)*AP.S1
		  '+AP.C1^5*(-27/32-4689/256*AP.C2β-27/256*AP.C4β+(27/16+27/32*AP.C2β+27/32*AP.C4β)*AP.C2+(81/256*AP.C2β+243/256*AP.C4β)*AP.C4)*AP.S1)
		  '26 Return Parameters.δ*(AP.C1^6*(11673/2048*AP.S2β+AP.C4*(243/2048*AP.S2β-2187/4096*AP.S4β)+AP.C2*(-837/512*AP.S2β+621/1024*AP.S4β)-1377/4096*AP.S4β)
		  '+AP.η*AP.C1^6*(-4761/1024*AP.S2β+AP.C2*(837/256*AP.S2β-621/512*AP.S4β)+1377/2048*AP.S4β+AP.C4*(-243/1024*AP.S2β+2187/2048*AP.S4β)))
		  '27 Return Parameters.δ*(AP.C1^7*((-81/64+27/32*AP.C2β)*AP.Sβ^2-(81/64+81/32*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1+AP.η*AP.C1^7*((81/32-27/16*AP.C2β)*AP.Sβ^2
		  '+(81/32+81/16*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1)
		  '28 Return Parameters.δ*(81/32*AP.η-81/64)*AP.Cβ*AP.C1^8*AP.Sβ^3*AP.S1^2
		  '29 Return Parameters.δ*(4375/384*AP.S2β+4375/256*AP.S4β-AP.η*(4375/192*AP.S2β+4375/128*AP.S4β))*AP.C1^6*AP.S1^4
		  '30 Return Parameters.δ*(625/96*AP.C2β+625/32*AP.C4β-AP.η*(625/48*AP.C2β+625/16*AP.C4β))*AP.C1^7*AP.S1^3
		  '31 Return Parameters.δ*(625/256*AP.S2β-5625/512*AP.S4β+AP.η*(-625/128*AP.S2β+5625/256*AP.S4β))*AP.C1^8*AP.S1^2
		  '32 Return Parameters.δ*(625/96+625/48*AP.C2β-AP.η*(625/48+625/24*AP.C2β))*AP.C1^9*AP.Sβ^2*AP.S1
		  '33 Return Parameters.δ*(625/96*AP.η-625/192)*AP.Cβ*AP.C1^10*AP.Sβ^3
		  
		  '1 Return AP.χsy*(2*AP.C2^3*AP.Sβ-AP.η*AP.C2^3*AP.Sβ)
		  '2 Return AP.χsy*(AP.η*AP.C1^4*(-5*AP.Sβ/3 + 2*AP.C2*AP.Sβ/3)+AP.C1^4*(-14*AP.Sβ/3+20*AP.C2*AP.Sβ/3))
		  '3 Return AP.χsy*(-20/3*AP.Cβ*AP.C1^5*AP.S1-2/3*AP.η*AP.Cβ*AP.C1^5*AP.S1)
		  '4 Return AP.χsy*(AP.η*AP.C1^3*(7*AP.Cβ/3+AP.Cβ*AP.C2/3)*AP.S1+AP.C1^3*(-2*AP.Cβ/3+10*AP.Cβ*AP.C2/3)*AP.S1)
		  '5 Return AP.χsy*(AP.η*AP.C1*(7*AP.Cβ/3 - AP.Cβ*AP.C2/3)*AP.S1^3 + AP.C1*(-2*AP.Cβ/3-10*AP.Cβ*AP.C2/3)*AP.S1^3)
		  '6 Return AP.χsy*(AP.η*(5*AP.Sβ/3+2*AP.C2*AP.Sβ/3)*AP.S1^4+(14*AP.Sβ/3+20*AP.C2*AP.Sβ/3)*AP.S1^4)
		  '7 Return AP.χsy*(-20/3*AP.Cβ*AP.C1*AP.S1^5-2/3*AP.η*AP.Cβ*AP.C1*AP.S1^5)
		  '8 Return AP.χsy*(2*AP.C2*AP.Sβ*AP.S2^2-AP.η*AP.C2*AP.Sβ*AP.S2^2)
		  '9 Return AP.χsy*(10/3*AP.C2*AP.Sβ*AP.S2^2+1/3*AP.η*AP.C2*AP.Sβ*AP.S2^2)
		  '10 Return AP.χsy*(-AP.Cβ*AP.S2^3+1/2*AP.η*AP.Cβ*AP.S2^3)
		  '11 Return AP.χsy*(-5/4*AP.Cβ*AP.S2-1/4*AP.Cβ*AP.S6+AP.η*(5*AP.Cβ*AP.S2/8+1/8*AP.Cβ*AP.S6))
		  '12 Return AP.χsx*((-3*AP.Cβ/2-1/2*AP.Cβ*AP.C4)*AP.S2+AP.η*(3*AP.Cβ+1/4*AP.Cβ*AP.C4)*AP.S2)+AP.χsz*(-2*AP.C4*AP.Sβ*AP.S2+AP.η*AP.C4*AP.Sβ*AP.S2)
		  '13 Return AP.χsz*(2*AP.Cβ*AP.C2*AP.S2^2-AP.η*AP.Cβ*AP.C2*AP.S2^2)+AP.χsx*(-2*AP.C2*AP.Sβ*AP.S2^2+AP.η*AP.C2*AP.Sβ*AP.S2^2)
		  '14 Return AP.χsx*(AP.Cβ*AP.S2^3-1/2*AP.η*AP.Cβ*AP.S2^3)
		  '15 Return AP.χsx*(AP.C1*(-2*AP.Cβ/3-10*AP.Cβ*AP.C2/3)*AP.S1^3+AP.η*AP.C1*(-5/3*AP.Cβ+4*AP.C3β-AP.Cβ*AP.C2/3)*AP.S1^3)
		  '+AP.χsz*(AP.C1*(-4*AP.Sβ-40*AP.C2*AP.Sβ/3)*AP.S1^3+AP.η*AP.C1*(-2*AP.Sβ-4*AP.C2*AP.Sβ/3-4*AP.S3β)*AP.S1^3)
		  '16 Return AP.χsz*(AP.η*(5*AP.Cβ+AP.C3β+2*AP.Cβ*AP.C2/3)*AP.S1^4+(4*AP.Cβ+20*AP.Cβ*AP.C2/3)*AP.S1^4)
		  '+AP.χsx*((-14*AP.Sβ/3-20*AP.C2*AP.Sβ/3)*AP.S1^4+AP.η*(10*AP.Sβ/3-2*AP.C2*AP.Sβ/3+AP.S3β)*AP.S1^4)
		  '17 Return AP.χsx*(20/3*AP.Cβ*AP.C1*AP.S1^5+2/3*AP.η*AP.Cβ*AP.C1*AP.S1^5)
		  '18 Return -6*AP.β*AP.χsz*AP.Cβ*AP.Sβ^2*AP.S2^2 + AP.χsx*(1/3*AP.Sβ*AP.S2^2+AP.η*(-7/6+3*AP.C2β)*AP.Sβ*AP.S2^2)
		  '19 Return AP.χsx*(AP.η*AP.C1^3*(-5*AP.Cβ/3+4*AP.C3β+AP.Cβ*AP.C2/3)*AP.S1+AP.C1^3*(-2*AP.Cβ/3+10*AP.Cβ*AP.C2/3)*AP.S1)
		  '+AP.χsz*(AP.C1^3*(-4*AP.Sβ+40*AP.C2*AP.Sβ/3)*AP.S1+AP.η*AP.C1^3*(-2*AP.Sβ+4*AP.C2*AP.Sβ/3-4*AP.S3β)*AP.S1)
		  '20 Return AP.χsz*(AP.η*AP.C1^4*(-5*AP.Cβ-AP.C3β+2*AP.Cβ*AP.C2/3)+AP.C1^4*(-4*AP.Cβ+20*AP.Cβ*AP.C2/3))
		  '+AP.χsx*(AP.C1^4*(14*AP.Sβ/3-20*AP.C2*AP.Sβ/3)+AP.η*AP.C1^4*(-10*AP.Sβ/3-2*AP.C2*AP.Sβ/3-AP.S3β))
		  '21 Return AP.χsx*(20/3*AP.Cβ*AP.C1^5*AP.S1+2/3*AP.η*AP.Cβ*AP.C1^5*AP.S1)
		  '22 Return Parameters.δ*(2*AP.χay*AP.C2^3*AP.Sβ)
		  '23 Return Parameters.δ*(AP.χay*AP.C1^4*(-14/3*AP.Sβ+20/3*AP.C2*AP.Sβ))
		  '24 Return Parameters.δ*(AP.χay*AP.C1^3*(-2/3*AP.Cβ+10/3*AP.Cβ*AP.C2)*AP.S1)
		  '25 Return Parameters.δ*(-20/3*AP.χay*AP.Cβ*AP.C1^5*AP.S1)
		  '26 Return Parameters.δ*(AP.χay*AP.C1*(-2/3*AP.Cβ-10/3*AP.Cβ*AP.C2)*AP.S1^3)
		  '27 Return Parameters.δ*(AP.χay*(14/3*AP.Sβ+20/3*AP.C2*AP.Sβ)*AP.S1^4)
		  '29 Return Parameters.δ*(2*AP.χay*AP.C2*AP.Sβ*AP.S2^2)
		  '30 Return Parameters.δ*(10/3*AP.χay*AP.C2*AP.Sβ*AP.S2^2)
		  '31 Return Parameters.δ*(-AP.χay*AP.Cβ*AP.S1^3)
		  '32 Return Parameters.δ*(AP.χay*(-5/4*AP.Cβ*AP.S2-1/4*AP.Cβ*AP.S6))
		  '33 Return Parameters.δ*(AP.χax*(-3/2*AP.Cβ-AP.Cβ*AP.C4/2)*AP.S2-2*AP.χaz*AP.C4*AP.Sβ*AP.S2)
		  '34 Return Parameters.δ*(2*AP.χaz*AP.Cβ*AP.C2*AP.S2^2-2*AP.χax*AP.C2*AP.Sβ*AP.S2^2)
		  '35 Return Parameters.δ*(AP.χax*AP.Cβ*AP.S2^3)
		  '36 Return Parameters.δ*(AP.χax*AP.C1*(-2/3*AP.Cβ-10/3*AP.Cβ*AP.C2)*AP.S1^3+AP.χaz*AP.C1*(-4*AP.Sβ-40/3*AP.C2*AP.Sβ)*AP.S1^3)
		  '37 Return Parameters.δ*(AP.χaz*(4*AP.Cβ+20/3*AP.Cβ*AP.C2)*AP.S1^4-AP.χax*(14/3*AP.Sβ+20/3*AP.C2*AP.Sβ)*AP.S1^4)
		  '38 Return Parameters.δ*(20/3*AP.χax*AP.Cβ*AP.C1*AP.S1^5)
		  '39 Return Parameters.δ*(1/3*AP.χax*AP.Sβ*AP.S2^2)
		  '40 Return Parameters.δ*(AP.χax*AP.C1^3*(-2/3*AP.Cβ+10/3*AP.Cβ*AP.C2)*AP.S1+AP.χaz*AP.C1^3*(-4*AP.Sβ+40/3*AP.C2*AP.Sβ)*AP.S1)
		  '41 Return Parameters.δ*(AP.χaz*AP.C1^4*(-4*AP.Cβ+20/3*AP.Cβ*AP.C2)+AP.χax*AP.C1^4*(14/3*AP.Sβ-20/3*AP.C2*AP.Sβ))
		  '42 Return Parameters.δ*(20/3*AP.χax*AP.Cβ*AP.C1^5*AP.S1)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateWaveFactors()
		  // Now calculate all wavy parts
		  // Factors for H0P
		  W(0) = c20*c02 - s20*s02
		  W(1) = c10*c02 - s10*s02
		  W(2) = c10*c02 + s10*s02
		  W(3) = c20*c02 + s20*s02
		  W(4) = c02
		  
		  // Factors for H0X
		  W(5) = s10*c02 - c10*s02
		  W(6) = s20*c02 - c20*s02
		  W(7) = s10*c02 + c10*s02
		  W(8) = s20*c02 + c20*s02
		  
		  // Factors for H1P
		  W(9) = c30*c03 - s30*s03
		  W(10) = c10*c01 - s10*s01
		  W(11) = c10*c01 + s10*s01
		  W(12) = c30*c01 - s30*s01
		  W(13) = c10*c03 - s10*s03
		  W(14) = c10*c03 + s10*s03
		  W(15) = c30*c01 + s30*s01
		  W(16) = c30*c03 + s30*s03
		  W(17) = c03
		  W(18) = c20*c01 - s20*s01
		  W(19) = c20*c03 - s20*s03
		  W(20) = c20*c01 + s20*s01
		  W(21) = c20*c03 + s20*s03
		  W(22) = c01
		  
		  // Factors for H1X
		  
		  // Calculate derivatives with respect to Ψr
		  // For H0P
		  DWDΨ(0) = -2*(s20*c02+c20*s02)
		  DWDΨ(1) = -2*(s10*c02+c10*s02)
		  DWDΨ(2) = 2*(s10*c02-c10*s02)
		  DWDΨ(3) = 2*(s20*c02-c20*s02)
		  DWDΨ(4) = -2*s02
		  
		  // Factors for H0X
		  DWDΨ(5) = -2*(c10*c02+s10*s02)
		  DWDΨ(6) = -2*(c20*c02+s20*s02)
		  DWDΨ(7) = 2*(c10*c02-s10*s02)
		  DWDΨ(8) = 2*(c20*c02-s20*s02)
		  
		  // Factors for H1P
		  DWDΨ(9) = -2*(s30*c03+c30*s03)
		  DWDΨ(10) = -2*(s10*c01+c10*s01)
		  DWDΨ(11) = s10*c01-c10*s01
		  DWDΨ(12) = -s30*c01-c30*s01
		  DWDΨ(13) = -3*(s10*c03+c10*s03)
		  DWDΨ(14) = 3*(s10*c03-c10*s03)
		  DWDΨ(15) = s30*c01-c30*s01
		  DWDΨ(16) = 3*(s30*c03-c30*s03)
		  DWDΨ(17) = -3*s03
		  DWDΨ(18) = -s20*c01-c20*s01
		  DWDΨ(19) = -3*(s20*c03+c20*s03)
		  DWDΨ(20) = s20*c01-c20*s01
		  DWDΨ(21) = 3*(s20*c03-c20*s03)
		  DWDΨ(22) = -s01
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(P As CaseParametersClass, BaseCase As EvolverClass = Nil)
		  Parameters = P
		  Dτr = P.ΔT/P.GM
		  
		  // Initialize phase-related properties
		  ΨrN = P.λ0  // Set the initial phase
		  ΨrP = ΨrN   // The past phase is initially the same
		  VeSinΘ = Sin(Parameters.Θ)*Parameters.Ve
		  
		  // Initialize the velocity-related properties
		  VN = P.V0
		  VP = VN
		  
		  // Initialize the spin-related properties
		  InitializeSpins
		  
		  // Initialize some constants for velocity evolution
		  Var δ As Double = P.δ
		  Var η As Double = P.η
		  Var π As Double = P.π
		  Var γE As Double = 0.5772156649015328606
		  C0 = 32*η/5
		  C2 = -743/336 - 11*η/4
		  C3 = 4*π - 47*χs𝓁/3 - δ*25*χa𝓁/4
		  C4 = 34103/18144 + 13661*η/2016 + 59*η*η/18
		  C5 = (-5861/144 + 1001*η/12)*χs𝓁 + δ*(-809/84 + 281*η/8)*χa𝓁
		  C5 = C5 + 4159*π/672 + 189*π*η/8
		  C6 = 16477322263.0/139708800 - 1712*γE/105 + 16*π*π/3
		  C6 = C6 + (-56198689/217728 + 451*π*π/48)*η
		  C6 = C6 + 541*η*η/896 - 5605*η*η*η/2592 - 856*Log(32)/105
		  C6L = 856/105
		  C7 = π*(-4415/4032 + 358675*η/6048 + 91495*η*η/1512)
		  CSD0 = C0
		  CSD1 = -47/3
		  CSD2 = -5861/144 + 1001*η/12
		  CAD0 = C0*δ
		  CAD1 = -25/4
		  CAD2 = -809/84 + 281*η/8
		  
		  //Initialize some constants for amplitude calculation
		  Cβ(1) = Cos(β)
		  Sβ(1) = Sin(β)
		  Cβ(2) = Cos(2*β)
		  Sβ(2) = Sin(2*β)
		  Cβ(3) = Cos(3*β)
		  Sβ(3) = Sin(3*β)
		  Cβ(4) = Cos(4*β)
		  Sβ(4) = Sin(4*β)
		  Cβ(5) = Cos(5*β)
		  Sβ(5) = Sin(5*β)
		  
		  // Initialize time-related properties
		  τ = 0.0  // currently, we are at time step zero
		  
		  Var Dτ0 As Double = 0.5*Dτr/(1.0 + Parameters.Z)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSourceStep(DτF As Double, DτP As Double)
		  // This is the main method for doing a time step for the source.
		  // We first need to make the future from the past step the present for the current step
		  MakeFuturePresent
		  
		  // The current time at Now is equal to the previous time times the magnitude of the past time step
		  τ = τ + DτP
		  
		  // Calculate new past values using interpolation (note that this effectively does nothing if DτF/DτP = 1,
		  // but it is probably faster just to do the calculation than to do a check and then a calculation
		  Var dτRatio As Double = DτF/DτP // calculate this ratio once so we don't have to do it many times
		  Var oneMinusRatio As Double = 1.0 - DτRatio // Calculate this only once also
		  Var twoDτF As Double = 2.0*DτF
		  VP = oneMinusRatio*VN + dτRatio*VP
		  
		  // Evolve the velocity forward
		  Var v2 As Double = VN*VN
		  Var v3 As Double = v2*VN
		  Var v4 As Double = v2*v2
		  Var v5 As Double = v2*v3
		  Var v6 As Double = v3*v3
		  Var v7 As Double = v3*v4
		  Var v9 As Double = v4*v5
		  Var vDotN = C0*v9*(1 + CV2*v2 + CV3*v3 + CV4*v4 + CV5*v5 + (CV6 + CV6L*Log(VNow))*v6 + CV7*v7)
		  VF = VP + twoDτF*vDotN
		  DτIdeal As Double = ε/vDotN  // Calculate the ideal next step (we will only pay attention to the base case value).
		  
		  // Now we will do the spin evolution
		  If Magχ1 = 0.0 and Magχ2 = 0.0 Then // If spins are both strictly zero, then there is no evolution
		    χ1hatF = χ1hatN
		    χ2hatF = χ2hatN
		    LF = LN
		    αF = αN
		    ιF = ιN
		    αDotN = 0.0
		    DτIdeal = Infinity
		    χsF = χsN
		    χaF = χaN
		    αF = αN
		    ιF = 0.0
		    αDotN = 0.0
		  Else // spins are not strictly zero
		    // Calculate new past values using interpolation (note that this effectively does nothing if DτF/DτP = 1,
		    // but it is probably faster just to do the calculation
		    // Note that we are NOT using the defined vector operations because the overhead is large
		    // and we do not want to create new objects after initialization
		    χ1hatP.X = oneMinusRatio*χ1hatN.X + dτRatio*χ1hatP.X  
		    χ1hatP.Y = oneMinusRatio*χ1hatN.Y + dτRatio*χ1hatP.Y 
		    χ1hatP.Z = oneMinusRatio*χ1hatN.Z + dτRatio*χ1hatP.Z  
		    χ2hatP.X = oneMinusRatio*χ2hatN.X + dτRatio*χ2hatP.X  
		    χ2hatP.Y = oneMinusRatio*χ2hatN.Y + dτRatio*χ2hatP.Y 
		    χ2hatP.Z = oneMinusRatio*χ2hatN.Z + dτRatio*χ2hatP.Z  
		    LP.X = oneMinusRatio*LN.X + dτRatio*LP.X
		    LP.Y = oneMinusRatio*LN.Y + dτRatio*LP.Y
		    LP.Z = oneMinusRatio*LN.Z + dτRatio*LP.Z
		    αP = oneMinusRatio*αN + dτRatio*αP
		    
		    // Do the step
		    // Evolve the two spins using the leapfrog method
		    Var Factor As Double = v5*(CΩ0 + CΩ1 + (CΩ2 + CΩ3)*v2 + (CΩ4 + CΩ5)*v4)*2*DτF
		    Var χ1HatDotNx As Double = Factor*(LN.Y*χ1HatN.Z - LN.Z*χ1HatN.Y)
		    Var χ1HatDotNy As Double = Factor*(LN.Z*χ1HatN.X - LN.X*χ1HatN.Z)
		    Var χ1HatDotNz As Double = Factor*(LN.X*χ1HatN.Y - LN.Y*χ1HatN.X)
		    χ1HatF.X = χ1HatP.X + χ1HatDotNx
		    χ1HatF.Y = χ1HatP.Y + χ1HatDotNy
		    χ1HatF.Z = χ1HatP.Z + χ1HatDotNz
		    Var χ2HatDotNx As Double = Factor*(LN.Y*χ2HatN.Z - LN.Z*χ2HatN.Y)
		    Var χ2HatDotNy As Double = Factor*(LN.Z*χ2HatN.X - LN.X*χ2HatN.Z)
		    Var χ2HatDotNz As Double = Factor*(LN.X*χ2HatN.Y - LN.Y*χ2HatN.X)
		    χ2HatF.X = χ2HatP.X + χ2HatDotNx
		    χ2HatF.Y = χ2HatP.Y + χ2HatDotNy
		    χ2HatF.Z = χ2HatP.Z + χ2HatDotNz
		    
		    
		    // Evolve the orbital angular momentum
		    Factor = -VN*(1-CL3*v2-CL4*v4)*2*DτF
		    Var ellNDotx As Double = Factor*(CL1*χ1HatDotNx + CL2*χ2HatDotNx)
		    Var ellNDoty As Double = Factor*(CL1*χ1HatDotNy + CL2*χ2HatDotNy)
		    Var ellNDotz As Double = Factor*(CL1*χ1HatDotNz + CL2*χ2HatDotNz)
		    LF.X = LP.X + ellNDotx
		    LF.Y = LP.Y + ellNDoty
		    LF.Z = LP.Z + ellNDotz
		    // The magnitude of L MUST be one, so ensure this
		    Var invMagLF As Double = 1.0/LF.GetMagnitude
		    LF.X = LF.X*invMagLF
		    LF.Y = LF.Y*invMagLF
		    LF.Z = LF.Z*invMagLF
		    
		    // Calculate the future angles
		    Var ellFx As Double = LF.X
		    Var ellFy As Double = LF.Y
		    Var ellNx As Double = LN.X
		    Var ellNy As Double = LN.Y
		    If ellFx*ellFx + ellFy*ellFy > 1.0e-10 Then
		      // The future L vector points at least some angle away from the vertical,
		      // so α is well-defined and we can calculate it normally
		      αF = ATan2(ellFy, ellFx)
		      // To keep α from jumping in value when the L vector crosses the x axis,
		      // we need to adjust its value from what the ATan2 function gives us
		      If ellFy < 0.0 and ellNy > 0.0 Then // If we are crossing the x axis downward
		        // and if the intercept with the x axis is negative, meaning we are going
		        // from the second quadrant to the third, then ATan jumps from π to -π,
		        // so we add 2π to compensate
		        If (ellFy*ellNx - ellFx*ellNy)/(ellFy-ellNy) < 0.0 Then αF = αF + 2*P.π
		      Elseif ellFy > 0.0 and ellNy < 0.0 Then // If we are crossing the x axis upward
		        // and if the intercept with the x axis is negative, meaning we are going
		        // from the third quadrant to the second, then ATan jumps from -π to π,
		        // so we subtract2π to compensate
		        If (ellFy*ellNx - ellFx*ellNy)/(ellFy-ellNy) < 0.0 Then αF = αF - 2*P.π
		      End If
		      CosιF = LF.Z  // This is the future value of iota
		    Else
		      CosιF = 1.0 // we are going through vertical
		      αF = 2*αN - αP // Guess that we are going in a reasonably straight line
		    End If
		    αDotN = (αF - αP)/(2*DτF)   // Calculate the present value of αDot
		    
		    // Calculate future values of χs and χa
		    Var onePlusδ As Double = 1.0 + Parameters.δ
		    Var oneMinusδ As Double = 1.0 - Parameters.δ
		    χsF.X = 0.25*(Magχ1*onePlusδ*onePlusδ*χ1HatF.X + Magχ2*oneMinusδ*oneMinusδ*χ2HatF.X)
		    χsF.Y = 0.25*(Magχ1*onePlusδ*onePlusδ*χ1HatF.Y + Magχ2*oneMinusδ*oneMinusδ*χ2HatF.Y)
		    χsF.Z = 0.25*(Magχ1*onePlusδ*onePlusδ*χ1HatF.Z + Magχ2*oneMinusδ*oneMinusδ*χ2HatF.Z)
		    χaF.X = 0.5*(Magχ1*oneMinusδ*χ1HatF.X-Magχ2*onePlusδ*χ2HatF.X)
		    χaF.Y = 0.5*(Magχ1*oneMinusδ*χ1HatF.Y-Magχ2*onePlusδ*χ2HatF.Y)
		    χaF.Z = 0.5*(Magχ1*oneMinusδ*χ1HatF.Z-Magχ2*onePlusδ*χ2HatF.Z)
		    
		    If Parameters.InvDε > 0.0 Then  // We only do this for the base case
		      // This section chooses a time step such that the change in any of the unit
		      // vectors is less than 1/1000 of its magnitude (which is 1).
		      // We only do this for the base case.
		      Var ε As Double = 1.0e-3
		      Var infinity As Double = Double.ToString("INF")
		      Var dτχ1 As Double = infinity
		      Var dτχ2 As Double = infinity
		      Var dτL As Double = infinity
		      // If the magnitudes of the change are not strictly zero, then calculate
		      // what time step would lead to a change of 1/1000
		      Var χ1HatDotMag As Double = Sqrt(χ1HatDotNx*χ1HatDotNx + χ1HatDotNy*χ1HatDotNy + χ1HatDotNz*χ1HatDotNz)
		      If χ1HatDotMag > 0.0 Then dτχ1 = ε/χ1HatDotMag
		      Var χ2HatDotMag As Double = Sqrt(χ2HatDotNx*χ2HatDotNx + χ2HatDotNy*χ2HatDotNy + χ2HatDotNz*χ2HatDotNz)
		      If χ2HatDotMag > 0.0 Then dτχ2 = ε/χ2HatDotMag
		      Var ellDotMag As Double = Sqrt(ellNDotx*ellNDotx + ellNDoty*ellNDoty+ ellNDotz*ellNDotz)
		      If ellDotMag > 0.0 Then dτL = ε/ellDotMag
		      // Then choose the minimum of these values (including the earlier calculation of DτIdeal for the velocity evolution)
		      DτIdeal = Min(dτχ1, dτχ2, dτL, DτIdeal)
		    End If
		  End If
		  
		  // Now evolve the phase
		  Var τr As Double = τ*(1.0 + Parameters.Z) // Calculate the received time
		  Var gMΩeτr As Double = Parameters.GMΩe*τr
		  Var sinOrbit As Double = Sin(gMΩeτr - Φ)
		  Var cosOrbit As Double = Cos(gMΩeτr - Φ)
		  Var LF As Double =2.0*(Log(V/Parameters.V0) + 1.0)
		  Var LF2 As Double = v2*LF
		  Var LF1 As Double = VN*(2.0*LF + 1.0)*vDotN
		  Var ΨrDot As Double = VN - Cosι*αDotN - LF2*vDotN
		  Var stepFactor As Double = 2*DτF*(1.0 + VeSinΘ*sinOrbit)
		  
		  // Calculate new past values using interpolation (note that this effectively does nothing if DτF/DτP = 1,
		  // but it is probably faster just to do the calculation
		  ΨrP = oneMinusRatio*ΨrN + dτRatio*ΨrP
		  
		  // Now update the evolving phase value and its derivatives
		  ΨrF = ΨrP + StepFactor*ΨrDot
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitializeSpins()
		  // This method initializes properties that we will use in evolving the system's spin
		  
		  // Get the the stars' initial spins
		  Var spin1 As New Vector(Parameters.χ10x, Parameters.χ10y, Parameters.χ10z)
		  Var spin2 As New Vector(Parameters.χ20x, Parameters.χ20y, Parameters.χ20z)
		  
		  // calculate the magnitudes of the spin vectors
		  Magχ1 = spin1.GetMagnitude
		  Magχ1 = spin1.GetMagnitude
		  
		  // If its magnitude is not strictly zero, create a unit vector for each spin
		  // But if the magnitude is zero, then the unit vector is also zero
		  if Magχ1 > 0.0 Then
		    χ1HatN = spin1/Magχ1
		  Else
		    χ1HatN = New Vector(0.0, 0.0, 0.0)
		  end if
		  χ1HatP = χ1HatN  // initially, the past is the same as the present
		  χ1HatF = New Vector  // This is just a placeholder so this vector is defined
		  if Magχ2 > 0.0 Then 
		    χ2HatN = spin2/Magχ2
		  Else
		    χ2HatN = New Vector(0.0, 0.0, 0.0)
		  End if
		  χ2HatP = χ2HatN   // Past is the same as present
		  χ2HatF = New Vector  // Placeholder
		  
		  // get some local variables from the parameters
		  Var v0 As Double = Parameters.V0
		  Var η As Double = Parameters.η
		  Var δ As Double = Parameters.δ
		  Var onePlusδ As Double = 1.0 + δ
		  Var oneMinusδ As Double = 1.0 - δ
		  Var plusOverMinus As Double = onePlusδ/oneMinusδ
		  Var minusOverPlus As Double = oneMinusδ/onePlusδ
		  
		  // This value is the inverse magnitude of the L vector  
		  Var B As Double = v0 - (1.5 + η/6.0)*v0^3 - ((27.0-19.0*η)/8.0 + η^2/24.0)*v0^4
		  
		  // This sets up the LHat vector according to equation 12.37 
		  Var ellx As Double = -B*(plusOverMinus*Parameters.χ10x + minusOverPlus*Parameters.χ20x)
		  Var elly As Double = -B*(plusOverMinus*Parameters.χ10y + minusOverPlus*Parameters.χ20y)
		  LN = New Vector(ellx, elly, Sqrt(1.0 - ellx*ellx - elly*elly))  // set the LN vector
		  LP = LN  // Past is the same as the presnet
		  LF = New Vector  // Placeholder
		  
		  // Compute the symmetric and antisymmetric spin vectors and set the parameters
		  χsN = 0.25*(onePlusδ*onePlusδ*spin1 + oneMinusδ*oneMinusδ*spin2)
		  χaN = 0.5*(oneMinusδ*spin1-Magχ2*onePlusδ*spin2)
		  
		  // Compute their projections on the L unit vector and set those parameters
		  χs𝓁 = χsN*LN
		  χa𝓁 = χaN*LN
		  χaF = New Vector
		  χsF = New Vector
		  Var LProj As Double = LN.X*LN.X + LN.Y*LN.Y // squared projection of LHat on xy plane
		  If LProj > 0.0 then // If we don't have exactly zero total spin
		    αN = Atan2(LN.Y,LN.X) // we should be able to define alpha
		    αP = αN  // Past is the same as the present
		    CosιN = LN.Z // and iota based on the projection of LHat on the z axis
		  Else // otherwise, these are the conventions for no spin evolution
		    αN = Parameters.π
		    αP = αN
		    CosιN = 1.0
		  End If
		  
		  // Set up some constants that will be useful for the spin evolution equations.
		  CΩ0 = 0.75 + η/2.0
		  CΩ1 = -0.75*δ
		  CΩ2 = 9.0/16.0 + 1.25*η + η*η/24.0 + 0.675*δ*η
		  CΩ3 = (-9.0/16.0 + 0.675*η)*δ
		  CΩ4 = 27.0/32.0 + 3.0*η/16.0 - 105.0*η*η/32.0 - η*η*η/48.0
		  CΩ5 = (-27.0/32.0 + 39.0*η/8.0 - 5.0*η*η/32.0)*δ
		  CL1 = Magχ1*(1.0 + δ)/(1.0 - δ)
		  CL2 = Magχ2*(1.0 - δ)/(1.0 + δ)
		  CL3 = 1.5 + η/6.0
		  CL4 = 27.0/8.0 - 19.0*η/8.0 + η*η/24.0
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub MakeFuturePresent()
		  // Make the future step the present step, and the present step the past step
		  VP = VN
		  VN = VF
		  ιN = ιF
		  LP.X = LN.X
		  LP.Y = LN.Y
		  LP.Z = LN.Z
		  LN.X = LF.X
		  LN.Y = LF.Y
		  LN.Z = LF.Z
		  αP = αN
		  αN = αF
		  χ1HatP.X =χ1HatN.X
		  χ1HatP.Y =χ1HatN.Y
		  χ1HatP.Z =χ1HatN.Z
		  χ1HatN.X = χ1HatF.X
		  χ1HatN.Y = χ1HatF.Y
		  χ1HatN.Z = χ1HatF.Z
		  χ2HatP.X = χ2HatN.X
		  χ2HatP.Y = χ2HatN.Y
		  χ2HatP.Z = χ2HatN.Z
		  χ2HatN.X = χ2HatF.X
		  χ2HatN.Y = χ2HatF.Y
		  χ2HatN.Z = χ2HatF.Z
		  χaN.X = χaF.X
		  χaN.Y = χaF.Y
		  χaN.Z = χaF.Z
		  χsN.X = χsF.X
		  χsN.Y = χsF.Y
		  χsN.Z = χsF.Z
		  ΨrP = ΨrN
		  ΨrN = ΨrF
		  DΨrDΘP = DΨrDΘN
		  DΨrDΘN = DΨrDΘF
		  DΨrDΦP = DΨrDΘN
		  DΨrDΦN = DΨrDΘF
		  
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		A(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		BaseCase As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		C(10) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CAD0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CAD1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CAD2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CL1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CL2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CL3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CL4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CL5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CL5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CL5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CL5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CosιF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CosιMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CosιN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CSD0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CSD1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CSD2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CV2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CV3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CV4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CV5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CV6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CV6L As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CV7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Cα(5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Cβ(5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CΨ(5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CΩ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CΩ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CΩ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CΩ3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CΩ4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CΩ5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CΩ6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DWDΨ(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DτIdeal As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dτr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		H As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IsBaseCase As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		LF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		LN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		LP As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		Magχ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Magχ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		S(10) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sα(5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sβ(5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SΨ(5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		UaeBasePhase As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		UseBaseAmplitude As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		VeSinΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		W(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αDotN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ε As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		τ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatP As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatP As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χaF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χaMN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χaN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χa𝓁 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χsMN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χsN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χs𝓁 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrP As Double
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
			Name="ε"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Dτr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="τ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Magχ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Magχ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CosιF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CosιN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αDotN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χa𝓁"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χs𝓁"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CL1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CL2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CL3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CL4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CΩ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CΩ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CΩ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CΩ3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CΩ4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CΩ5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CL5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CΩ6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VeSinΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CV2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CV3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CV4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CV5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CV6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CV6L"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CV7"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CAD0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CAD1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CAD2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CSD0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CSD1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CSD2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DτIdeal"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CosιMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="UseBaseAmplitude"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="UaeBasePhase"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
