#tag Class
Protected Class SourceEvolverClass
	#tag Method, Flags = &h0
		Sub Constructor(P As CaseParametersClass)
		  π = P.π
		  // Initialize the velocity-related properties
		  VN = P.V0
		  VP = VN
		  V0 = VN
		  // Initialize spin-replated quantities
		  χ1 = Sqrt(P.χ10x*P.χ10x + P.χ10y*P.χ10y + P.χ10z*P.χ10z)
		  χ2 = Sqrt(P.χ20x*P.χ20x + P.χ20y*P.χ20y + P.χ20z*P.χ20z)
		  If χ1 <> 0.0 Then
		    χ1HatXN = P.χ10x/χ1
		    χ1HatYN = P.χ10y/χ1
		    χ1HatZN = P.χ10z/χ1
		    χ1HatXP = χ1HatXN
		    χ1HatYP = χ1HatYN
		    χ1HatZP = χ1HatZN
		  End If
		  If χ2 <> 0.0 Then
		    χ2HatXN = P.χ20x/χ2
		    χ2HatYN = P.χ20y/χ2
		    χ2HatZN = P.χ20z/χ2
		    χ2HatXP = χ2HatXN
		    χ2HatYP = χ2HatYN
		    χ2HatZP = χ2HatZN
		  End If
		  Var δ As Double = P.δ
		  Var η As Double = 0.25*(1.0 - δ*δ)
		  OnePlusδ = 1.0 + δ
		  OneMinusδ = 1.0 - δ
		  Var plusOverMinus As Double = OnePlusδ/OneMinusδ
		  Var minusOverPlus As Double = OneMinusδ/OnePlusδ
		  
		  // Cosmic redshift factor
		  Inv1PlusZ = 1.0/(1.0 + P.Z)
		  
		  // This value is the inverse magnitude of the L vector  
		  Var B As Double = VN - (1.5 + η/6.0)*VN*VN*VN - ((27.0-19.0*η)/8.0 + η*η/24.0)*VN*VN*VN*VN
		  
		  // This sets up the LHat vector according to equation 12.37 
		  LXN = -B*(plusOverMinus*P.χ10x + minusOverPlus*P.χ20x)
		  LYN = -B*(plusOverMinus*P.χ10y + minusOverPlus*P.χ20y)
		  LZN = Sqrt(1.0 - LXN*LXN- LYN*LYN)
		  LXP = LXN
		  LYP = LYN
		  LZP = LZN
		  
		  // Compute the symmetric and antisymmetric spin vectors and set the parameters
		  
		  χsXN = 0.25*(OnePlusδ*OnePlusδ*P.χ10x + OneMinusδ*OneMinusδ*P.χ20x)
		  χsYN = 0.25*(OnePlusδ*OnePlusδ*P.χ10y + OneMinusδ*OneMinusδ*P.χ20y)
		  χsZN = 0.25*(OnePlusδ*OnePlusδ*P.χ10z + OneMinusδ*OneMinusδ*P.χ20z)
		  χaXN = 0.5*(OneMinusδ*P.χ20x-OnePlusδ*P.χ10x)
		  χaYN = 0.5*(OneMinusδ*P.χ20y-OnePlusδ*P.χ10y)
		  χaZN = 0.5*(OneMinusδ*P.χ20z-OnePlusδ*P.χ10z)
		  
		  χsXP = χsXN
		  χsYP = χsYN 
		  χsZP= χsZN
		  χaXP = χaXN
		  χaYP = χaYN
		  χaZP = χaZN
		  
		  // Compute their projections on the L unit vector and set those parameters
		  χsL = χsXN*LXN + χsYN*LYN + χsZN*LZN
		  χaL = χaXN*LXN + χaYN*LYN + χaZN*LZN
		  
		  Var LProj As Double = LXN*LXN + LYN*LYN // squared projection of LHat on xy plane
		  If LProj > 0.0 then // If we don't have exactly zero total spin
		    αN = Atan2(LYN,LXN) // we should be able to define alpha
		    αP = αN  // Past is the same as the present
		  Else // otherwise, these are the conventions for no spin evolution
		    αN = π
		    αP = αN
		    LZN = 1.0
		    LZP = 1.0
		  End If
		  
		  // Initialize constants
		  Var γE As Double = 0.5772156649015328606
		  CV0 = 32*η/5
		  CV2 = -743/336 - 11*η/4
		  CV3 = 4*π - 47*χsL/3 - δ*25*χaL/4
		  CV4 = 34103/18144 + 13661*η/2016 + 59*η*η/18
		  CV5 = (-5861/144 + 1001*η/12)*χsL + δ*(-809/84 + 281*η/8)*χaL _
		  + 4159*π/672 + 189*π*η/8
		  CV6 = 16477322263.0/139708800 - 1712*γE/105 + 16*π*π/3 _
		  + (-56198689/217728 + 451*π*π/48)*η _
		  + 541*η*η/896 - 5605*η*η*η/2592 - 856*Log(32)/105
		  CV6L = 856/105
		  CV7 = π*(-4415/4032 + 358675*η/6048 + 91495*η*η/1512)
		  
		  // Initialize constants for spin evolution
		  // Set up some constants that will be useful for the spin evolution equations.
		  CΩ0 = 0.75 + η/2.0 - 0.75*δ
		  CΩ1 = 0.75 + η/2.0 + 0.75*δ
		  CΩ2 = 9.0/16.0 + 1.25*η + η*η/24.0 + 0.675*δ*η + (-9.0/16.0 + 0.675*η)*δ
		  CΩ3 = 9.0/16.0 + 1.25*η + η*η/24.0 - 0.675*δ*η - (-9.0/16.0 + 0.675*η)*δ
		  CΩ4 = 27.0/32.0 + 3.0*η/16.0 - 105.0*η*η/32.0 - η*η*η/48.0 + (-27.0/32.0 + 39.0*η/8.0 - 5.0*η*η/32.0)*δ
		  CΩ5 = 27.0/32.0 + 3.0*η/16.0 - 105.0*η*η/32.0 - η*η*η/48.0 - (-27.0/32.0 + 39.0*η/8.0 - 5.0*η*η/32.0)*δ
		  CL1 = (1.0 + δ)/(1.0 - δ)
		  CL2 = (1.0 - δ)/(1.0 + δ)
		  CL3 = 1.5 + η/6.0
		  CL4 = 27.0/8.0 - 19.0*η/8.0 + η*η/24.0
		  
		  // Initialize phase stuff
		  λN = P.λ0
		  λP = λN
		  ΨN = P.λ0
		  ΨP = ΨN
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoStep(DτrP As Double, DτrF As Double, ByRef DτIdeal As Double)
		  // Since a past time step less than or equal to zero is impossible, we are using that value as a flag
		  // that this is either a test step (DτrP < 0) or a first step (DτrP = 0).
		  // Note that these time intervals are solar system times.
		  Var testStep As Boolean = (DτrP < 0.0)
		  Var eulerStep As Boolean = testStep or (DτrP = 0.0)
		  Var dτRatio As Double
		  Var twoDτF As Double = 2.0*DτrF*Inv1PlusZ  // this is the future time step corrected for the source frame
		  If eulerStep Then
		    dτRatio = 1.0
		    twoDτF = 0.5*twoDτF  // Test or first step is an Euler step, so half the usual step size
		  Else
		    dτRatio = DτrF/DτrP
		  End If
		  
		  Var oneMinusRatio As Double = 1.0 - dτRatio
		  // Calculate new past values using interpolation (note that this effectively does nothing if DτRatio = 1,
		  // but it is probably faster just to do the calculation than to do a check and then a calculation
		  VPold = VP // Save the old value for interpolation purposes
		  VP = oneMinusRatio*VN + dτRatio*VP
		  
		  // Evolve the velocity forward
		  Var v2 As Double = VN*VN
		  Var v3 As Double = v2*VN
		  Var v4 As Double = v2*v2
		  Var v5 As Double = v2*v3
		  Var v6 As Double = v3*v3
		  Var v7 As Double = v3*v4
		  Var v9 As Double = v4*v5
		  Var vDotN As Double = CV0*v9*(1 + CV2*v2 + CV3*v3 + CV4*v4 + CV5*v5 + (CV6 + CV6L*Log(VN))*v6 + CV7*v7)
		  VF = VP + twoDτF*vDotN
		  Var ε As Double = 1.0e-3  // define what the maximum allowable change during a step should be
		  DτIdeal = Min(DτIdeal, ε/Abs(vDotN))  // Calculate the ideal next step (we will only pay attention to the base case value)
		  
		  Var αDotN As Double
		  // Now we will do the spin evolution
		  If χ1 = 0.0 and χ2 = 0.0 Then // If spins are both strictly zero, then there is no evolution
		    χ1HatXF = χ1HatXN
		    χ2hatXF = χ2HatXN
		    χ1HatYF = χ1HatYN
		    χ2hatYF = χ2HatYN
		    χ1HatZF = χ1HatZN
		    χ2hatZF = χ2HatZN
		    LXF = LXN
		    LYF = LYN
		    LZF = LZN
		    αF = αN
		    ιF = ιN
		    αDotN = 0.0
		    χsXF = χsXN
		    χaXF = χaXN
		    χsYF = χsYN
		    χaYF = χaYN
		    χsZF = χsZN
		    χaZF = χaZN
		    αF = αN
		    ιF = 0.0
		  Else // spins are not strictly zero
		    // Calculate new past values using interpolation (note that this effectively does nothing if DτRatio = 1,
		    // but it is probably faster just to do the calculation
		    // Note that we are calculating components directly rather than defining a vector class and
		    // doing operations via operator overloading because the overhead is with the latter is huge and
		    // we also do not want to be creating any new objects while we are looping, as that is also slow.
		    
		    χ1hatXP = OneMinusRatio*χ1hatXN + dτRatio*χ1hatXP  
		    χ1hatYP = OneMinusRatio*χ1hatYN + dτRatio*χ1hatYP
		    χ1hatZP = OneMinusRatio*χ1hatZN + dτRatio*χ1hatZP  
		    χ2hatXP = OneMinusRatio*χ2hatXN + dτRatio*χ2hatXP  
		    χ2hatYP = OneMinusRatio*χ2hatYN + dτRatio*χ2hatYP
		    χ2hatZP = OneMinusRatio*χ2hatZN + dτRatio*χ2hatZP  
		    LXP = OneMinusRatio*LXN + dτRatio*LXP
		    LYP = OneMinusRatio*LYN + dτRatio*LYP
		    LZP = OneMinusRatio*LZN + dτRatio*LZP
		    αPold = αP  // Save the old value for interpolation purposes
		    αP = OneMinusRatio*αN + dτRatio*αP
		    
		    // Do the step
		    // Evolve the two spins using the leapfrog method
		    Var Factor As Double = v5*(CΩ0  + CΩ2*v2 + CΩ4*v4)
		    Var χ1HatDotNx As Double = Factor*(LYN*χ1HatZN - LZN*χ1HatYN)
		    Var χ1HatDotNy As Double = Factor*(LZN*χ1HatXN - LXN*χ1HatZN)
		    Var χ1HatDotNz As Double = Factor*(LXN*χ1HatYN - LYN*χ1HatXN)
		    χ1HatXF = χ1HatXP + χ1HatDotNx*twoDτF
		    χ1HatYF = χ1HatYP + χ1HatDotNy*twoDτF
		    χ1HatZF = χ1HatZP + χ1HatDotNz*twoDτF
		    Factor = v5*(CΩ1  + CΩ3*v2 + CΩ5*v4)
		    Var χ2HatDotNx As Double = Factor*(LYN*χ2HatZN - LZN*χ2HatYN)
		    Var χ2HatDotNy As Double = Factor*(LZN*χ2HatXN - LXN*χ2HatZN)
		    Var χ2HatDotNz As Double = Factor*(LXN*χ2HatYN - LYN*χ2HatXN)
		    χ2HatXF = χ2HatXP + χ2HatDotNx*twoDτF
		    χ2HatYF = χ2HatYP + χ2HatDotNy*twoDτF
		    χ2HatZF = χ2HatZP + χ2HatDotNz*twoDτF
		    
		    // Calculate the future value of the orbital angular momentum
		    Factor = -(VN-CL3*v3-CL4*v4)
		    Var Factor1 As Double = Factor*CL1*χ1
		    Var Factor2 As Double = Factor*CL2*χ2
		    LXF = Factor1*χ1HatXF + Factor2*χ2HatXF
		    LYF = Factor1*χ1HatYF + Factor2*χ2HatYF
		    LZF = Sqrt(1.0 - LXF*LXF - LYF*LYF)
		    // Note that the above could possibly be NaN if LXF^2 + LYF^2 > 1,
		    // but this is not likely, as the spin angular momenta should be way
		    // less than the orbital angular momentum
		    
		    // Calculate the future angles
		    Var ellHF As Double = Sqrt(LXF*LXF+LYF*LYF)
		    If ellHF > 1.0e-12 Then
		      // The future L vector points at least some angle away from the vertical,
		      // so α is well-defined and we can calculate it normally
		      αF = ATan2(LYF, LXF)
		      // To keep α from jumping in value when the L vector crosses the x axis,
		      // we need to adjust its value from what the ATan2 function gives us
		      If LYF < 0.0 and LYN > 0.0 Then // If we are crossing the x axis downward
		        // and if the intercept with the x axis is negative, meaning we are going
		        // from the second quadrant to the third, then ATan jumps from π to -π,
		        // so we add 2π to compensate
		        If (LYF*LXN - LXF*LYN)/(LYF-LYN) < 0.0 Then αF = αF + 2*π
		      Elseif LYF > 0.0 and LYN < 0.0 Then // If we are crossing the x axis upward
		        // and if the intercept with the x axis is negative, meaning we are going
		        // from the third quadrant to the second, then ATan jumps from -π to π,
		        // so we subtract2π to compensate
		        If (LYF*LXN - LXF*LYN)/(LYF-LYN) < 0.0 Then αF = αF - 2*π
		      End If
		      ιF = ATan2(ellHF,LZF)  // This is the future value of iota
		    Else
		      ιF = 0.0 // we are going through vertical
		      αF = 2*αN - αP // Guess that we are going in a reasonably straight line
		    End If
		    αDotN = (αF - αP)/twoDτF // Calculate the present value of αDot
		    
		    // Calculate future values of χs and χa
		    χsXF = 0.25*(χ1*OnePlusδ*OnePlusδ*χ1HatXF + OneMinusδ*OneMinusδ*χ2HatXF)
		    χsYF = 0.25*(χ1*OnePlusδ*OnePlusδ*χ1HatYF + OneMinusδ*OneMinusδ*χ2HatYF)
		    χsZF = 0.25*(χ1*OnePlusδ*OnePlusδ*χ1HatZF + OneMinusδ*OneMinusδ*χ2HatZF)
		    χaXF = 0.5*(χ2*OneMinusδ*χ2HatXF-χ1*OnePlusδ*χ1HatXF)
		    χaYF = 0.5*(χ2*OneMinusδ*χ2HatYF-χ1*OnePlusδ*χ1HatYF)
		    χaZF = 0.5*(χ2*oneMinusδ*χ2HatZF-χ1*OnePlusδ*χ1HatZF)
		    
		    // This section chooses a time step such that the change in any of the unit
		    // vectors is less than 1/100 of its magnitude (which is 1).
		    ε = 0.01
		    Var χ1HatDotMag As Double = Sqrt(χ1HatDotNx*χ1HatDotNx + χ1HatDotNy*χ1HatDotNy + χ1HatDotNz*χ1HatDotNz)
		    If χ1HatDotMag > 0.0 Then DτIdeal= Min(DτIdeal, ε/χ1HatDotMag)
		    Var χ2HatDotMag As Double = Sqrt(χ2HatDotNx*χ2HatDotNx + χ2HatDotNy*χ2HatDotNy + χ2HatDotNz*χ2HatDotNz)
		    If χ2HatDotMag > 0.0 Then DτIdeal= Min(DτIdeal, ε/χ2HatDotMag)
		    Var ellDotMag As Double = Sqrt((LXF-LXP)*(LXF-LXP) + (LYF-LYP)+ (LZF-LZP)*(LZF-LZP))/twoDτF
		    If ellDotMag > 0.0 Then DτIdeal= Min(DτIdeal, ε/ellDotMag)
		  End If
		  
		  // Now evolve the source phase
		  If eulerStep Then  // if this an Euler step
		    v3 = 0.5*(VN + VF)
		    v3 = v3*v3*v3
		    λF = λN + twoDτF*(v3 - 0.5*(LZN+LZF)*(αF-αN)) // (This is actually 2nd-order accurate!)
		  Else
		    λF = λP + twoDτF*(v3 - LZN*αDotN)
		  End If
		  ΨF = λF - 6.0*VF*VF*VF*Log(VF/V0)
		  
		  // The step just taken now becomes the present
		  // (as long as it is not a test step)
		  If not testStep Then
		    VP = VN
		    VN = VF
		    ιP = ιN
		    ιN = ιF
		    LXP = LXN
		    LYP = LYN
		    LZP = LZN
		    LXN = LXF
		    LYN = LYF
		    LZN = LZF
		    αP = αN
		    αN = αF
		    χ1HatXP =χ1HatXN
		    χ1HatYP =χ1HatYN
		    χ1HatZP =χ1HatZN
		    χ1HatXN = χ1HatXF
		    χ1HatYN = χ1HatYF
		    χ1HatZN = χ1HatZF
		    χ2HatXP = χ2HatXN
		    χ2HatYP = χ2HatYN
		    χ2HatZP = χ2HatZN
		    χ2HatXN = χ2HatXF
		    χ2HatYN = χ2HatYF
		    χ2HatZN = χ2HatZF
		    χaXP = χaXN
		    χaYP = χaYN
		    χaZP = χaZN
		    χaXN = χaXF
		    χaYN = χaYF
		    χaZN = χaZF
		    χsXP = χsXN
		    χsYP = χsYN
		    χsZP = χsZN
		    χsXN = χsXF
		    χsYN = χsYF
		    χsZN = χsZF
		    ΨP = ΨN
		    ΨN = ΨF
		  End If
		End Sub
	#tag EndMethod


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
		CV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CV1 As Double
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
		Inv1PlusZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LXF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LXN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LXP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LYF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LYN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LYP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LZF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LZN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LZP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		OneMinusδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		OnePlusδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VPold As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ιF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ιN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ιP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αPold As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		λF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		λN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		λP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		π As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatXF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatXN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatXP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatYF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatYN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatYP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatZF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatZN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1HatZP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatXF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatXN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatXP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatYF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatYN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatYP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatZF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatZN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2HatZP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaL As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaXF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaXN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaXP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaYF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaYN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaYP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaZF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaZN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaZP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsL As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsXF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsXN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsXP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsYF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsYN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsYP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsZF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsZN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsZP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨP As Double
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
			Name="ιF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ιN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ιP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LXF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LYF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LZF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LXN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LYN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LZN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LXP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LYP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LZP"
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
			Name="αP"
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
			Name="χ1HatXF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ1HatXN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ1HatXP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ1HatYF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ1HatYN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ1HatYP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ1HatZF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ1HatZP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ1HatZN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2HatXF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2HatXN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2HatXP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2HatYF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2HatYN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2HatYP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2HatZF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2HatZN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2HatZP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨP"
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
			Name="CV1"
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
			Name="CV7"
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
			Name="χ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2"
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
			Name="CΩ2"
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
			Name="χaL"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsL"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="OneMinusδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="OnePlusδ"
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
			Name="π"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaXF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaXN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaXP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaYF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaYN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaYP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaZF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaZN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsXF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsXN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsXP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsYF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsYN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsYP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsZF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsZN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsZP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αPold"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaZP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VPold"
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
			Name="CΩ3"
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
			Name="Inv1PlusZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="λF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="λN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="λP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
