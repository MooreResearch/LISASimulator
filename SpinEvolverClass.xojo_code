#tag Class
Protected Class SpinEvolverClass
	#tag Method, Flags = &h0
		Sub Constructor(Parameters As CaseParametersClass, Dτ0 As Double)
		  P = Parameters  // Store a reference to this case's parameter list
		  // calculate the magnitudes of the spin vectors
		  Magχ1 = Sqrt(P.χ10x^2 + P.χ10y^2 + P.χ10z^2)
		  Magχ2 = Sqrt(P.χ20x^2 + P.χ20y^2 + P.χ20z^2)
		  // If its magnitude is not strictly zero, create a unit vector for each spin
		  // But if the magnitude is zero, then the unit vector is also zero
		  if Magχ1 > 0.0 Then
		    χ1HatN = New Vector(P.χ10x/Magχ1, P.χ10y/Magχ1, P.χ10z/Magχ1)
		  Else
		    χ1HatN = New Vector(0.0, 0.0, 0.0)
		  end if
		  χ1HatP = χ1HatN
		  χ1HatF = New Vector  // This is just a placeholder so this vector is defined
		  if Magχ2 > 0.0 Then 
		    χ2HatN = New Vector(P.χ20x/Magχ2, P.χ20y/Magχ2, P.χ20z/Magχ2)
		  Else
		    χ2HatN = New Vector(0.0, 0.0, 0.0)
		  End if
		  χ2HatP = χ2HatN
		  χ2HatF = New Vector
		  // The following method calculates the orbital angular momentum unit vector L,
		  // the antisymmetric spin sum χa, the symmetric spin sum χs, and their projections
		  // χa𝓁 and χa𝓁 on the L direction. Note that the last five parameters of the method are
		  // passed by reference so that we can return the five calculated values at once.
		  InitializeSpins(P, χs𝓁, χa𝓁, LN, χsN, χaN)
		  LP = LN
		  LF = New Vector
		  χa0 = χaN
		  χaF = New Vector
		  χs0 = χsN
		  χsF = New Vector
		  Var LProj As Double = LN.x^2 + LN.y^2 // squared projection of LHat on xy plane
		  If LProj > 0.0 then // If we don't have exactly zero total spin
		    αN = Atan2(LN.y,LN.x) // we should be able to define alpha
		    αP = αN
		    ιN = ACos(LN.z) // and iota based on the projection of LHat on the z axis
		  Else // otherwise
		    αN = P.π
		    αP = αN
		    ιN = 0.0
		  End If
		  // Set up some constants that will be useful for the evolution equations.
		  Var δ As Double = P.δ
		  Var η As Double = P.η
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
		  Var s As String = "INF"
		  Infinity = s.ToDouble
		  InverseOnePlusZ = 1.0/(1.0 + Parameters.Z)
		  
		  // Calculate the derivatives of χs𝓁, and χa𝓁
		  Setχ𝓁Derivatives
		  
		  // Do a trial first step to calculate the ideal time step for the actual first step
		  // (Note that doing a half-step with past values set to present values is
		  // equivalent to doing an Euler step.)
		  DoStep(0.5*Dτ0, 0.5*Dτ0, Parameters.V0)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoStep(DτP as Double, DτF as Double, VN as Double)
		  // This method serves a similar purpose to the DoStep method of EvolverClass, but it evolves just the spin variables.
		  // This stepper takes DτF, the future time step, DτP the past time step, and the current value of V and it solves for
		  // the "future" value of each variable.
		  
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
		    DαDZ = 0.0
		    DιDZ = 0.0
		  Else // spins are not strictly zero
		    Var dτRatio As Double = DτF/DτP // calculate this ratio once so we don't have to do it many times
		    Var oneMinusRatio As Double = 1.0 - dτRatio // Calculate this only once also
		    
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
		    
		    // Calculate local versions of powers of the current value of v
		    Var vN2 As Double = VN*VN
		    Var vN4 As Double = vN2*vN2
		    Var vN5 As Double = vN4*VN
		    
		    // Do the step
		    // Evolve the two spins using the leapfrog method
		    Var Factor As Double = vN5*(CΩ0 + CΩ1 + (CΩ2 + CΩ3)*vN2 + (CΩ4 + CΩ5)*vN4)*2*DτF
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
		    Factor = -VN*(1-CL3*vN2-CL4*vN4)*2*DτF
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
		      ιF = ACos(LF.Z)  // This is the future value of iota
		    Else
		      ιF = 0.0 // we are going through vertical
		      αF = 2*αN - αP // Guess that we are going in a reasonably straight line
		    End If
		    αDotN = (αF - αP)/(2*DτF)   // Calculate the present value of αDot
		    DαDZ = -(αN-α0)*InverseOnePlusZ  // and the present values of these derivatives
		    DιDZ = -(ιN-ι0)*InverseOnePlusZ
		    
		    // This section chooses a time step such that the change in any of the unit
		    // vectors is less than 1/1000 of its magnitude (which is 1).
		    Var ε As Double = 1.0e-3
		    Var Dτχ1 As Double = Infinity
		    Var Dτχ2 As Double = Infinity
		    Var DτL As Double = Infinity
		    // If the magnitudes of the change are not strictly zero, then calculate
		    // what time step would lead to a change of 1/1000
		    Var χ1HatDotMag As Double = Sqrt(χ1HatDotNx*χ1HatDotNx + χ1HatDotNy*χ1HatDotNy + χ1HatDotNz*χ1HatDotNz)
		    If χ1HatDotMag > 0.0 Then Dτχ1 = ε/χ1HatDotMag
		    Var χ2HatDotMag As Double = Sqrt(χ2HatDotNx*χ2HatDotNx + χ2HatDotNy*χ2HatDotNy + χ2HatDotNz*χ2HatDotNz)
		    If χ2HatDotMag > 0.0 Then Dτχ2 = ε/χ2HatDotMag
		    Var ellDotMag As Double = Sqrt(ellNDotx*ellNDotx + ellNDoty*ellNDoty+ ellNDotz*ellNDotz)
		    If ellDotMag > 0.0 Then DτL = ε/ellDotMag
		    // Then choose the minimum of these values
		    DτIdeal = Min(Dτχ1, Dτχ2, DτL)
		    
		    // Calculate future values of χs and χa
		    χsF.X = 0.25*(Magχ1*onePlusδ*onePlusδ*χ1HatF.X + Magχ2*oneMinusδ*oneMinusδ*χ2HatF.X)
		    χsF.Y = 0.25*(Magχ1*onePlusδ*onePlusδ*χ1HatF.Y + Magχ2*oneMinusδ*oneMinusδ*χ2HatF.Y)
		    χsF.Z = 0.25*(Magχ1*onePlusδ*onePlusδ*χ1HatF.Z + Magχ2*oneMinusδ*oneMinusδ*χ2HatF.Z)
		    χaF.X = 0.5*(Magχ1*oneMinusδ*χ1HatF.X-Magχ2*onePlusδ*χ2HatF.X)
		    χaF.Y = 0.5*(Magχ1*oneMinusδ*χ1HatF.Y-Magχ2*onePlusδ*χ2HatF.Y)
		    χaF.Z = 0.5*(Magχ1*oneMinusδ*χ1HatF.Z-Magχ2*onePlusδ*χ2HatF.Z)
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitializeSpins(PL As CaseParametersClass, ByRef XsL As Double, ByRef XaL As Double, ByRef L As Vector, ByRef Xs As Vector, ByRef Xa As Vector)
		  // get some local variables from the parameters
		  Var v0 As Double = PL.V0
		  Var η As Double = PL.η
		  Var δ As Double = PL.δ
		  Var onePlusδ As Double = 1.0 + δ
		  Var oneMinusδ As Double = 1.0 - δ
		  Var plusOverMinus As Double = onePlusδ/oneMinusδ
		  Var minusOverPlus As Double = oneMinusδ/onePlusδ
		  // Get the the stars' spins
		  Var spin1 As New Vector(PL.χ10x, PL.χ10y, PL.χ10z)
		  Var spin2 As New Vector(PL.χ20x, PL.χ20y, PL.χ20z)
		  // This value is the inverse magnitude of the L vector  
		  Var B As Double = v0 - (1.5 + η/6.0)*v0^3 - ((27.0-19.0*η)/8.0 + η^2/24.0)*v0^4
		  // This sets up the LHat vector according to equation 12.37 
		  Var ellx As Double = -B*(plusOverMinus*PL.χ10x + minusOverPlus*P.χ20x)
		  Var elly As Double = -B*(plusOverMinus*P.χ10y + minusOverPlus*P.χ20y)
		  L = New Vector(ellx, elly, Sqrt(1.0 - ellx*ellx - elly*elly))  // set the L parameter
		  // Compute the symmetric and antisymmetric spin vectors and set the parameters
		  Xs = 0.25*(onePlusδ*onePlusδ*spin1 + oneMinusδ*oneMinusδ*spin2)
		  Xa = 0.5*(oneMinusδ*spin1-Magχ2*onePlusδ*spin2)
		  // Compute their projections on the L unit vector and set those parameters
		  XsL = Xs*L
		  XaL = Xa*L
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub MakeFuturePresent()
		  // Make the future step the present step, and the present step the past step
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
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OldFindα()
		  '//This method takes the past and present value of α, and returns its cumulative value.
		  '
		  'Var α As Double //Creates the variable which will be returned at the end 
		  '
		  'if αP = 0 then
		  'return 0
		  '
		  'elseif αF = 0 then
		  'break
		  'return 0
		  'end if
		  '
		  '// check if the current α is negative
		  'Var αFnew As Double  // create a new variable to be the always-positive α
		  'If αF < 0 Then
		  'αFnew = 2*π + αF  // if α is negative, add 2π to make it positive
		  'Else
		  'αFnew = αF  // otherwise, keep it the same
		  'End If
		  '
		  '// check if the past α is negative
		  'Var αPnew As Double // create a new variable to be the always-positive previous α
		  'If αP < 0 Then
		  'αPnew = 2*π + αP  // if the previous α is negative, add 2π to make it positive
		  'Else
		  'αPnew = αP  // otherwise, keep it the same
		  'End If
		  '
		  '
		  'If αPnew > αFnew and SignChangedLastTime = False Then  // if we have just crossed the x-axis from the 4th to 1st quadrant
		  'FullTurns = FullTurns + 1  // then we have completed a full turn
		  'SignChangedLastTime = True  // and we have changed "sign" from negative to positive
		  'ElseIf αPnew < αFnew and SignChangedLastTime = True Then  // if we have already crossed the x-axis but the α values are still split
		  'SignChangedLastTime = False  // update that we haven't changed signs last time to avoid double counting
		  'End If
		  '
		  'α = αFnew + FullTurns*2*π  // make the return variable the accumulated value of α
		  '
		  'return α
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Setχ𝓁DerivativePair(Which As CaseParametersClass.Item, ByRef Dχs𝓁 As Double, ByRef Dχa𝓁 As Double)
		  // This method calculates the derivatives of χs𝓁 and χa𝓁 by running pairs
		  // of calculations with tweaked parameter values.
		  // Note that the final two parameters of this method are passed by reference
		  // so that the items passed can be modified directly. This allows us to return
		  // two values at once.
		  Var ε As Double = 1.0e-6
		  // The following a dummy vector, which we are creating because the InitializeSpins
		  // method returns three vectors, which we don't care about in this case
		  Var d As New Vector(0,0,0)
		  Var χs𝓁Plus As Double
		  Var χa𝓁Plus As Double
		  InitializeSpins(P.GetTweaked(Which, ε), χs𝓁Plus, χa𝓁Plus, d, d, d)
		  Var χs𝓁Minus As Double
		  Var χa𝓁Minus As Double
		  InitializeSpins(P.GetTweaked(Which, -ε), χs𝓁Minus, χa𝓁Minus, d, d, d)
		  Dχs𝓁 = 0.5*(χs𝓁Plus - χs𝓁Minus)/ε
		  Dχa𝓁 = 0.5*(χa𝓁Plus - χa𝓁Minus)/ε
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Setχ𝓁Derivatives()
		  // Call the Setχ𝓁DerivativePair method to calculate the pair of derivatives
		  // with respect to each parameter. Note that the final two parameter for the
		  // Setχ𝓁DerivativePair method are passed by reference, so it returns the
		  // derivative values through those parameters.
		  Setχ𝓁DerivativePair(CaseParametersClass.Item.χ10x, DχsDχ10x, DχaDχ10x)
		  Setχ𝓁DerivativePair(CaseParametersClass.Item.χ10y, DχsDχ10y, DχaDχ10y)
		  Setχ𝓁DerivativePair(CaseParametersClass.Item.χ10z, DχsDχ10z, DχaDχ10z)
		  Setχ𝓁DerivativePair(CaseParametersClass.Item.χ20x, DχsDχ20x, DχaDχ20x)
		  Setχ𝓁DerivativePair(CaseParametersClass.Item.χ20y, DχsDχ20y, DχaDχ20y)
		  Setχ𝓁DerivativePair(CaseParametersClass.Item.χ20z, DχsDχ20z, DχaDχ20z)
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
		DιDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DτIdeal As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Infinity As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		InverseOnePlusZ As Double
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
		OneMinusδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		OnePlusδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		P As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		ι0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ιF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ιN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		α0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αDotN As Double
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
		χa0 As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χaF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χaN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χa𝓁 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χs0 As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χsF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χsN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χs𝓁 As Double
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
			Name="αF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
			Name="αN"
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
			Name="DτIdeal"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Infinity"
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
			Name="αP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ι0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="α0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DιDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="InverseOnePlusZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
