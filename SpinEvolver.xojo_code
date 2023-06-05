#tag Class
Protected Class SpinEvolver
	#tag Method, Flags = &h0
		Function AccumulatingAtan(αP As Double, αF As Double) As Double
		  //This method takes the past and present value of α, and returns its cumulative value.
		  
		  Var α As Double //Creates the variable which will be returned at the end 
		  
		  if αP = 0 then
		    return 0
		    
		  elseif αF = 0 then
		    break
		    return 0
		  end if
		  
		  // check if the current α is negative
		  Var αFnew As Double  // create a new variable to be the always-positive α
		  If αF < 0 Then
		    αFnew = 2*π + αF  // if α is negative, add 2π to make it positive
		  Else
		    αFnew = αF  // otherwise, keep it the same
		  End If
		  
		  // check if the past α is negative
		  Var αPnew As Double // create a new variable to be the always-positive previous α
		  If αP < 0 Then
		    αPnew = 2*π + αP  // if the previous α is negative, add 2π to make it positive
		  Else
		    αPnew = αP  // otherwise, keep it the same
		  End If
		  
		  
		  If αPnew > αFnew and SignChangedLastTime = False Then  // if we have just crossed the x-axis from the 4th to 1st quadrant
		    FullTurns = FullTurns + 1  // then we have completed a full turn
		    SignChangedLastTime = True  // and we have changed "sign" from negative to positive
		  ElseIf αPnew < αFnew and SignChangedLastTime = True Then  // if we have already crossed the x-axis but the α values are still split
		    SignChangedLastTime = False  // update that we haven't changed signs last time to avoid double counting
		  End If
		  
		  α = αFnew + FullTurns*2*π  // make the return variable the accumulated value of α
		  
		  return α
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CheckLNHat(LNHat As Vector)
		  // This method makes sure that the magnitude of LNHat never evolves to be greater than one. If a component has done so, we set it to
		  // the maximum allowed value based on the other two components.
		  
		  Var value As Double
		  
		  If LNhat.getZ > 1 Then
		    value = Sqrt(1 - LNHat.getX^2 - LNHat.getY^2) 
		    LNhat.setZ(value)
		  End If
		  
		  If LNhat.getX > 1 Then
		    value = Sqrt(1 - LNHat.getZ^2 - LNHat.getY^2) 
		    LNhat.setX(value)
		  End If
		  
		  If LNhat.getY > 1 Then
		    value = Sqrt(1 - LNHat.getX^2 - LNHat.getZ^2) 
		    LNhat.setZ(Value)
		  End If
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(v0 As Double, dτ0 As Double, myδ As Double, Ln0 As Double, myι0 As Double, myα0 As Double, χ1x As Double, χ1y As Double, χ1z As Double, χ2x As Double, χ2y As Double, χ2z As Double)
		  // This SpinEvolver class is instantiated by the Evolver class, and it handles the evolution of the spin variables 
		  // χ1hat, χ2hat, ι, and α (as well as the derivatives of ι and α). It takes some initial condition parameters, including 
		  // the components of χ1 and χ2, in order to evolve these variables. The majority of the work occurs in the SpinDoStep 
		  // method, which is called by DoStep of Evolver and actually performs a step in each variable.
		  
		  //Define necessary constants for the GetΩ1Coef and GetΩ2Coef equations
		  π = 3.141592653589793238
		  c1 = 3/4
		  c2 = 9/16
		  c3 = 5/4
		  c4 = 5/8
		  c5 = 27/32
		  c6 = 3/16
		  c7 = 105/32
		  c8 = 39/8
		  c9 = 5/32
		  
		  //Define some necessary initial parameters
		  δ = myδ                    //δ comes from the main program, and it's defined to be (m1 - m2)/M, where m1 is the larger mass
		  η = (1-δ^2)/4           //Defined to be m1m2/M^2
		  ι0 = myι0                 //This is calculated in the Evolver constructor using the components of χ1 and χ2 (defined to be the angle between LNhat and J0hat)
		  α0 = myα0               //This is calculated in the Evolver constructor using the components of χ1 and χ2 (defined to be the angle between LNhatX and the x-axis)    
		  
		  αP = α0
		  
		  LNhatN = New Vector(Sin(ι0)*Cos(α0), Sin(ι0)*Sin(α0), Cos(ι0)) //Set up the initial component values of LNhat
		  CheckLNHat(LNhatN)
		  oldψangle0 = ATan2(-LNhatN.y, LNHatN.z)
		  
		  //Create χ1 and χ2 as vectors using the passed components
		  χ1 = New Vector (χ1x, χ1y, χ1z)
		  χ2 = New Vector (χ2x, χ2y, χ2z)
		  
		  //Compute the magnitudes of χ1 and χ2 from the vectors
		  χ1mag = χ1.getMagnitude
		  χ2mag = χ2.getMagnitude
		  
		  //Compute the unit vectors of χ1 and χ2
		  If χ1mag = 0 Then
		    χ1hatN = New Vector(0,0,0)       //If the magnitude of the χ1 vector is zero, the unit vector χ1hat will also be zero
		  Else 
		    χ1hatN = χ1/χ1mag                      //Otherwise, calculate normally
		  End If
		  If χ2mag = 0 Then
		    χ2hatN = New Vector(0,0,0)      //If the magnitude of the χw vector is zero, the unit vector χ2hat will also be zero
		  Else 
		    χ2hatN = χ2/χ2mag                      //Otherwise, calculate normally
		  End If
		  
		  //Compute the "future" values of LNhat, ι, α, χ1hat, and χ2hat using the "now" value of Ω1 and Ω2
		  Var Ω1N As Vector = GetΩ1Coef(v0)*LNhatN                //Required for calculating the derivative of χ1hat with respect to time. Note that the asterisk is a dot product
		  Var Ω2N As Vector = GetΩ2Coef(v0)*LNhatN              //Required for calculating the derivative of χ2hat with respect to time. Note that the asterisk is a dot product
		  LNhatF = LNhatN + dτ0*GetLNhatdot(v0, χ1hatN, χ2hatN, Ω1N, Ω2N)         //Compute the first "future" value of LNhat using the non-centered approximation
		  CheckLNHat(LNHatF)
		  ιF = ACos(LNhatF.getZ)                   //Compute iota from the z-component of LNhat
		  if Abs(ιF) < 10^-8 then     //If ι is zero, we cannot find α through inverse trig operations because it will be undefined. Thus, we want to...
		    ιF = 0                   //Manually set iota to zero, in case there is a floating point error
		    Var LNhatFFx As Double = -LNhatN.getX          //Assuming LNhatX changes linearly across the zero point, we assume if the "future" value is zero that the "future future value will be the negative of the "now" value
		    Var LNhatFFy As Double = -LNhatN.getY          //Assuming LNhatY changes linearly across the zero point, we assume if the "future" value is zero that the "future future value will be the negative of the "now" value
		    If LNhatFFx = 0 And LNhatFFy = 0 Then
		      αF = α0
		    Else
		      Var αFF As Double = ATan2(LNhatFFy, LNhatFFx)     //First, find the α value after αF, the "future future" value
		      Var αdotF As Double = (αFF - α0)/(2*dτ0)     //Then, find the derivative of αdot at αF using the centered difference aproximation
		      αF = α0 + dτ0*αdotF     //Finally, find αF using this derivative and αΝ using the centered difference aproximation
		    End If
		  else
		    αF = ATan2(LNhatF.getY, LNhatF.getX)         //If iota is nonzero, we can just calculate alpha normally
		  end if
		  αAccN = α0  // calculated the accumulated alpha
		  αAccF = AccumulatingAtan(α0, αF)  // calculated the accumulated alpha
		  
		  αdot0 = (αAccF - αAccN)/dτ0         //Compute the first value of alphadot using the non-centered approximation
		  χ1hatF = χ1hatN + dτ0*(Ω1N^χ1hatN)   //Compute the first "future" value of χ1hat using the non-centered approximation
		  χ2hatF = χ2hatN + dτ0*(Ω2N^χ2hatN)  //Compute the first "future" value of χ2hat using the non-centered approximation
		  
		  LNhatMag = LNhatN.getMagnitude
		  
		  // Calculate ideal values of time-steps (for initialization)
		  Var LNhatDot As Vector = GetLNhatdot(v0, χ1hatN, χ2hatN, Ω1N, Ω2N)
		  
		  Var ε2 As Double = 1.0E-2   // scaling factor for time step size
		  Var χ1hatdot As Vector = Ω1N^χ1hatN           //Find derivative
		  Var χ2hatdot As Vector = Ω2N^χ2hatN           //Find derivative
		  dτLN = π*ε2/LNhatdot.getMagnitude
		  dτχ1 = π*ε2/χ1hatdot.getMagnitude
		  dτχ2 = π*ε2/χ2hatdot.getMagnitude
		  
		  // calculate the old ψ, which is used in the detector functions
		  oldψangle = ATan2(-LNhatF.y, LNHatF.z)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetLNhatdot(v As Double, χ1hat As Vector, χ2hat As Vector, Ω1 As Vector, Ω2 As Vector) As Vector
		  // This method is called by the constructor and the SpinDoStep method, and it takes v, χ1hat, χ2hat, Ω1hat, and Ω2hat as 
		  // parameters to solve for and return dLNhatdot
		  
		  Var vector1 As Vector = χ1mag*(1 + δ)^2*(Ω1^χ1hat)       //This vector and the one below are meant for organization, to make the equation easier to understand
		  Var vector2 As Vector = χ2mag*(1 - δ)^2*(Ω2^χ2hat)
		  Var LNhatdot As Vector = -v/(4*η)*(vector1 + vector2)                     //This is the actual equation for dLNhatdot
		  
		  Return LNhatdot
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetΩ1Coef(v As Double) As Double
		  // This method is called by the constructor and the SpinDoStep method, and it takes v as a parameter
		  // to return the value of the scalar coefficient needed to solve for Ω1. Ω1 equals this scalar multiplied 
		  // with LNhat.
		  
		  Var Ω1Coef As Double = v^5*(c1 + η/2 - (c1)*δ + v^2 _
		  *(c2 + (c3)*η - (η^2)/24 + δ*(-c2 + (c4)*η)) _
		  + v^4*(c5 + (c6)*η - (c7)*η^2 - (η^3)/48 _
		  + δ*(-c5 + (c8)*η - (c9)*η^2)))
		  
		  Return Ω1Coef
		  
		  'Here are the constant values for reference:
		  'c1 = 3/4
		  'c2 = 9/16
		  'c3 = 5/4
		  'c4 = 5/8
		  'c5 = 27/32
		  'c6 = 3/16
		  'c7 = 105/32
		  'c8 = 39/8
		  'c9 = 5/32
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetΩ2Coef(v As Double) As Double
		  // This method is called by the constructor and the SpinDoStep method, and it takes v as a parameter
		  // to return the value of the scalar coefficient needed to solve for Ω2. Ω2 equals this scalar multiplied 
		  // with LNhat.
		  
		  Var Ω2Coef As Double = v^5*(c1 + η/2 + (c1)*δ + v^2 _
		  *(c2 + (c3)*η - (η^2)/24 - δ*(-c2 + (c4)*η)) _
		  + v^4*(c5 + (c6)*η - (c7)*η^2 - (η^3)/48 _
		  - δ*(-c5 + (c8)*η - (c9)*η^2)))
		  
		  Return Ω2Coef
		  
		  'Here are the constant values for reference:
		  'c1 = 3/4
		  'c2 = 9/16
		  'c3 = 5/4
		  'c4 = 5/8
		  'c5 = 27/32
		  'c6 = 3/16
		  'c7 = 105/32
		  'c8 = 39/8
		  'c9 = 5/32
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SpinDoStep(dτP As Double, dτ As Double, vN As Double)
		  // This method serves a similar purpose to the DoStep method of Evolver, but it evolves just the spin variables.
		  // This stepper also takes a value of dτ, as well as a value of v and of dτP, and it solves for the "future" value of each variable.
		  
		  // Calculate new past values using interpolation
		  χ1hatP = (1 - dτ/dτP)*χ1hatF + (dτ/dτP)*χ1hatN   
		  χ2hatP = (1 - dτ/dτP)*χ2hatF + (dτ/dτP)*χ2hatN   
		  LNhatP = (1 - dτ/dτP)*LNhatF + (dτ/dτP)*LNhatN 
		  CheckLNHat(LNhatP)
		  Var αAccP As Double = (1 - dτ/dτP)*αAccF + (dτ/dτP)*αAccN
		  
		  // Reassign "future" values to present values
		  χ1hatN = χ1hatF
		  χ2hatN = χ2hatF
		  LNhatN = LNhatF  
		  Var αN As Double = αF     //This variable is local because we do not need it anywhere else (just for the condition where iota is zero)
		  αAccN = αAccF
		  
		  // Do step
		  Var Ω1N As Vector = GetΩ1Coef(vN)*LNhatN      //Solve for the Ω1 vector, which is necessary to find χ1hat
		  Var χ1hatdot As Vector = Ω1N^χ1hatN           //Find derivative
		  χ1hatF = χ1hatP + 2*dτ*χ1hatdot                       //Actually solve for the χ1hat vector using the centered difference "leapfrog" approach
		  
		  Var Ω2N As Vector = GetΩ2Coef(vN)*LNhatN     //Solve for the Ω2 vector, which is necessary to find χ2hat
		  Var χ2hatdot As Vector = Ω2N^χ2hatN           //Find derivative
		  χ2hatF = χ2hatP + 2*dτ*χ2hatdot                     //Actually solve for the χ2hat vector using the centered difference "leapfrog" approach
		  
		  Var LNhatdot As Vector = GetLNhatdot(vN, χ1hatN, χ2hatN, Ω1N, Ω2N)       //Solve for the time derivative of LNhat
		  LNhatF = LNhatP + 2*dτ*LNhatdot      //Solve for the future LNhat vector using the centered difference approach
		  CheckLNHat(LNhatF)
		  
		  ιF = ACos(LNhatF.getZ)                                //Calculate iota from the Z component of LNhat
		  if Abs(ιF) < 10^-8 then     //If ι is zero, we cannot find α through inverse trig operations because it will be undefined. Thus, we want to...
		    ιF = 0                   //Manually set iota to zero, in case there is a floating point error
		    Var LNhatFFx As Double = -LNhatN.getX         //Assuming LNhatX changes linearly across the zero point, we assume if the "future" value is zero that the "future future value will be the negative of the "now" value
		    Var LNhatFFy As Double = -LNhatN.getY         //Assuming LNhatY changes linearly across the zero point, we assume if the "future" value is zero that the "future future value will be the negative of the "now" value
		    If LNhatFFx = 0 And LNhatFFy = 0 Then
		      αF = αN
		    Else
		      Var αFF As Double = ATan2(LNhatFFy, LNhatFFx)     //First, find the α value after αF, the "future future" value
		      Var αdotF As Double = (αFF - αN)/(2*dτ)     //Then, find the derivative of αdot at αF using the centered difference aproximation
		      αF = αN + dτ*αdotF     //Finally, find αF using this derivative and αΝ using the non-centered difference approximation
		    End If
		  else
		    αF = ATan2(LNhatF.getY, LNhatF.getX)   //If iota is nonzero, we can just calculate alpha from the Y and X components of LNhat
		  end if
		  
		  Var αP As Double = ATan2(LNhatP.getY, LNhatP.getX)       //Set up a "past" value of alpha for calculating alphadot
		  
		  αAccF = AccumulatingAtan(αP,αF)
		  
		  αdotN = (αAccF - αAccP)/(2*dτ)      // Finds αdot using a centered difference approximation. NOTE: this is the "current" value, not the "future" value
		  
		  // Calculate ideal values of time-steps
		  Var ε As Double = 1.0E-2   // scaling factor for time step size
		  
		  dτLN = π*ε/LNhatdot.getMagnitude
		  dτχ1 = π*ε/χ1hatdot.getMagnitude
		  dτχ2 = π*ε/χ2hatdot.getMagnitude
		  dτα = π*ε/αdotN
		  
		  LNhatMag = LNhatN.getMagnitude
		  oldψangle = ATan2(-LNhatF.y, LNHatF.z) // calculate the old ψ, which is used in the detector functions
		  
		  
		  
		  
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		#tag Note
			//Value: 3/4
		#tag EndNote
		c1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 9/16
		#tag EndNote
		c2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 5/4
		#tag EndNote
		c3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 5/8
		#tag EndNote
		c4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 27/32
		#tag EndNote
		c5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 3/16
		#tag EndNote
		c6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 105/32
		#tag EndNote
		c7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 39/8
		#tag EndNote
		c8 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		#tag Note
			//Value: 5/32
		#tag EndNote
		c9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτLN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτα As double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτχ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτχ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FullTurns As Integer = 0
	#tag EndProperty

	#tag Property, Flags = &h0
		LNhatF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		LNhatMag As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LNhatN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		LNhatP As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		oldψangle As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		oldψangle0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		QuarterTurns As Integer = 0
	#tag EndProperty

	#tag Property, Flags = &h0
		SignChangedLastTime As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		ι0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ιF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		α0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αAccF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αAccN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αdot0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αdotN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		δ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		η As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		π As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1 As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1hatF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1hatN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1hatP As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1mag As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2 As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2hatF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2hatN As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2hatP As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2mag As Double
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
			Name="αF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="δ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="η"
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
			Name="χ1mag"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2mag"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c7"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c8"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="c9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αdotN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αdot0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dτLN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dτχ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dτχ2"
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
			Name="LNhatMag"
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
			Name="QuarterTurns"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αAccN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SignChangedLastTime"
			Visible=false
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="oldψangle"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FullTurns"
			Visible=false
			Group="Behavior"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="oldψangle0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αAccF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dτα"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
