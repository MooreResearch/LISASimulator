#tag Class
Protected Class SpinEvolverClass
	#tag Method, Flags = &h21
		Private Sub AdjustThePast()
		  // If our future step is half the previous step then
		  // readjust past variable values used in leapfrog
		  // calculations to be the same interval to the past
		  // of now that the future is ahead of now.
		  
		  χ1xP = 0.5*(χ1xN + χ1xP)
		  χ1yP = 0.5*(χ1yN + χ1yP)
		  χ1zP = 0.5*(χ1zN + χ1zP)
		  
		  ℓxP = 0.5*(ℓxN + ℓxP)
		  ℓyP = 0.5*(ℓyN + ℓyP)
		  ℓzP = 0.5*(ℓzN + ℓzP)
		  
		  ΨprP = 0.5*(ΨprN + ΨprP)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(CInfo As CaseInfoClass)
		  P = CInfo
		  If CInfo.χ1 = 0.0 And CInfo.χ2 = 0.0 Then
		    // If we have no spins, then we do not need to do any evolution.
		    // But we do need to set up the VCalculator and get the V0 value.
		    VCalc = New VCalculatorClass(P.τc, P.δ, 0.0, 0.0)
		    V0 = VCalc.VAtTime(0.0)
		    
		  Else // We have at least one nonzero spin
		    // Start with basic setup
		    Var δ As Double = P.δ
		    η = 0.25*(1 - δ*δ)
		    Var η2 As Double = η*η
		    Var η3 As Double = η2*η
		    
		    // Calculate spin evolution coefficients
		    C10 = 0.75*(1.0 - δ) + 0.5*η
		    C20 = 0.75*(1.0 + δ) + 0.5*η
		    C12 = 9/16 + 5/4*η - η2/24 + δ*(-9/16 + 5/8*η)
		    C22 = 9/16 + 5/4*η - η2/24 - δ*(-9/16 + 5/8*η)
		    C14 = 27/32 + 3/16*η - 105/32*η2 - η3/48 + δ*(-27/32 + 39/8*η -5/32*η2)
		    C24 = 27/32 + 3/16*η - 105/32*η2 - η3/48 - δ*(-27/32 + 39/8*η -5/32*η2)
		    
		    // Calculate orbital AM magnitude coefficients
		    Opδ2I4 = 0.25*(1.0 + δ)*(1.0 + δ)
		    Omδ2I4 = 0.25*(1.0 - δ)*(1.0 - δ)
		    χ1ℓ = P.χ1*Cos(P.θ1)
		    χ2ℓ = P.χ2*Cos(P.θ2)
		    Sℓ = Opδ2I4*χ1ℓ + Omδ2I4*χ2ℓ
		    Σℓ = 0.5*(1.0 - δ)*χ2ℓ - 0.5*(1.0 + δ)*χ1ℓ
		    L2 = 3/2 + η/6
		    L3 = -35/6*Sℓ-5/2*δ*Σℓ
		    L4 = 27/8 - 19/8*η + η2/24
		    
		    // Initialize the velocity calculator
		    VCalc = New VCalculatorClass(P.τc, δ, χ1ℓ, χ2ℓ)
		    
		    // Calculate the initial orbital angular momentum magnitude
		    V0 = VCalc.VAtTime(0.0)
		    Var v2 As Double = V0*V0
		    Var v3 As Double = v2*V0
		    Var v4 As Double = v3*V0
		    var v5 As Double = v4*V0
		    Var L0 As Double = η/V0*(1.0 + L2*v2 + L3*v3 + L4*v4)
		    
		    // Calculate spin components in the precessing frame
		    Var χ1xL As Double = P.χ1*Sin(P.θ1)*Cos(P.φ1)
		    Var χ1yL As Double = P.χ1*Sin(P.θ1)*Sin(P.φ1)
		    Var χ1zL As Double = χ1ℓ
		    Var χ2xL As Double = P.χ2*Sin(P.θ2)*Cos(P.φ2)
		    Var χ2yL As Double = P.χ2*Sin(P.θ2)*Sin(P.φ2)
		    Var χ2zL As Double = χ2ℓ
		    
		    // Calculate total angular momentum components in the precessing frame
		    Var j0x As Double = Opδ2I4*χ1xL + Omδ2I4*χ2xL
		    Var j0y As Double = Opδ2I4*χ1yL + Omδ2I4*χ2yL
		    Var j0z As Double = Opδ2I4*χ1zL + Omδ2I4*χ2zL + L0
		    
		    // Calculate rotation matrix
		    Var θ0 As Double = ATan2(j0z, Sqrt(j0x*j0x+j0y*j0y))
		    Var φ0 As Double = ATan2(j0y, j0x)
		    Var rxx As Double = Cos(θ0)*Cos(φ0)
		    Var rxy As Double = Cos(θ0)*Sin(φ0)
		    Var rxz As Double = -Sin(θ0)
		    Var ryx As Double = -Sin(φ0)
		    Var ryy As Double = Cos(φ0)
		    Var ryz As Double = 0.0
		    Var rzx As Double = Sin(θ0)*Cos(φ0)
		    Var rzy As Double = Sin(θ0)*Sin(φ0)
		    Var rzz As Double = Cos(θ0)
		    
		    // Calculate initial values for the spin vector components
		    χ1xP = rxx*χ1xL + rxy*χ1yL  + rxz*χ1zL
		    χ1yP = ryx*χ1xL + ryy*χ1yL  + ryz*χ1zL
		    χ1zP = rzx*χ1xL + rzy*χ1yL  + rzz*χ1zL
		    χ2xP = rxx*χ2xL + rxy*χ2yL  + rxz*χ2zL
		    χ2yP = ryx*χ2xL + ryy*χ2yL  + ryz*χ2zL
		    χ2zP = rzx*χ2xL + rzy*χ2yL  + rzz*χ2zL
		    
		    // Calculate intial values for the orbital angular momentum components and angles
		    ℓxP = rxz*L0
		    ℓyP = ryz*L0
		    ℓzP = rzz*L0
		    αP = Atan2(ℓyP,ℓxP)
		    ιP = Atan2(ℓzP,Sqrt(ℓxP*ℓxP + ℓyP*ℓyP))
		    
		    // Calculate components of the initial spin rate of change
		    Var Ω1 As Double = v5*(C10 + C12*v2 + C14*v4)/L0
		    Var χ1xDotP As Double = Ω1*(ℓyP*χ1zP - ℓzP*χ1yP)
		    Var χ1yDotP As Double = Ω1*(ℓzP*χ1xP - ℓxP*χ1zP)
		    Var χ1zDotP As Double = Ω1*(ℓxP*χ1yP - ℓyP*χ1xP)
		    Var Ω2 As Double = v5*(C20 + C22*v2 + C24*v4)/L0
		    Var χ2xDotP As Double = Ω2*(ℓyP*χ2zP - ℓzP*χ2yP)
		    Var χ2yDotP As Double = Ω2*(ℓzP*χ2xP - ℓxP*χ2zP)
		    Var χ2zDotP As Double = Ω2*(ℓxP*χ2yP - ℓyP*χ2xP)
		    
		    // Calculate the first time step to be half the step that would
		    // take 628 steps for the fastest spin to precess once
		    Var s1dot As Double = Sqrt(χ1xDotP*χ1xDotP + χ1yDotP*χ1yDotP + χ1zDotP*χ1zDotP)
		    Var s2dot As Double = Sqrt(χ2xDotP*χ2xDotP + χ2yDotP*χ2yDotP + χ2zDotP*χ2zDotP)
		    If s1dot = 0.0 Then
		      ΔτhP = P.χ2/s2dot
		    ElseIf s2dot = 0.0 Then
		      ΔτhP = P.χ1/s1dot
		    Else
		      ΔτhP = 0.5*Min(P.χ1/s1dot, P.χ2/s2dot)
		    End If
		    ΔτhF = ΔτhP
		    
		    // Evolve the spins using an Euler step
		    χ1xN = χ1xP + ΔτhP*χ1xDotP
		    χ1yN = χ1yP + ΔτhP*χ1yDotP
		    χ1zN = χ1zP + ΔτhP*χ1zDotP
		    χ2xN = χ2xP + ΔτhP*χ2xDotP
		    χ2yN = χ2yP + ΔτhP*χ2yDotP
		    χ2zN = χ2zP + ΔτhP*χ2zDotP
		    
		    // Evolve the orbital angular momentum using an Euler step
		    Var ℓDotP As Double = η*(-1.0/v2 + L2 + 2*L2*v0 + 3*L4*v2)*VCalc.VDotForLastV/L0
		    Var ℓxDotP As Double = -Opδ2I4*χ1xDotP - Omδ2I4*χ2xDotP + ℓDotP*ℓxP
		    Var ℓyDotP As Double = -Opδ2I4*χ1yDotP - Omδ2I4*χ2yDotP + ℓDotP*ℓyP
		    Var ℓzDotP As Double = -Opδ2I4*χ1zDotP - Omδ2I4*χ2zDotP + ℓDotP*ℓzP
		    ℓxN = ℓxP + ΔτhP*ℓxDotP
		    ℓyN = ℓyP + ΔτhP*ℓyDotP
		    ℓzN = ℓzP + ΔτhP*ℓzDotP
		    
		    // To get a more precise estimate of the future values, iterate the calculation
		    // Calculate the orbital angular momentum magnitude at the first time step
		    Var v1 As Double = VCalc.VAtTime(ΔτhP)
		    v2 = v1*v1
		    v3 = v2*v1
		    v4 = v3*v1
		    v5 = v4*v1
		    L0  = η/v1*(1.0 + L2*v2 + L3*v3 + L4*v4)
		    
		    // Calculate components of the future spin rate of change
		    Ω1 = v5*(C10 + C12*v2 + C14*v4)/L0
		    Var χ1xDotN As Double = Ω1*(ℓyN*χ1zN - ℓzN*χ1yN)
		    Var χ1yDotN As Double = Ω1*(ℓzN*χ1xN - ℓxN*χ1zN)
		    Var χ1zDotN As Double = Ω1*(ℓxN*χ1yN - ℓyN*χ1xN)
		    Ω2 = v5*(C20 + C22*v2 + C24*v4)/L0
		    Var χ2xDotN As Double = Ω2*(ℓyN*χ2zN - ℓzN*χ2yN)
		    Var χ2yDotN As Double = Ω2*(ℓzN*χ2xN - ℓxN*χ2zN)
		    Var χ2zDotN As Double = Ω2*(ℓxN*χ2yN - ℓyN*χ2xN)
		    
		    // Evolve the spins using a more correct step
		    χ1xN = χ1xP + 0.5*ΔτhP*(χ1xDotP + χ1xDotN)
		    χ1yN = χ1yP + 0.5*ΔτhP*(χ1yDotP + χ1yDotN)
		    χ1zN = χ1zP + 0.5*ΔτhP*(χ1zDotP + χ1zDotN)
		    χ2xN = χ2xP + 0.5*ΔτhP*(χ2xDotP + χ2xDotN)
		    χ2yN = χ2yP + 0.5*ΔτhP*(χ2yDotP + χ2yDotN)
		    χ2zN = χ2zP + 0.5*ΔτhP*(χ2zDotP + χ2zDotN)
		    
		    // Evolve the orbital angular momentum using a more correct step
		    Var ℓDotN As Double = η*(-1.0/v2 + L2 + 2*L2*v0 + 3*L4*v2)*VCalc.VDotForLastV/L0
		    Var ℓxDotN As Double = -Opδ2I4*χ1xDotN - Omδ2I4*χ2xDotN + ℓDotP*ℓxN
		    Var ℓyDotN As Double = -Opδ2I4*χ1yDotN - Omδ2I4*χ2yDotN + ℓDotP*ℓyN
		    Var ℓzDotN As Double = -Opδ2I4*χ1zDotN - Omδ2I4*χ2zDotN + ℓDotP*ℓzN
		    ℓxN = ℓxP + 0.5*ΔτhP*(ℓxDotN + ℓxDotP)
		    ℓyN = ℓyP + 0.5*ΔτhP*(ℓyDotN + ℓyDotP)
		    ℓzN = ℓzP + 0.5*ΔτhP*(ℓzDotN + ℓzDotP)
		    αN = Atan2(ℓyN,ℓxN)
		    ιN = Atan2(ℓzN,Sqrt(ℓxN*ℓxN + ℓyN*ℓyN))
		    
		    // Initialize the precession phase
		    Var αDotP As Double = (ℓyP*ℓxDotP - ℓxP*ℓyDotP)/(ℓxDotP*ℓxDotP + ℓyDotP*ℓyDotP)
		    Var αDotN As Double = (ℓyN*ℓxDotN - ℓxN*ℓyDotN)/(ℓxDotN*ℓxDotN + ℓyDotN*ℓyDotN)
		    ΨprP = 0.0
		    ΨprN = -0.5*ΔτhP*(αDotP*Cos(ιP) + αDotN*Cos(ιN))
		    
		    // Check to see whether we have crossed the 2nd/3rd quadrant line
		    If ℓyN < 0.0 And ℓyP > 0.0 Then
		      If (ℓxN*ℓyP - ℓxP*ℓyN)/(ℓyP-ℓyN) < 0.0 Then NαCycles = 1
		    ElseIf ℓyN > 0.0 And ℓyP < 0.0 Then
		      If (ℓxN*ℓyP - ℓxP*ℓyN)/(ℓyP-ℓyN) < 0.0 Then NαCycles = -1
		    Else
		      NαCycles = 0
		    End If
		    αN = αN + NαCycles*2*P.π
		    
		    // Finally, update the times
		    τP = 0
		    τN = ΔτhP
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function DoStepSucceeded() As Boolean
		  // Check whether the step has been adjusted
		  If ΔτhP > ΔτhF Then AdjustThePast  // if so, adjust the past values
		  
		  // Calculate the current orbital angular momentum magnitude
		  Var v1 As Double = VCalc.VAtTime(τN)
		  If v1 > 0.5 Then Return False // if our speed is getting too high, bail out
		  Var v2 As Double = v1*v1
		  Var v3 As Double = v2*v1
		  Var v4 As Double = v3*v1
		  Var v5 As Double = v4*v1
		  Var LN As Double = η/v1*(1.0 + L2*v2 + L3*v3 + L4*v4)
		  
		  // Calculate components of the initial spin rate of change
		  Var Ω1 As Double = v5*(C10 + C12*v2 + C14*v4)/LN
		  Var χ1xDotN As Double = Ω1*(ℓyN*χ1zN - ℓzN*χ1yN)
		  Var χ1yDotN As Double = Ω1*(ℓzN*χ1xN - ℓxN*χ1zN)
		  Var χ1zDotN As Double = Ω1*(ℓxN*χ1yN - ℓyP*χ1xN)
		  Var Ω2 As Double = v5*(C20 + C22*v2 + C24*v4)/LN
		  Var χ2xDotN As Double = Ω2*(ℓyN*χ2zN - ℓzN*χ2yN)
		  Var χ2yDotN As Double = Ω2*(ℓzN*χ2xN - ℓxN*χ2zN)
		  Var χ2zDotN As Double = Ω2*(ℓxN*χ2yN - ℓyN*χ2xN)
		  
		  // Evolve the spins using an leapfrog step
		  Var TwoΔτ As Double = 2.0*ΔτhF
		  Var χ1xF As Double = χ1xP + TwoΔτ*χ1xDotN
		  Var χ1yF As Double = χ1yP + TwoΔτ*χ1yDotN
		  Var χ1zF As Double = χ1zP + TwoΔτ*χ1zDotN
		  Var χ2xF As Double = χ2xP + TwoΔτ*χ2xDotN
		  Var χ2yF As Double = χ2yP + TwoΔτ*χ2yDotN
		  Var χ2zF As Double= χ2zP + TwoΔτ*χ2zDotN
		  
		  // Evolve the orbital angular momentum using a leapfrog step
		  Var ℓDotN As Double = η*(-1.0/v2 + L2 + 2*L2*v1 + 3*L4*v2)*VCalc.VDotForLastV/LN
		  Var ℓxDotN As Double = -Opδ2I4*χ1xDotN - Omδ2I4*χ2xDotN + ℓDotN*ℓxN
		  Var ℓyDotN As Double = -Opδ2I4*χ1yDotN - Omδ2I4*χ2yDotN + ℓDotN*ℓyN
		  Var ℓzDotN As Double = -Opδ2I4*χ1zDotN - Omδ2I4*χ2zDotN + ℓDotN*ℓzN
		  Var ℓxF As Double = ℓxP + TwoΔτ*ℓxDotN
		  Var ℓyF As Double = ℓyP + TwoΔτ*ℓyDotN
		  Var ℓzF As Double = ℓzP + TwoΔτ*ℓzDotN
		  
		  // Evolve the precession phase
		  Var αDotN As Double = (ℓyN*ℓxDotN - ℓxN*ℓyDotN)/(ℓxDotN*ℓxDotN + ℓyDotN*ℓyDotN)
		  Var ΨprF As Double = ΨprP - TwoΔτ*αDotN*Cos(ιN)
		  
		  // Check to see whether we have just crossed the 2nd/3rd quadrant line
		  If ℓyF < 0.0 And ℓyN > 0.0 Then
		    If (ℓxF*ℓyN - ℓxN*ℓyF)/(ℓyN-ℓyF) < 0.0 Then NαCycles = 1
		  ElseIf ℓyF > 0.0 And ℓyN < 0.0 Then
		    If (ℓxF*ℓyN - ℓxN*ℓyF)/(ℓyN-ℓyF) < 0.0 Then NαCycles = -1
		  End If
		  
		  // From here on, the future step just calculated becomes the present step
		  // and the present step becomes the past step
		  τP = τN
		  τN = τN + ΔτhF
		  
		  χ1xP = χ1xN
		  χ1yP = χ1yN
		  χ1zP = χ1zN
		  χ1xN = χ1xF
		  χ1yN = χ1yF
		  χ1zN = χ1zF
		  
		  χ2xP = χ2xN
		  χ2yP = χ2yN
		  χ2zP = χ2zN
		  χ2xN = χ2xF
		  χ2yN = χ2yF
		  χ2zN = χ2zF
		  
		  ℓxP = ℓxN
		  ℓyP = ℓyN
		  ℓzP = ℓzN
		  ℓxN = ℓxF
		  ℓyN = ℓyF
		  ℓzN = ℓzF
		  
		  ΨprP = ΨprN
		  ΨprN = ΨprF
		  
		  αP = αN
		  αN = Atan2(ℓyN,ℓxN) + NαCycles*2.0*P.π
		  
		  ιP = ιN
		  ιN = Atan2(ℓzN,Sqrt(ℓxN*ℓxN + ℓyN*ℓyN))
		  
		  // Calculate the ideal next time step
		  Var s1dot As Double = Sqrt(χ1xDotN*χ1xDotN + χ1yDotN*χ1yDotN + χ1zDotN*χ1zDotN)
		  Var s2dot As Double = Sqrt(χ2xDotN*χ2xDotN + χ2yDotN*χ2yDotN + χ2zDotN*χ2zDotN)
		  Var ΔτhIdeal As Double = Min(P.χ1/s1dot, P.χ2/s2dot)
		  If ΔτhIdeal < ΔτhF Then // if we need a smaller step
		    If ΔτhP > ΔτhF Then
		      Return False // if we just did a smaller step, we are breaking down, so quit
		    Else
		      ΔτhP = ΔτhF  // store the previous step
		      ΔτhF = ΔτhF/2  // reduce the next step size by two
		    End If
		  Else // if we don't need a smaller step, repeat the current step
		    ΔτhP = ΔτhF
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetSpinDataAtTime(τ As Double) As SpinDataClass
		  If P.χ1 = 0.0 And P.χ2 = 0.0 Then // if we have no spins
		    Var data As New SpinDataClass
		    data.ι = 1.0
		    data.α = P.π
		    data.χax = 0.0
		    data.χay = 0.0
		    data.χaz = 0.0
		    data.χsx = 0.0
		    data.χsy = 0.0
		    data.χsz = 0.0
		    Var vForτ As Double = VCalc.VAtTime(τ)
		    data.V = vForτ
		    data.Ψ = P.λ0 + VCalc.ΨOrbForLastV - 6*vForτ*vForτ*vForτ*Log(vForτ/V0)
		    Return data
		  Else // If we have at least one nonzero spin, then we need to evolve
		    // Cycle through steps until we get beyond the requested time
		    // If DoStepSucceeded = False then we have reached coalescence
		    While τ > τN
		      If Not DoStepSucceeded Then Return Nil
		    Wend
		    // Interpolate data to pass on to the rest of the program
		    Var fN As Double = (τ - τP)/ΔτhP
		    Var fP As Double = 1.0 - fN
		    Var data As New SpinDataClass
		    data.ι = fN*ιN + fP*ιP
		    data.α = fN*αN + fP*αP
		    data.χax = 0.5*(fN*(χ1xN - χ2xN) + fP*(χ1xP - χ2xP))
		    data.χay = 0.5*(fN*(χ1yN - χ2yN) + fP*(χ1yP - χ2yP))
		    data.χaz = 0.5*(fN*(χ1zN - χ2zN) + fP*(χ1zP - χ2zP))
		    data.χsx = 0.5*(fN*(χ1xN + χ2xN) + fP*(χ1xP + χ2xP))
		    data.χsy = 0.5*(fN*(χ1yN + χ2yN) + fP*(χ1yP + χ2yP))
		    data.χsz = 0.5*(fN*(χ1zN + χ2zN) + fP*(χ1zP + χ2zP))
		    
		    // Calculate phase at the current time
		    Var vForτ As Double = VCalc.VAtTime(τ)
		    data.V = vForτ
		    data.Ψ = P.λ0 + VCalc.ΨOrbForLastV + fN*ΨprN + fP*ΨprP - 6*vForτ*vForτ*vForτ*Log(vForτ/V0)
		    Return data
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetVAtTime(τ As Double) As Double
		  Return VCalc.VAtTime(τ)
		End Function
	#tag EndMethod


	#tag Property, Flags = &h21
		Private C10 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private C12 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private C14 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private C20 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private C22 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private C24 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private L2 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private L3 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private L4 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private NαCycles As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Omδ2I4 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Opδ2I4 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private P As CaseInfoClass
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Sℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private V0 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private VCalc As VCalculatorClass
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ιN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ιP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private αN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private αP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ΔτhF As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ΔτhP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		η As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Σℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private τN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private τP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1xN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1xP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1yN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1yP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1zN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1zP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2xN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2xP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2yN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2yP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2zN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2zP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ΨprN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ΨprP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ℓxN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ℓxP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ℓyN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ℓyP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ℓzN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ℓzP As Double
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
			Name="η"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass