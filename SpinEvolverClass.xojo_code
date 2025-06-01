#tag Class
Protected Class SpinEvolverClass
	#tag Method, Flags = &h21
		Private Sub AdjustThePast()
		  // If our future step is half the previous step then
		  // readjust past variable values used in leapfrog
		  // calculations to be the same interval to the past
		  // of now that the future is ahead of now.
		  
		  // This method assumes that the future step is
		  // half the past step size.
		  
		  χ1x(0) = 0.5*(χ1x(1) + χ1x(0))
		  χ1y(0) = 0.5*(χ1y(1) + χ1y(0))
		  χ1z(0) = 0.5*(χ1z(1) + χ1z(0))
		  
		  χ2x(0) = 0.5*(χ2x(1) + χ2x(0))
		  χ2y(0) = 0.5*(χ2y(1) + χ2y(0))
		  χ2z(0) = 0.5*(χ2z(1) + χ2z(0))
		  
		  ℓx(0) = 0.5*(ℓx(1) + ℓx(0))
		  ℓy(0) = 0.5*(ℓy(1) + ℓy(0))
		  ℓz(0) = 0.5*(ℓz(1) + ℓz(1))
		  
		  Ψpr(0) = 0.5*(Ψpr(1) + Ψpr(0))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub CalculateInitialSpins()
		  // Calculate spin components in the precessing frame
		  Var χ1xL As Double = χ1*Sin(θ1)*Cos(φ1)
		  Var χ1yL As Double = χ1*Sin(θ1)*Sin(φ1)
		  Var χ1zL As Double = χ1*Cos(θ1)
		  Var χ2xL As Double = χ2*Sin(θ2)*Cos(φ2)
		  Var χ2yL As Double = χ2*Sin(θ2)*Sin(φ2)
		  Var χ2zL As Double = χ2*Cos(θ2)
		  
		  // Calculate total angular momentum components in the precessing frame
		  Var j0x As Double = μ12*χ1xL + μ22*χ2xL
		  Var j0y As Double = μ12*χ1yL + μ22*χ2yL
		  Var j0z As Double = μ12*χ1zL + μ22*χ2zL + ℓ
		  Var j0f As Double = Sqrt(j0x*j0x + j0y*j0y)
		  Var j0 As Double = Sqrt(j0z*j0z + j0f*j0f)
		  
		  // Calculate rotation matrix
		  Var rxx As Double = j0z*j0x/(j0*j0f)
		  Var rxy As Double = j0z*j0y/(j0*j0f)
		  Var rxz As Double = -j0f/j0
		  Var ryx As Double = -j0y/j0f
		  Var ryy As Double = j0x/j0f
		  Var ryz As Double = 0.0
		  Var rzx As Double = j0x/j0
		  Var rzy As Double = j0y/j0
		  Var rzz As Double = j0z/j0
		  
		  // Calculate initial values for the spin vector components
		  χ1x(0) = rxx*χ1xL + rxy*χ1yL  + rxz*χ1zL
		  χ1y(0) = ryx*χ1xL + ryy*χ1yL  + ryz*χ1zL
		  χ1z(0) = rzx*χ1xL + rzy*χ1yL  + rzz*χ1zL
		  χ2x(0) = rxx*χ2xL + rxy*χ2yL  + rxz*χ2zL
		  χ2y(0) = ryx*χ2xL + ryy*χ2yL  + ryz*χ2zL
		  χ2z(0) = rzx*χ2xL + rzy*χ2yL  + rzz*χ2zL
		  
		  // Calculate initial values for the orbital angular momentum components and angles
		  ℓx(0) = rxz*ℓ
		  ℓy(0) = ryz*ℓ
		  ℓz(0) = rzz*ℓ
		  α(0) = Atan2(ℓy(0),ℓx(0))
		  Var ℓ0f As Double = Sqrt(ℓx(0)*ℓx(0) + ℓy(0)*ℓy(0))
		  ι(0) = Atan2(ℓ0f, ℓz(0))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub CalculateStuffAtTime(τ As Double)
		  // This method calculates the following at the time in question:
		  //   V and its powers, ℓ, Ψorb, and Ψtail
		  
		  // Calculate v and its powers at the given time
		  VN = VCalc.VAtTime(τ)
		  V2 = VN*VN
		  V3 = V2*VN
		  V4 = V3*VN
		  V5 = V4*VN
		  V6 = V5*VN
		  V7 = V6*VN
		  
		  // Calculate the orbital angular momentum magnitude at the given time
		  ℓ = L0/V0*(1.0 + L2*V2 + L3*V3 + L4*V4)
		  
		  // Get the orbital phase and its derivatives at the given time
		  Ψorb = VCalc.ΨorbForLastV
		  
		  // Get the phase tail and its derivatives at the time in question
		  Ψtail = VCalc.ΨtailForLastV
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(CaseInfo As CaseInfoClass)
		  InitializeConstants(CaseInfo)
		  VCalc = New VCalculatorClass(CaseInfo.τc, δ, χ1*Cos(θ1), χ2*Cos(θ2), CaseInfo.λ0)
		  CalculateStuffAtTime(0.0)
		  V0 = VN
		  CalculateInitialSpins
		  If (χ1 = 0 And χ2 = 0) Or (θ1 = 0 And θ2 = 0) Then
		    NoPrecession = True
		  Else
		    NoPrecession = False
		    TakeTheFirstStep
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetSpinDataAtTime(τ As Double) As SpinResultsClass
		  // Create a new class to contain the spin results
		  Var data As New SpinResultsClass
		  
		  // Now copy over the results of the spin evolution. The situation is different
		  // if we have no precession or we do have precession
		  If NoPrecession Then // if we have no spins or spins are aligned with the orbital AM
		    data.ι = 0.0
		    data.α = π
		    data.χax = 0.0
		    data.χay = 0.0
		    data.χaz = 0.5*(χ1-χ2)
		    data.χsx = 0.0
		    data.χsy = 0.0
		    data.χsz = 0.5*(χ1+χ2)
		    CalculateStuffAtTime(τ)
		    data.V = VN
		    data.Ψ = Ψorb + Ψtail
		    Return data
		  Else // If we have at least one nonaligned spin, then we need to evolve
		    // Cycle through steps until we get beyond the requested time
		    // If DoStepSucceeded = False then we have reached coalescence
		    While τ > τN
		      If Not StepWasSuccessful Then Return Nil 
		    Wend
		    // Interpolate data to pass on to the rest of the program
		    Var fN As Double = (τ - τP)/ΔτhP
		    Var fP As Double = 1.0 - fN
		    data.ι = fN*ι(1) + fP*ι(0)
		    data.α = fN*α(1) + fP*α(0)
		    data.χax = 0.5*(fN*(χ1x(1) - χ2x(1)) + fP*(χ1x(0) - χ2x(0)))
		    data.χay = 0.5*(fN*(χ1y(1) - χ2y(1)) + fP*(χ1y(0) - χ2y(0)))
		    data.χaz = 0.5*(fN*(χ1z(1) - χ2z(1)) + fP*(χ1z(0) - χ2z(0)))
		    data.χsx = 0.5*(fN*(χ1x(1) + χ2x(1)) + fP*(χ1x(0) + χ2x(0)))
		    data.χsy = 0.5*(fN*(χ1y(1) + χ2y(1)) + fP*(χ1y(0) + χ2y(0)))
		    data.χsz = 0.5*(fN*(χ1z(1) + χ2z(1)) + fP*(χ1z(0) + χ2z(0)))
		    CalculateStuffAtTime(τ)
		    data.V = VN
		    data.Ψ = fN*Ψpr(1) + fP*Ψpr(0) + Ψorb + Ψtail
		    Return data
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub InitializeConstants(CaseInfo As CaseInfoClass)
		  // Initialize some basic things
		  δ = CaseInfo.δ
		  χ1 = CaseInfo.χ1
		  θ1 = CaseInfo.θ1
		  φ1 = CaseInfo.φ1
		  χ2 = CaseInfo.χ2
		  θ2 = CaseInfo.θ2
		  φ2 = CaseInfo.φ2
		  Var χ1ℓ As Double = χ1*Cos(θ1)
		  Var χ2ℓ As Double = χ2*Cos(θ2)
		  η = 0.25*(1.0 - δ*δ)
		  Var η2 As Double = η*η
		  Var η3 As Double = η2*η
		  μ1 = 0.5*(1.0 + δ)
		  μ12 = μ1*μ1
		  μ2 = 0.5*(1.0 - δ)
		  μ22 = μ2*μ2
		  
		  // Calculate spin evolution coefficients
		  C10 = 0.75*(1.0 - δ) + 0.5*η
		  C20 = 0.75*(1.0 + δ) + 0.5*η
		  C12 = 9/16 + 5/4*η - η2/24 + δ*(-9/16 + 5/8*η)
		  C22 = 9/16 + 5/4*η - η2/24 - δ*(-9/16 + 5/8*η)
		  C14 = 27/32 + 3/16*η - 105/32*η2 - η3/48 + δ*(-27/32 + 39/8*η -5/32*η2)
		  C24 = 27/32 + 3/16*η - 105/32*η2 - η3/48 - δ*(-27/32 + 39/8*η -5/32*η2)
		  Sℓ = μ12*χ1ℓ + μ22*χ2ℓ
		  Σℓ =  μ2*χ2ℓ - μ1*χ2ℓ
		  L0 = η
		  L2 = 3/2 + η/6
		  L3 = -35/6*Sℓ-5/2/η2/24
		  L4 = 27/8 - 19/8*η + η2/24
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function StepWasSuccessful() As Boolean
		  // Check whether the step has been adjusted
		  If ΔτhP > ΔτhF Then // if so, adjust the past step so that "now" is centered
		    AdjustThePast
		  End If
		  
		  // Calculate the current orbital angular momentum magnitude
		  CalculateStuffAtTime(τN)
		  
		  // Calculate components of the initial spin rate of change
		  Var Ω1 As Double = V5*(C10 + C12*V2 + C14*V4)/ℓ
		  Var χ1xDotN As Double = Ω1*(ℓy(1)*χ1z(1) - ℓz(1)*χ1y(1))
		  Var χ1yDotN As Double = Ω1*(ℓz(1)*χ1x(1) - ℓx(1)*χ1z(1))
		  Var χ1zDotN As Double = Ω1*(ℓx(1)*χ1y(1) - ℓy(vN)*χ1x(1))
		  Var Ω2 As Double = V5*(C20 + C22*V2 + C24*4)/ℓ
		  Var χ2xDotN As Double = Ω2*(ℓy(1)*χ2z(1) - ℓz(1)*χ2y(1))
		  Var χ2yDotN As Double = Ω2*(ℓz(1)*χ2x(1) - ℓx(1)*χ2z(1))
		  Var χ2zDotN As Double = Ω2*(ℓx(1)*χ2y(1) - ℓy(1)*χ2x(1))
		  
		  // Evolve the spins using an leapfrog step
		  Var TwoΔτ As Double = 2.0*ΔτhF
		  Var χ1xF As Double = χ1x(0) + TwoΔτ*χ1xDotN
		  Var χ1yF As Double = χ1y(0) + TwoΔτ*χ1yDotN
		  Var χ1zF As Double = χ1z(0) + TwoΔτ*χ1zDotN
		  Var χ2xF As Double = χ2x(0) + TwoΔτ*χ2xDotN
		  Var χ2yF As Double = χ2y(0) + TwoΔτ*χ2yDotN
		  Var χ2zF As Double= χ2z(0) + TwoΔτ*χ2zDotN
		  
		  // Evolve the orbital angular momentum using a leapfrog step
		  Var vDotN As Double = VCalc.VDotForLastV
		  Var ℓDotN As Double = L0*(-1.0/V2 + L2 + 2*L2*VN + 3*L4*V2)*vDotN/ℓ
		  Var ℓxDotN As Double = -μ12*χ1xDotN - μ22*χ2xDotN + ℓDotN*ℓx(1)
		  Var ℓyDotN As Double = -μ12*χ1yDotN - μ22*χ2yDotN + ℓDotN*ℓy(1)
		  Var ℓzDotN As Double = -μ12*χ1zDotN - μ22*χ2zDotN + ℓDotN*ℓz(1)
		  Var ℓxF As Double = ℓx(0) + TwoΔτ*ℓxDotN
		  Var ℓyF As Double = ℓy(0) + TwoΔτ*ℓyDotN
		  Var ℓzF As Double = ℓz(0) + TwoΔτ*ℓzDotN
		  Var αF As Double = Atan2(ℓyF,ℓxF)
		  Var ιF As Double = Atan2(Sqrt(ℓxF*ℓxF + ℓyF*ℓyF), ℓzF)
		  
		  // Check to see whether we have crossed the 2nd/3rd quadrant line
		  Var ΔCycle As Integer = 0
		  If ℓyF < 0.0 And ℓy(1) > 0.0 Then
		    If (ℓxF*ℓy(1) - ℓx(1)*ℓyF)/(ℓy(1)-ℓyF) < 0.0 Then ΔCycle = 1
		  ElseIf ℓyF > 0.0 And ℓy(1) < 0.0 Then
		    If (ℓxF*ℓy(1) - ℓx(1)*ℓyF)/(ℓy(1)-ℓyF) < 0.0 Then ΔCycle = -1
		  End If
		  αF = αF + ΔCycle*2*π
		  
		  // Evolve the precession phase
		  Var ΨprF As Double = Ψpr(0) + TwoΔτ*αDotN*Cos(ι(1))
		  
		  // From here on, the future step just calculated becomes the present step
		  // and the present step becomes the past step
		  τP = τN
		  τN = τN + ΔτhF
		  
		  χ1x(0) = χ1x(1)
		  χ1y(0)= χ1y(1)
		  χ1z(0) = χ1z(1)
		  χ1x(1) = χ1xF
		  χ1y(1) = χ1yF
		  χ1z(1) = χ1zF
		  
		  χ2x(0) = χ2x(1)
		  χ2y(0) = χ2y(1)
		  χ2z(0) = χ2z(1)
		  χ2x(1) = χ2xF
		  χ2y(1) = χ2yF
		  χ2z(1) = χ2zF
		  
		  ℓx(0) = ℓx(1)
		  ℓy(0) = ℓy(1)
		  ℓz(0) = ℓz(1)
		  ℓx(1) = ℓxF
		  ℓy(1) = ℓyF
		  ℓz(1) = ℓzF
		  
		  Ψpr(0) = Ψpr(1)
		  Ψpr(1) = ΨprF
		  
		  α(0) = α(1)
		  α(1) = αF
		  
		  ι(0) = ι(1)
		  ι(1) = ιF
		  
		  // Calculate the ideal next time step
		  Var s1dot As Double = Sqrt(χ1xDotN*χ1xDotN + χ1yDotN*χ1yDotN + χ1zDotN*χ1zDotN)
		  Var s2dot As Double = Sqrt(χ2xDotN*χ2xDotN + χ2yDotN*χ2yDotN + χ2zDotN*χ2zDotN)
		  Var ΔτBest As Double
		  If s1dot = 0.0 Then
		    ΔτBest = χ2/s2dot
		  ElseIf s2dot = 0.0 Then
		    ΔτBest = χ1/s1dot
		  Else
		    ΔτBest = Min(χ1/s1dot, χ2/s2dot)
		  End If
		  If ΔτBest < ΔτhF Then // if we need a smaller step
		    If ΔτhP > ΔτhF Then
		      Return False // if we just did a smaller step, we are breaking down, so quit
		    Else
		      ΔτhP = ΔτhF  // store the previous step
		      ΔτhF = ΔτhF/2  // reduce the next step size by two
		    End If
		  Else // if we don't need a smaller step, repeat the current step
		    ΔτhP = ΔτhF
		  End If
		  Return True
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub TakeTheFirstStep()
		  // Calculate components of the initial spin rate of change
		  Var Ω1 As Double = V5*(C10 + C12*V2 + C14*V4)/ℓ
		  Var χ1xDot0 As Double = Ω1*(ℓy(0)*χ1z(0) - ℓz(0)*χ1y(0))
		  Var χ1yDot0 As Double = Ω1*(ℓz(0)*χ1x(0) - ℓx(0)*χ1z(0))
		  Var χ1zDot0 As Double = Ω1*(ℓx(0)*χ1y(0) - ℓy(0)*χ1x(0))
		  Var Ω2 As Double = V5*(C20 + C22*V2 + C24*V4)/ℓ
		  Var χ2xDot0 As Double = Ω2*(ℓy(0)*χ2z(0) - ℓz(0)*χ2y(0))
		  Var χ2yDot0 As Double = Ω2*(ℓz(0)*χ2x(0) - ℓx(0)*χ2z(0))
		  Var χ2zDot0 As Double = Ω2*(ℓx(0)*χ2y(0) - ℓy(0)*χ2x(0))
		  
		  // Calculate the first time step to be half the step that would
		  // take 628 steps for the fastest spin to precess once
		  Var s1dot As Double = Sqrt(χ1xDot0*χ1xDot0 + χ1yDot0*χ1yDot0 + χ1zDot0*χ1zDot0)
		  Var s2dot As Double = Sqrt(χ2xDot0*χ2xDot0 + χ2yDot0*χ2yDot0 + χ2zDot0*χ2zDot0)
		  If s1dot = 0.0 Then
		    ΔτhP = 0.5*χ2/s2dot
		  ElseIf s2dot = 0.0 Then
		    ΔτhP = 0.5*χ1/s1dot
		  Else
		    ΔτhP = 0.5*Min(χ1/s1dot, χ2/s2dot)
		  End If
		  ΔτhF = ΔτhP
		  
		  // Evolve the spins using an Euler step
		  χ1x(1) = χ1x(0) + ΔτhP*χ1xDot0
		  χ1y(1) = χ1y(0) + ΔτhP*χ1yDot0
		  χ1z(1) = χ1z(0) + ΔτhP*χ1zDot0
		  χ2x(1) = χ2x(0) + ΔτhP*χ2xDot0
		  χ2y(1) = χ2y(0) + ΔτhP*χ2yDot0
		  χ2z(1) = χ2z(0) + ΔτhP*χ2zDot0
		  
		  // Evolve the orbital angular momentum using an Euler step
		  Var vDot0 As Double = VCalc.VDotForLastV
		  Var ℓDot0 As Double = L0*(-1.0/V2 + L2 + 2.0*L2*V0 + 3.0*L4*V2)*vDot0/ℓ
		  Var ℓxDot0 As Double = -μ12*χ1xDot0 - μ22*χ2xDot0 + ℓDot0*ℓx(0)
		  Var ℓyDot0 As Double = -μ12*χ1yDot0 - μ22*χ2yDot0 + ℓDot0*ℓy(0)
		  Var ℓzDot0 As Double = -μ12*χ1zDot0 - μ22*χ2zDot0 + ℓDot0*ℓz(0)
		  ℓx(1) = ℓx(0) + ΔτhP*ℓxDot0
		  ℓy(1) = ℓy(0) + ΔτhP*ℓyDot0
		  ℓz(1) = ℓz(0) + ΔτhP*ℓzDot0
		  
		  // Calculate things at this time step
		  CalculateStuffAtTime(ΔτhP)
		  
		  // Calculate components of the future spin rate of change
		  Ω1 = V5*(C10 + C12*V2 + C14*V4)/ℓ
		  Var χ1xDot1 As Double = Ω1*(ℓy(1)*χ1z(1) - ℓz(1)*χ1y(1))
		  Var χ1yDot1 As Double = Ω1*(ℓz(1)*χ1x(1) - ℓx(1)*χ1z(1))
		  Var χ1zDot1 As Double = Ω1*(ℓx(1)*χ1y(1) - ℓy(1)*χ1x(1))
		  Ω2 = V5*(C20 + C22*V2 + C24*V4)/ℓ
		  Var χ2xDot1 As Double = Ω2*(ℓy(1)*χ2z(1) - ℓz(1)*χ2y(1))
		  Var χ2yDot1 As Double = Ω2*(ℓz(1)*χ2x(1) - ℓx(1)*χ2z(1))
		  Var χ2zDot1 As Double = Ω2*(ℓx(1)*χ2y(1) - ℓy(1)*χ2x(1))
		  
		  // Evolve the spins using a more correct step
		  χ1x(1) = χ1x(0) + 0.5*ΔτhP*(χ1xDot0 + χ1xDot1)
		  χ1y(1) = χ1y(0) + 0.5*ΔτhP*(χ1yDot0 + χ1yDot1)
		  χ1z(1) = χ1z(0) + 0.5*ΔτhP*(χ1zDot0 + χ1zDot1)
		  χ2x(1) = χ2x(0) + 0.5*ΔτhP*(χ2xDot0 + χ2xDot1)
		  χ2y(1) = χ2y(0) + 0.5*ΔτhP*(χ2yDot0 + χ2yDot1)
		  χ2z(1) = χ2z(0) + 0.5*ΔτhP*(χ2zDot0 + χ2zDot1)
		  
		  // Evolve the orbital angular momentum using a more correct step
		  Var vDot1 As Double = VCalc.VDotForLastV
		  Var ℓDot1 As Double = L0*(-1.0/V2 + L2 + 2*L2*VN + 3*L4*V2)*vDot1/ℓ
		  Var ℓxDot1 As Double = -μ12*χ1xDot1 - μ22*χ2xDot1 + ℓDot1*ℓx(1)
		  Var ℓyDot1 As Double = -μ12*χ1yDot1 - μ22*χ2yDot1 + ℓDot1*ℓy(1)
		  Var ℓzDot1 As Double = -μ12*χ1zDot1 - μ22*χ2zDot1 + ℓDot1*ℓz(1)
		  ℓx(1) = ℓx(0) + ΔτhP*0.5*(ℓxDot1 + ℓxDot0)
		  ℓy(1) = ℓy(0) + ΔτhP*0.5*(ℓyDot1 + ℓyDot0)
		  ℓz(1) = ℓz(0) + ΔτhP*0.5*(ℓzDot1 + ℓzDot0)
		  α(1) = Atan2(ℓy(1),ℓx(1))
		  ι(1) = Atan2(Sqrt(ℓx(1)*ℓx(1) + ℓy(1)*ℓy(1)), ℓz(1))
		  
		  // Check to see whether we have crossed the 2nd/3rd quadrant line
		  Var ΔCycles As Integer = 0
		  If ℓy(1) < 0.0 And ℓy(0) > 0.0 Then
		    If (ℓx(1)*ℓy(0) - ℓx(0)*ℓy(1))/(ℓy(0)-ℓy(1)) < 0.0 Then ΔCycles = 1
		  ElseIf ℓy(1) > 0.0 And ℓy(0) < 0.0 Then
		    If (ℓx(1)*ℓy(0) - ℓx(0)*ℓy(1))/(ℓy(0)-ℓy(1)) < 0.0 Then ΔCycles = -1
		  End If
		  α(1) = α(1) + ΔCycles*2*π
		  
		  // Initialize the precession phase
		  Ψpr(0) = 0.0
		  Ψpr(1) = -0.5*ΔτhP*(αDot0*Cos(ι(0)) + αDot1*Cos(ι(1)))
		  
		  // Finally, update the times
		  τP = 0.0
		  τN = ΔτhP
		End Sub
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
		Private L0 As Double
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
		Private NoPrecession As Boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Sℓ As Double
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
		Private VCalc As VCalculatorClass
	#tag EndProperty

	#tag Property, Flags = &h21
		Private VN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private μ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private μ12 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private μ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private μ22 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ι(1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private α(1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private δ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ΔτhF As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ΔτhP As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private η As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private θ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private θ2 As Double
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
		Private φ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private φ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1x(1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1y(1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ1z(1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2x(1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2y(1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private χ2z(1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Ψorb As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Ψpr(1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Ψtail As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ℓx(1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ℓy(1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ℓz(1) As Double
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
