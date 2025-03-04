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
		  
		  For dq As Integer = 0 To DLastSpinIndex
		    Dχ1xI(dq,0) = 0.5*(Dχ1xI(dq,1) + Dχ1xI(dq,0))
		    Dχ1yI(dq,0) = 0.5*(Dχ1yI(dq,1) + Dχ1yI(dq,0))
		    Dχ1zI(dq,0) = 0.5*(Dχ1zI(dq,1) + Dχ1zI(dq,0))
		    
		    Dχ2xI(dq,0) = 0.5*(Dχ2xI(dq,1) + Dχ2xI(dq,0))
		    Dχ2yI(dq,0) = 0.5*(Dχ2yI(dq,1) + Dχ2yI(dq,0))
		    Dχ2zI(dq,0) = 0.5*(Dχ2zI(dq,1) + Dχ2zI(dq,0))
		    
		    DℓxI(dq,0) = 0.5*(DℓxI(dq,1) + DℓxI(dq,0))
		    DℓyI(dq,0) = 0.5*(DℓyI(dq,1) + DℓyI(dq,0))
		    DℓzI(dq,0) = 0.5*(DℓzI(dq,1) + DℓzI(dq,0))
		    
		    DιI(dq,0) = 0.5*(DιI(dq,1) + DιI(dq,0))
		    
		    DαI(dq,0) = 0.5*(DαI(dq,1) + DαI(dq,0))
		    
		    DΨprI(dq,0) = 0.5*(DΨprI(dq,1) + DΨprI(dq,0))
		  Next
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
		  
		  // Declare derivatives of the total angular momentum components
		  Var dj0xI(7) As Double
		  Var dj0yI(7) As Double
		  Var dj0zI(7) As Double
		  Var dj0fI(7) As Double
		  Var dinvj0fI(7) As Double
		  Var dinvj0I(7) As Double
		  
		  // Declare derivatives of initial spins
		  Var dχ1xLI(7) As Double
		  Var dχ1yLI(7) As Double
		  Var dχ1zLI(7) As Double
		  Var dχ2xLI(7) As Double
		  Var dχ2yLI(7) As Double
		  Var dχ2zLI(7) As Double
		  
		  // Calculate δ-derivatives of the above quantities
		  dχ1xLI(Dδ) = 0.0
		  dχ1yLI(Dδ) = 0.0
		  dχ1zLI(Dδ) = 0.0
		  dχ2xLI(Dδ) = 0.0
		  dχ2yLI(Dδ) = 0.0
		  dχ2zLI(Dδ) = 0.0
		  dj0xI(Dδ) = μ1*χ1xL - μ2*χ2xL
		  dj0yI(Dδ) = μ1*χ1yL - μ2*χ2yL
		  dj0zI(Dδ) = μ1*χ1zL - μ2*χ2zL + DℓI(Dδ)
		  
		  // Calculate τc-derivatives of the above quantities
		  dχ1xLI(Dτc) = 0.0
		  dχ1yLI(Dτc) = 0.0
		  dχ1zLI(Dτc) = 0.0
		  dχ2xLI(Dτc) = 0.0
		  dχ2yLI(Dτc) = 0.0
		  dχ2zLI(Dτc) = 0.0
		  dj0xI(Dτc) = 0.0
		  dj0yI(Dτc) = 0.0
		  dj0zI(Dτc) = dℓI(Dτc)
		  
		  // Calculate χ1-derivatives of the above quantities
		  dχ1xLI(Dχ1) = Sin(θ1)*Cos(φ1)
		  dχ1yLI(Dχ1) = Sin(θ1)*Sin(φ1)
		  dχ1zLI(Dχ1) = Cos(θ1)
		  dχ2xLI(Dχ1) = 0.0
		  dχ2yLI(Dχ1) = 0.0
		  dχ2zLI(Dχ1) = 0.0
		  dj0xI(Dχ1) = μ12*dχ1xLI(Dχ1)
		  dj0yI(Dχ1) = μ12*dχ1yLI(Dχ1)
		  dj0zI(Dχ1) = μ12*dχ1zLI(Dχ1) + dℓI(Dχ1)
		  
		  // Calculate θ1-derivatives of the above quantities
		  dχ1xLI(Dθ1) = χ1*Cos(θ1)*Cos(φ1)
		  dχ1yLI(Dθ1) = χ1*Cos(θ1)*Sin(φ1)
		  dχ1zLI(Dθ1)= -χ1*Sin(θ1)
		  dχ2xLI(Dθ1) = 0.0
		  dχ2yLI(Dθ1) = 0.0
		  dχ2zLI(Dθ1) = 0.0
		  dj0xI(Dθ1) = μ12*dχ1xLI(Dθ1)
		  dj0yI(Dθ1) = μ12*dχ1yLI(Dθ1)
		  dj0zI(Dθ1) = μ12*dχ1zLI(Dθ1) + dℓI(Dθ1)
		  
		  // Calculate φ1-derivatives of the above quantities
		  dχ1xLI(Dφ1) = -χ1*Sin(θ1)*Sin(φ1)
		  dχ1yLI(Dφ1)  = χ1*Sin(θ1)*Cos(φ1)
		  dχ1zLI(Dφ1)  = 0.0
		  dχ2xLI(Dφ1) = 0.0
		  dχ2yLI(Dφ1) = 0.0
		  dχ2zLI(Dφ1) = 0.0
		  dj0xI(Dφ1) = μ12*dχ1xLI(Dφ1)
		  dj0yI(Dφ1) = μ12*dχ1yLI(Dφ1)
		  dj0zI(Dφ1) = 0.0
		  
		  // Calculate χ2-derivatives of the above quantities
		  dχ1xLI(Dχ2) = 0.0
		  dχ1yLI(Dχ2) = 0.0
		  dχ1zLI(Dχ2) = 0.0
		  dχ2xLI(Dχ2) = Sin(θ2)*Cos(φ2)
		  dχ2yLI(Dχ2) = Sin(θ2)*Sin(φ2)
		  dχ2zLI(Dχ2) = Cos(θ2)
		  dj0xI(Dχ2) = μ22*dχ2xLI(Dχ2)
		  dj0yI(Dχ2) = μ22*dχ2yLI(Dχ2)
		  dj0zI(Dχ2) = μ22*dχ2zLI(Dχ2) + dℓI(Dχ2)
		  
		  // Calculate θ2-derivatives of the above quantities
		  dχ1xLI(Dθ2) = 0.0
		  dχ1yLI(Dθ2) = 0.0
		  dχ1zLI(Dθ2) = 0.0
		  dχ2xLI(Dθ2) = χ2*Cos(θ2)*Cos(φ2)
		  dχ2yLI(Dθ2) = χ2*Cos(θ2)*Sin(φ2)
		  dχ2zLI(Dθ2) = -χ2*Sin(θ2)
		  dj0xI(Dθ2) = μ22*dχ2xLI(Dθ2)
		  dj0yI(Dθ2) = μ22*dχ2yLI(Dθ2)
		  dj0zI(Dθ2) = μ22*dχ2zLI(Dθ2) + dℓI(Dθ2)
		  
		  // Calculate φ2-derivatives of the above quantities
		  dχ1xLI(Dφ2) = 0.0
		  dχ1yLI(Dφ2) = 0.0
		  dχ1zLI(Dφ2) = 0.0
		  dχ2xLI(Dφ2) = -χ2*Sin(θ2)*Sin(φ2)
		  dχ2yLI(Dφ2) = χ2*Sin(θ2)*Cos(φ2)
		  dχ2zLI(Dφ2) = 0.0
		  dj0xI(Dφ2) = μ22*dχ2xLI(Dφ2)
		  dj0yI(Dφ2) = μ22*dχ2yLI(Dφ2)
		  dj0zI(Dφ2) = 0.0
		  
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
		  
		  // Declare rotation matrix derivatives
		  Var drxxI(7) As Double
		  Var drxyI(7) As Double
		  Var drxzI(7) As Double
		  Var dryxI(7) As Double
		  Var dryyI(7) As Double
		  Var dryzI(7) As Double
		  Var drzxI(7) As Double
		  Var drzyI(7) As Double
		  Var drzzI(7) As Double
		  
		  // Calculate derivatives of the rotation matrix
		  For dq As Integer = 0 To DLastSpinIndex
		    dj0fI(dq) = (j0x*dj0xI(dq) + j0y*dj0yI(dq))/j0f
		    dinvj0I(dq) = -dj0fI(dq)/(j0f*j0f*j0f)
		    dinvj0I(dq) = -(j0x*dj0xI(dq) + j0y*dj0yI(dq) + j0z*dj0yI(dq))/(j0*j0*j0)
		    drxxI(dq) = dj0zI(dq)*j0x/(j0*j0f) _
		    + dj0xI(dq)*j0z/(j0*j0f) _
		    + dinvj0I(dq)*j0z*j0x/j0f _
		    + dinvj0fI(dq)*j0z*j0x/j0
		    drxyI(dq) = dj0zI(dq)*j0y/(j0*j0f) _
		    + dj0yI(dq)*j0z/(j0*j0f) _
		    + dinvj0I(dq)*j0z*j0y/j0f _
		    + dinvj0fI(dq)*j0z*j0y/j0
		    drxzI(dq) = -dj0fI(dq)/j0 - dinvj0I(dq)*j0f
		    dryxI(dq) = -dj0yI(dq)/j0f - dinvj0fI(dq)*j0y
		    dryyI(dq) = -dj0xI(dq)/j0f - dinvj0fI(dq)*j0x
		    dryzI(dq) = 0.0
		    drzxI(dq) = dj0xI(dq)/j0 + dinvj0I(dq)*j0x
		    drzyI(dq) = dj0yI(dq)/j0 + dinvj0I(dq)*j0y
		    drzzI(dq) = dj0zI(dq)/j0 + dinvj0I(dq)*j0z
		  Next
		  
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
		  
		  // Calculate initial values of all the derivatives
		  For dq As Integer = 0 To DLastSpinIndex
		    Dχ1xI(dq,0) = rxx*dχ1xLI(dq) + rxy*dχ1yLI(dq)  + rxz*dχ1zLI(dq) _
		    +drxxI(dq)*χ1xL + drxyI(dq)*χ1yL  + drxzI(dq)*χ1zL
		    Dχ1yI(dq,0) = ryx*dχ1xLI(dq) + ryy*dχ1yLI(dq)  + ryz*dχ1zLI(dq) _
		    +dryxI(dq)*χ1xL + dryyI(dq)*χ1yL  + dryzI(dq)*χ1zL
		    Dχ1zI(dq,0) = rzx*dχ1xLI(dq) + rzy*dχ1yLI(dq)  + rzz*dχ1zLI(dq) _
		    +drzxI(dq)*χ1xL + drzyI(dq)*χ1yL  + drzzI(dq)*χ1zL
		    Dχ2xI(dq,0) = rxx*dχ2xLI(dq) + rxy*dχ2yLI(dq)  + rxz*dχ2zLI(dq) _
		    +drxxI(dq)*χ2xL + drxyI(dq)*χ2yL  + drxzI(dq)*χ2zL
		    Dχ2yI(dq,0) = ryx*dχ2xLI(dq) + ryy*dχ2yLI(dq)  + ryz*dχ2zLI(dq) _
		    +dryxI(dq)*χ2xL + dryyI(dq)*χ2yL  + dryzI(dq)*χ2zL
		    Dχ2zI(dq,0) = rzx*dχ2xLI(dq) + rzy*dχ2yLI(dq)  + rzz*dχ2zLI(dq) _
		    +drzxI(dq)*χ2xL + drzyI(dq)*χ2yL  + drzzI(dq)*χ2zL
		    DℓxI(dq,0) = rxz*dℓI(dq) + drxzI(dq)*ℓ
		    DℓyI(dq,0) = ryz*dℓI(dq) + dryzI(dq)*ℓ
		    DℓzI(dq,0) = rzz*dℓI(dq) + drzzI(dq)*ℓ
		    DαI(dq,0) = (ℓy(0)*DℓxI(dq,0) - ℓx(0)*DℓyI(dq,0))/(ℓ0f*ℓ0f)
		    DιI(dq,0) = (ℓ0f*DℓzI(dq,0) - ℓz(0)*(ℓx(0)*DℓxI(dq,0) + ℓy(0)*DℓyI(dq,0)/ℓ0f))/(ℓ0f*ℓ0f + ℓz(0)*ℓz(0))
		  Next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub CalculateVAndℓAtTime(τ As Double)
		  // Calculate v and its powers at the given time
		  VN = VCalc.VAtTime(τ)
		  V2 = VN*VN
		  V3 = V2*VN
		  V4 = V3*VN
		  V5 = V4*VN
		  V6 = V5*VN
		  V7 = V6*VN
		  
		  // Calculate derivatives of V at the time in question
		  DvI(Dδ) = VCalc.DVIdδForLastV
		  DvI(Dτc) = VCalc.DVIdτcForLastV
		  Var dvIdχ1ℓ As Double = VCalc.DVIdχ1ℓForLastV
		  DvI(Dχ1) = dvIdχ1ℓ*Cos(θ1)
		  DvI(Dθ1) = -dvIdχ1ℓ*χ1*Sin(θ1)
		  DvI(Dφ1) = 0.0
		  Var dvIdχ2ℓ As Double = VCalc.DVIdχ2ℓForLastV
		  DvI(Dχ2) = dvIdχ2ℓ*Cos(θ2)
		  DvI(Dθ2) = -dvIdχ2ℓ*χ2*Sin(θ2)
		  DvI(Dφ2) = 0.0
		  
		  // Calculate the orbital angular momentum magnitude and its derivatives
		  // at the given time
		  ℓ = L0/V0*(1.0 + L2*V2 + L3*V3 + L4*V4)
		  DℓIdv = L0*(-1.0/V2 + L2 + 2.0*L3*V0 + 3.0*L4*V2)
		  For dq As Integer = 0 To DLastSpinIndex
		    DℓI(dq) = dL0I(dq)/V0*(1.0 + L2*V2 + L3*V3 + L4*V4) _
		    + L0/V0*(DL2I(dq)*V2 + DL3I(dq)*V3 + DL4I(dq)*V4) _
		    + DℓIdv*DvI(dq)
		  Next
		  
		  // Calculate the orbital phase and its derivatives at the time in question
		  Ψmp = VCalc.ΨmpForLastV
		  DΨmpI(Dδ) = VCalc.DΨmpIdδForLastV
		  DΨmpI(Dτc) = VCalc.DΨmpIdτcForLastV
		  Var dΨmpIdχ1ℓ As Double = VCalc.DΨmpIdχ1ℓForLastV
		  DΨmpI(Dχ1) = dΨmpIdχ1ℓ*Cos(θ1)
		  DΨmpI(Dθ1) = -dΨmpIdχ1ℓ*χ1*Sin(θ1)
		  Var dΨmpIχ2ℓ As Double = VCalc.DΨmpIdχ2ℓForLastV
		  DΨmpI(Dφ1) = 0.0
		  DΨmpI(Dχ2) = dΨmpIχ2ℓ*Cos(θ2)
		  DΨmpI(Dθ2) = -dΨmpIχ2ℓ*χ2*Sin(θ2)
		  DΨmpI(Dφ2) = 0.0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(CaseInfo As CaseInfoClass)
		  If (CaseInfo.χ1 = 0 And CaseInfo.χ2 = 0) Or (CaseInfo.θ1 = 0 And CaseInfo.θ2 = 0) Then
		    NoPrecession = True
		    VCalc = New VCalculatorClass(CaseInfo.τc, CaseInfo.δ, CaseInfo.χ1*Cos(CaseInfo.θ1), CaseInfo.χ2*Cos(CaseInfo.θ2), CaseInfo.λ0)
		  Else
		    NoPrecession = False
		    InitializeConstants(CaseInfo)
		    VCalc = New VCalculatorClass(CaseInfo.τc, δ, χ1*Cos(θ1), χ2*Cos(θ2), CaseInfo.λ0)
		    CalculateVAndℓAtTime(0.0)
		    V0 = VN
		    CalculateInitialSpins
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
		    data.α = 3.14159265358979324
		    data.χax = 0.0
		    data.χay = 0.0
		    data.χaz = 0.5*(χ1-χ2)
		    data.χsx = 0.0
		    data.χsy = 0.0
		    data.χsz = 0.5*(χ1+χ2)
		    CalculateVAndℓAtTime(τ)
		    data.V = VN
		    For dq As Integer = 0 To DLastSpinIndex
		      data.DιI(dq) = 0.0
		      data.DαI(dq) = 0.0
		      data.DχaxI(dq) = 0.0
		      data.DχayI(dq) = 0.0
		      data.DχsxI(dq) = 0.0
		      data.DχsyI(dq) = 0.0
		      If dq = Dχ1 Then
		        data.DχazI(dq) = 0.5
		        data.DχszI(dq) = 0.5
		      ElseIf dq = Dχ2 Then
		        data.DχazI(dq) = -0.5
		        data.DχszI(dq) = 0.5
		      Else
		        data.DχazI(dq) = 0.0
		        data.DχszI(dq) = 0.0
		      End If
		      data.DvI(dq) = DvI(dq)
		      data.DΨI(dq) = DΨmpI(dq)
		    Next
		    data.Ψ = Ψmp
		    Return data
		  Else // If we have at least one nonzero spin, then we need to evolve
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
		    data.Ψ = fN*Ψpr(1) + fP*Ψpr(0) + Ψmp
		    CalculateVAndℓAtTime(τ)
		    data.V = VN
		    For dq As Integer = 0 To DLastSpinIndex
		      data.DιI(dq) = fN*DιI(dq,1) + fP*DιI(dq,0)
		      data.DαI(dq) = fN*DαI(dq,1) + fP*DαI(dq,0)
		      data.DχaxI(dq) = 0.5*(fN*(Dχ1xI(dq,1) - Dχ2xI(dq,1)) + fP*(Dχ1xI(dq,0) - Dχ2xI(dq,0)))
		      data.DχayI(dq) = 0.5*(fN*(Dχ1yI(dq,1) - Dχ2yI(dq,1)) + fP*(Dχ1yI(dq,0) - Dχ2yI(dq,0)))
		      data.DχazI(dq) = 0.5*(fN*(Dχ1zI(dq,1) - Dχ2zI(dq,1)) + fP*(Dχ1zI(dq,0) - Dχ2zI(dq,0)))
		      data.DχsxI(dq) = 0.5*(fN*(Dχ1xI(dq,1) + Dχ2xI(dq,1)) + fP*(Dχ1xI(dq,0) + Dχ2xI(dq,0)))
		      data.DχsyI(dq) = 0.5*(fN*(Dχ1yI(dq,1) + Dχ2yI(dq,1)) + fP*(Dχ1yI(dq,0) + Dχ2yI(dq,0)))
		      data.DχszI(dq) = 0.5*(fN*(Dχ1zI(dq,1) + Dχ2zI(dq,1)) + fP*(Dχ1zI(dq,0) + Dχ2zI(dq,0)))
		      data.DvI(dq) = DvI(dq)
		      data.DΨI(dq) = fN*DΨprI(dq,1) + fP*DΨprI(dq,0) + DΨmpI(dq)
		    Next
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
		  μ1 = 0.5*(1.0 + δ)
		  μ12 = μ1*μ1
		  μ2 = 0.5*(1.0 - δ)
		  μ22 = μ2*μ2
		  Var η2 As Double = η*η
		  Var η3 As Double = η2*η
		  
		  
		  
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
		  
		  // Calculate δ-derivatives of these constants
		  DC10I(Dδ) = 0.75 - 0.25*δ
		  DC12I(Dδ) = -5/8*δ + η*δ/24 - 9/16 + 5/8*η - 5/16*δ
		  DC22I(Dδ)  = -5/8*δ + η*δ/24 + 9/16 - 5/8*η + 5/16*δ
		  DC14I(Dδ)  = -3/32*δ + 105/32*η*δ + 3/96*η2*δ - 27/32 + 39/8*η - 39/16*δ - 5/32*η2 - 5/32*η*δ
		  DC24I(Dδ)  = -3/32*δ + 105/32*η*δ + 3/96*η2*δ + 27/32 - 39/8*η + 39/16*δ + 5/32*η2 + 5/32*η*δ
		  DSℓI(Dδ)  = μ1*χ1*Cos(θ1) - μ2*χ2*Cos(θ2)
		  DΣℓI(Dδ) = -0.5*χ2*Cos(θ2) - 0.5*χ1*Cos(θ1)
		  DL0I(Dδ)  = -0.5*δ
		  DL2I(Dδ) = -δ/12
		  DL3I(Dδ)  = -35/6*DSℓI(Dδ) - 5/2*Σℓ - 5/2*δ*DΣℓI(Dδ)
		  DL4I(Dδ)  = 19/16*δ - δ*η/24
		  
		  // Calculate τc-derivatives of these constants
		  DC10I(Dτc) = 0.0
		  DC10I(Dτc) = 0.0
		  DC12I(Dτc) = 0.0
		  DC22I(Dτc) = 0.0
		  DC14I(Dτc) = 0.0
		  DC24I(Dτc) = 0.0
		  DSℓI(Dτc) = 0.0
		  DΣℓI(Dτc) = 0.0
		  DL0I(Dτc) = 0.0
		  DL2I(Dτc) = 0.0
		  DL3I(Dτc) = 0.0
		  DL4I(Dτc) = 0.0
		  
		  // Calculate χ1-derivatives of these constants
		  DC10I(Dχ1) = 0.0
		  DC10I(Dχ1) = 0.0
		  DC12I(Dχ1) = 0.0
		  DC22I(Dχ1) = 0.0
		  DC14I(Dχ1) = 0.0
		  DC24I(Dχ1) = 0.0
		  DSℓI(Dχ1) = μ12*Cos(θ1)
		  DΣℓI(Dχ1) = -μ1*Cos(θ1)
		  DL0I(Dχ1) = 0.0
		  DL2I(Dχ1) = 0.0
		  DL3I(Dχ1) = -35/6*DSℓI(Dχ1) - 5/2*δ*DΣℓI(Dχ1)
		  DL4I(Dχ1) = 0.0
		  
		  // Calculate θ1-derivatives of these constants
		  DC10I(Dθ1) = 0.0
		  DC10I(Dθ1) = 0.0
		  DC12I(Dθ1) = 0.0
		  DC22I(Dθ1) = 0.0
		  DC14I(Dθ1) = 0.0
		  DC24I(Dθ1) = 0.0
		  DSℓI(Dθ1) = -μ12*χ1*Sin(θ1)
		  DΣℓI(Dθ1) = μ1*χ1*Sin(θ1)
		  DL0I(Dθ1) = 0.0
		  DL2I(Dθ1) = 0.0
		  DL3I(Dθ1) = -35/6*DSℓI(Dθ1) - 5/2*δ*DΣℓI(Dθ1)
		  DL4I(Dθ1) = 0.0
		  
		  // Calculate φ1-derivatives of these constants
		  DC10I(Dφ1) = 0.0
		  DC10I(Dφ1) = 0.0
		  DC12I(Dφ1) = 0.0
		  DC22I(Dφ1) = 0.0
		  DC14I(Dφ1) = 0.0
		  DC24I(Dφ1) = 0.0
		  DSℓI(Dφ1) = 0.0
		  DΣℓI(Dφ1) = 0.0
		  DL0I(Dφ1) = 0.0
		  DL2I(Dφ1) = 0.0
		  DL3I(Dφ1) = 0.0
		  DL4I(Dφ1) = 0.0
		  
		  // Calculate χ2-derivatives of these constants
		  DC10I(Dχ2) = 0.0
		  DC10I(Dχ2) = 0.0
		  DC12I(Dχ2) = 0.0
		  DC22I(Dχ2) = 0.0
		  DC14I(Dχ2) = 0.0
		  DC24I(Dχ2) = 0.0
		  DSℓI(Dχ2) = μ22*Cos(θ2)
		  DΣℓI(Dχ2) = μ2*Cos(θ2)
		  DL0I(Dχ2) = 0.0
		  DL2I(Dχ2) = 0.0
		  DL3I(Dχ2) = -35/6*DSℓI(Dχ2) - 5/2*δ*DΣℓI(Dχ2)
		  DL4I(Dχ2) = 0.0
		  
		  // Calculate θ2-derivatives of these constants
		  DC10I(Dθ2) = 0.0
		  DC10I(Dθ2) = 0.0
		  DC12I(Dθ2) = 0.0
		  DC22I(Dθ2) = 0.0
		  DC14I(Dθ2) = 0.0
		  DC24I(Dθ2) = 0.0
		  DSℓI(Dθ2) = -μ22*χ2*Sin(θ2)
		  DΣℓI(Dθ2) = -μ2*Sin(θ2)
		  DL0I(Dθ2) = 0.0
		  DL2I(Dθ2) = 0.0
		  DL3I(Dθ2) = -35/6*χ2*DSℓI(Dθ2) - 5/2*δ*DΣℓI(Dθ2)
		  DL4I(Dθ2) = 0.0
		  
		  // Calculate φ1-derivatives of these constants
		  DC10I(Dφ2) = 0.0
		  DC10I(Dφ2) = 0.0
		  DC12I(Dφ2) = 0.0
		  DC22I(Dφ2) = 0.0
		  DC14I(Dφ2) = 0.0
		  DC24I(Dφ2) = 0.0
		  DSℓI(Dφ2) = 0.0
		  DΣℓI(Dφ2) = 0.0
		  DL0I(Dφ2) = 0.0
		  DL2I(Dφ2) = 0.0
		  DL3I(Dφ2) = 0.0
		  DL4I(Dφ2) = 0.0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function StepWasSuccessful() As Boolean
		  // Check whether the step has been adjusted
		  If ΔτhP > ΔτhF Then // if so, adjust the past step so that "now" is centered
		    AdjustThePast
		  End If
		  
		  // Calculate the current orbital angular momentum magnitude
		  CalculateVAndℓAtTime(τN)
		  
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
		  
		  // Now we handle evolving the derivatives
		  // First declare the arrays
		  Var dΩ1I(7) As Double
		  Var dΩ2I(7) As Double
		  Var dΩ1xI(7) As Double
		  Var dΩ1yI(7) As Double
		  Var dΩ1zI(7) As Double
		  Var dΩ2xI(7) As Double
		  Var dΩ2yI(7) As Double
		  Var dΩ2zI(7) As Double
		  Var dχ1xDotI(7) As Double
		  Var dχ1yDotI(7) As Double
		  Var dχ1zDotI(7) As Double
		  Var dχ2xDotI(7) As Double
		  Var dχ2yDotI(7) As Double
		  Var dχ2zDotI(7) As Double
		  Var dχ1xFI(7) As Double
		  Var dχ1yFI(7) As Double
		  Var dχ1zFI(7) As Double
		  Var dχ2xFI(7) As Double
		  Var dχ2yFI(7) As Double
		  Var dχ2zFI(7) As Double
		  Var dℓDotI(7) As Double
		  Var dℓxDotI(7) As Double
		  Var dℓyDotI(7) As Double
		  Var dℓzDotI(7) As Double
		  Var dℓxFI(7) As Double
		  Var dℓyFI(7) As Double
		  Var dℓzFI(7) As Double
		  
		  // Get values of the derivatives of vDot at the present time
		  Var dvDotI(7) As Double
		  dvDotI(Dδ)  = VCalc.DVDotIdδForLastV
		  dvDotI(Dτc) = VCalc.DVDotIdτcForLastV
		  Var dvDotIdχ1ℓ As Double = VCalc.DVDotIdχ1ℓForLastV
		  dvDotI(Dχ1) = dvDotIdχ1ℓ*Cos(θ1)
		  dvDotI(Dθ1) = -dvDotIdχ1ℓ*χ1*Sin(θ1)
		  dvDotI(Dφ1) = 0.0
		  Var dvDotIdχ2ℓ As Double = VCalc.DVDotIdχ2ℓForLastV
		  dvDotI(Dχ2) = dvDotIdχ2ℓ*Cos(θ2)
		  dvDotI(Dθ2) = -dvDotIdχ2ℓ*χ2*Sin(θ2)
		  dvDotI(Dφ2) = 0.0
		  
		  // Evolve the derivatives of the spin and orbital angular momenta
		  For dq As Integer = 0 To DLastSpinIndex
		    
		    // Calculate derivatives of the spin-dot quantities
		    dΩ1I(dq) = V4*(5.0*C10 + 7.0*C12*V2 + 9.0*C14*V4)*DvI(dq)/ℓ _
		    + V5*(DC10I(dq) + DC12I(dq)*V2 + DC14I(dq)*V4)/ℓ
		    dΩ1xI(dq) = dΩ1I(dq)*ℓx(1) + Ω1*(DℓxI(dq,1) - ℓx(1)*DℓI(dq)/ℓ)
		    dΩ1yI(dq) = dΩ1I(dq)*ℓy(1) + Ω1*(DℓyI(dq,1) - ℓy(1)*DℓI(dq)/ℓ)
		    dΩ1zI(dq) = dΩ1I(dq)*ℓz(1) + Ω1*(DℓzI(dq,1) - ℓz(1)*DℓI(dq)/ℓ)
		    dΩ2I(dq) = V4*(5.0*C20 + 7.0*C22*V2 + 9.0*C24*V4)*DvI(dq)/ℓ _
		    + V5*(DC20I(dq) + DC22I(dq)*V2 + DC24I(dq)*V4)/ℓ
		    dΩ2xI(dq) = dΩ2I(dq)*ℓx(1) + Ω2*(DℓxI(dq,1) - ℓx(1)*DℓI(dq)/ℓ)
		    dΩ2yI(dq) = dΩ2I(dq)*ℓy(1) + Ω2*(DℓyI(dq,1) - ℓy(1)*DℓI(dq)/ℓ)
		    dΩ2zI(dq) = dΩ2I(dq)*ℓz(1) + Ω2*(DℓzI(dq,1) - ℓz(1)*dℓI(dq)/ℓ)
		    dχ1xDotI(dq) = dΩ1yI(dq)*χ1z(1) - dΩ1zI(dq)*χ1y(1) + χ1yDotN*Dχ1zI(dq,1) - χ1zDotN*Dχ1yI(dq,1)
		    dχ1yDotI(dq) = dΩ1zI(dq)*χ1x(1) - dΩ1xI(dq)*χ1z(1) + χ1zDotN*Dχ1xI(dq,1) - χ1xDotN*Dχ1zI(dq,1)
		    dχ1zDotI(dq) = dΩ1xI(dq)*χ1y(1) - dΩ1yI(dq)*χ1x(1) + χ1xDotN*Dχ1yI(dq,1) - χ1yDotN*Dχ1xI(dq,1)
		    dχ2xDotI(dq) = dΩ2yI(dq)*χ2z(1) - dΩ2zI(dq)*χ2y(1) + χ2yDotN*Dχ2zI(dq,1) - χ2zDotN*Dχ2yI(dq,1)
		    dχ2yDotI(dq) = dΩ2zI(dq)*χ2x(0) - dΩ2xI(dq)*χ2z(1) + χ2zDotN*Dχ2xI(dq,1) - χ2xDotN*Dχ2zI(dq,1)
		    dχ2zDotI(dq) = dΩ2xI(dq)*χ2y(1) - dΩ2yI(dq)*χ2x(1) + χ2xDotN*Dχ2yI(dq,1) - χ2yDotN*Dχ2xI(dq,1)
		    
		    // Evolve the spin derivatives using a leapfrog step
		    dχ1xFI(dq) = Dχ1xI(dq,0) + TwoΔτ*dχ1xDotI(dq)
		    dχ1yFI(dq) = Dχ1yI(dq,0) + TwoΔτ*dχ1yDotI(dq)
		    dχ1zFI(dq) = Dχ1zI(dq,0) + TwoΔτ*dχ1zDotI(dq)
		    
		    // Calculate derivatives of angular momenta time derivatives
		    dℓDotI(dq) = DL0I(dq)*(-1.0/V2 + L2 + 2.0*L3*VN + 3.0*L4*V2)*vDotN _
		    + L0*(2.0/V3 + 2.0*L3 + 6.0*L4*VN)*DvI(dq) + DℓIdv*dvDotI(dq) _
		    + L0*(DL2I(dq) + 2.0*DL3I(dq)*VN + 3.0*DL4I(dq)*V2)*vDotN
		    dℓxDotI(dq) = -μ12*dχ1xDotI(dq) - μ22*dχ2xDotI(dq) _
		    + dℓDotI(dq)*ℓx(1)/ℓ - ℓDotN*DℓI(dq)*ℓx(1)/(ℓ*ℓ) + ℓDotN/ℓ*DℓxI(dq,1)
		    dℓyDotI(dq) = -μ12*dχ1yDotI(dq) - μ22*dχ2yDotI(dq) _
		    + dℓDotI(dq)*ℓy(1)/ℓ - ℓDotN*DℓI(dq)*ℓy(1)/(ℓ*ℓ) + ℓDotN/ℓ*DℓyI(dq,1)
		    dℓzDotI(dq) = -μ12*dχ1zDotI(dq) - μ22*dχ2zDotI(dq) _
		    + dℓDotI(dq)*ℓz(1)/ℓ - ℓDotN*DℓI(dq)*ℓz(1)/(ℓ*ℓ) + ℓDotN/ℓ*DℓzI(dq,1)
		    If dq = Dδ Then
		      dℓxDotI(dq) = dℓxDotI(dq) - μ1*χ1xDotN + μ2*χ2xDotN
		      dℓyDotI(dq) = dℓyDotI(dq) - μ1*χ1yDotN + μ2*χ2yDotN
		      dℓzDotI(dq) = dℓzDotI(dq) - μ1*χ1zDotN + μ2*χ2zDotN
		    End If
		    
		    // Evolve the derivatives of the total angular momentum using a leapfrog step
		    dℓxFI(dq) = DℓxI(dq,0) + TwoΔτ*dℓxDotI(dq)
		    dℓyFI(dq) = DℓyI(dq,0) + TwoΔτ*dℓyDotI(dq)
		    dℓzFI(dq) = DℓzI(dq,0) + TwoΔτ*dℓyDotI(dq)
		  Next
		  
		  // Check to see whether we have crossed the 2nd/3rd quadrant line
		  If ℓyF < 0.0 And ℓy(1) > 0.0 Then
		    If (ℓxF*ℓy(1) - ℓx(1)*ℓyF)/(ℓy(1)-ℓyF) < 0.0 Then NαCycles = 1
		  ElseIf ℓyF > 0.0 And ℓy(1) < 0.0 Then
		    If (ℓxF*ℓy(1) - ℓx(1)*ℓyF)/(ℓy(1)-ℓyF) < 0.0 Then NαCycles = -1
		  Else
		    NαCycles = 0
		  End If
		  αF = αF + NαCycles*6.283185307179586
		  
		  // Calculate derivatives of ι, α, and αDot at the current step
		  Var ℓxy2 As Double = ℓx(1)*ℓx(1) + ℓy(1)*ℓy(1)
		  Var ℓxy As Double = Sqrt(ℓxy2)
		  Var dℓxyIdℓx As Double = ℓx(1)/ℓxy
		  Var dℓxyIdℓy As Double = ℓy(1)/ℓxy
		  Var dℓxyI(7) As Double
		  Var αDotN As Double = (ℓy(1)*ℓxDotN - ℓx(1)*ℓyDotN)/ℓxy2
		  Var dιFI(7) As Double
		  Var dαFI(7) As Double
		  Var dαDotI(7) As Double
		  For dq As Integer = 0 To DLastSpinIndex
		    dℓxyI(dq) = dℓxyIdℓx*DℓxI(dq,1) + dℓxyIdℓy*DℓyI(dq,1)
		    dιFI(dq) = (ℓxy*DℓzI(dq,1) - ℓz(1)*dℓxyI(dq))/(ℓ*ℓ)
		    dαFI(dq) = (ℓy(1)*DℓxI(dq,1) - ℓx(1)*DℓyI(dq,1))/ℓxy2
		    dαDotI(dq) = (DℓyI(dq,1)*ℓxDotN + ℓy(1)*dℓxDotI(dq) - DℓxI(dq,1)*ℓyDotN - ℓx(1)*dℓyDotI(dq))/ℓxy2 _
		    - (ℓy(1)*ℓxDotN - ℓx(1)*ℓyDotN)/(ℓxy2*ℓxy2)*2.0*(ℓx(1)*DℓxI(dq,1) + ℓy(1)*DℓyI(dq,1))
		  Next
		  
		  // Evolve the precession phase
		  Var ΨprF As Double = Ψpr(0) + TwoΔτ*αDotN*Cos(ι(1))
		  
		  // Calculate derivatives of the precession phase
		  Var dΨprFI(7) As Double
		  For dq As Integer = 0 To DLastSpinIndex
		    dΨprFI(dq) = DΨprI(dq,0) - TwoΔτ*(dαDotI(dq)*Cos(ι(1)) - αDotN*Sin(ι(1))*DιI(dq,1))
		  Next
		  
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
		  
		  For dq As Integer = 0 To DLastSpinIndex
		    Dχ1xI(dq,0) = Dχ1xI(dq,1)
		    Dχ1yI(dq,0) = Dχ1yI(dq,1)
		    Dχ1zI(dq,0) = Dχ1zI(dq,1)
		    Dχ1xI(dq,1) = dχ1xFI(dq)
		    Dχ1yI(dq,1) = dχ1yFI(dq)
		    Dχ1zI(dq,1) = dχ1zFI(dq)
		    
		    Dχ2xI(dq,0) = Dχ2xI(dq,1)
		    Dχ2yI(dq,0) = Dχ2yI(dq,1)
		    Dχ2zI(dq,0) = Dχ2zI(dq,1)
		    Dχ2xI(dq,1) = dχ2xFI(dq)
		    Dχ2yI(dq,1) = dχ2yFI(dq)
		    Dχ2zI(dq,1) = dχ2zFI(dq)
		    
		    DℓxI(dq,0)  = DℓxI(dq,1)
		    DℓyI(dq,0)  = DℓyI(dq,1)
		    DℓzI(dq,0)  = DℓzI(dq,1)
		    DℓxI(dq,1)  = dℓxFI(dq)
		    DℓyI(dq,1)  = dℓyFI(dq)
		    DℓzI(dq,1)  = dℓzFI(dq)
		    
		    DαI(dq,0) = DαI(dq,1)
		    DαI(dq,1) = dαFI(dq)
		    
		    DιI(dq,0) = DιI(dq,1)
		    DιI(dq,1) = dιFI(dq)
		    
		    DΨprI(dq,0) = DΨprI(dq,1)
		    DΨprI(dq,1) = dΨprFI(dq)
		  Next
		  
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
		  
		  // Evolve all the derivatives using a similar Euler step
		  // First declare the arrays
		  Var dΩ1I(7,1) As Double
		  Var dΩ2I(7,1) As Double
		  Var dΩ1xI(7,1) As Double
		  Var dΩ1yI(7,1) As Double
		  Var dΩ1zI(7,1) As Double
		  Var dΩ2xI(7,1) As Double
		  Var dΩ2yI(7,1) As Double
		  Var dΩ2zI(7,1) As Double
		  Var dχ1xDotI(7,1) As Double
		  Var dχ1yDotI(7,1) As Double
		  Var dχ1zDotI(7,1) As Double
		  Var dχ2xDotI(7,1) As Double
		  Var dχ2yDotI(7,1) As Double
		  Var dχ2zDotI(7,1) As Double
		  Var dℓDotI(7,1) As Double
		  Var dℓxDotI(7,1) As Double
		  Var dℓyDotI(7,1) As Double
		  Var dℓzDotI(7,1) As Double
		  
		  // Get values of the derivatives of vDot at t = 0
		  Var dvDotI(7) As Double
		  dvDotI(Dδ) = VCalc.DVDotIdδForLastV
		  dvDotI(Dτc) = VCalc.DVDotIdτcForLastV
		  Var dvDotIdχ1ℓ As Double = VCalc.DVDotIdχ1ℓForLastV
		  dvDotI(Dχ1) = dvDotIdχ1ℓ*Cos(θ1)
		  dvDotI(Dθ1) = -dvDotIdχ1ℓ*χ1*Sin(θ1)
		  dvDotI(Dφ1) = 0.0
		  Var dvDotIdχ2ℓ As Double = VCalc.DVDotIdχ2ℓForLastV
		  dvDotI(Dχ2) = dvDotIdχ2ℓ*Cos(θ2)
		  dvDotI(Dθ2) = -dvDotIdχ2ℓ*χ2*Sin(θ2)
		  dvDotI(Dφ2) = 0.0
		  
		  For dq As Integer = 0 To DLastSpinIndex
		    
		    // Calculate derivatives of spin time-derivatives
		    dΩ1I(dq,0) = V4*(5.0*C10 + 7.0*C12*V2 + 9.0*C14*V4)*DvI(dq)/ℓ _
		    + V5*(DC10I(dq) + DC12I(dq)*v2 + DC14I(dq)*V4)/ℓ
		    dΩ1xI(dq,0) = dΩ1I(dq,0)*ℓx(0) + Ω1*(DℓxI(dq,0) - ℓx(0)*DℓI(dq)/ℓ)
		    dΩ1yI(dq,0) = dΩ1I(dq,0)*ℓy(0) + Ω1*(DℓyI(dq,0) - ℓy(0)*DℓI(dq)/ℓ)
		    dΩ1zI(dq,0) = dΩ1I(dq,0)*ℓz(0) + Ω1*(DℓzI(dq,0) - ℓz(0)*DℓI(dq)/ℓ)
		    dΩ2I(dq,0) = V4*(5.0*C20 + 7.0*C22*V2 + 9.0*C24*V4)*DvI(dq)/ℓ _
		    + V5*(DC20I(dq) + DC22I(dq)*V2 + DC24I(dq)*V4)/ℓ
		    dΩ2xI(dq,0) = dΩ2I(dq,0)*ℓx(0) + Ω2*(DℓxI(dq,0) - ℓx(0)*DℓI(dq))/ℓ
		    dΩ2yI(dq,0) = dΩ2I(dq,0)*ℓy(0) + Ω2*(DℓyI(dq,0) - ℓy(0)*DℓI(dq))/ℓ
		    dΩ2zI(dq,0) = dΩ2I(dq,0)*ℓz(0) + Ω2*(DℓzI(dq,0) - ℓz(0)*DℓI(dq))/ℓ
		    dχ1xDotI(dq,0) = dΩ1yI(dq,0)*χ1z(0) - dΩ1zI(dq,0)*χ1y(0) + χ1yDot0*Dχ1zI(dq,0) - χ1zDot0*Dχ1yI(dq,0)
		    dχ1yDotI(dq,0) = dΩ1zI(dq,0)*χ1x(0) - dΩ1xI(dq,0)*χ1z(0) + χ1zDot0*Dχ1xI(dq,0) - χ1xDot0*Dχ1zI(dq,0)
		    dχ1zDotI(dq,0) = dΩ1xI(dq,0)*χ1y(0) - dΩ1yI(dq,0)*χ1x(0) + χ1xDot0*Dχ1yI(dq,0) - χ1yDot0*Dχ1xI(dq,0)
		    dχ2xDotI(dq,0) = dΩ2yI(dq,0)*χ2z(0) - dΩ2zI(dq,0)*χ2y(0) + χ2yDot0*Dχ2zI(dq,0) - χ2zDot0*Dχ2yI(dq,0)
		    dχ2yDotI(dq,0) = dΩ2zI(dq,0)*χ2x(0) - dΩ2xI(dq,0)*χ2z(0) + χ2zDot0*Dχ2xI(dq,0) - χ2xDot0*Dχ2zI(dq,0)
		    dχ2zDotI(dq,0) = dΩ2xI(dq,0)*χ2y(0) - dΩ2yI(dq,0)*χ2x(0) + χ2xDot0*Dχ2yI(dq,0) - χ2yDot0*Dχ2xI(dq,0)
		    
		    // Evolve the derivatives of the spins
		    Dχ1xI(dq,1) = Dχ1xI(dq,0) + ΔτhP*dχ1xDotI(dq,0)
		    Dχ1yI(dq,1) = Dχ1yI(dq,0) + ΔτhP*dχ1yDotI(dq,0)
		    Dχ1zI(dq,1) = Dχ1zI(dq,0) + ΔτhP*dχ1zDotI(dq,0)
		    Dχ2xI(dq,1) = Dχ2xI(dq,0) + ΔτhP*dχ2xDotI(dq,0)
		    Dχ2yI(dq,1) = Dχ2yI(dq,0) + ΔτhP*dχ2yDotI(dq,0)
		    Dχ2zI(dq,1) = Dχ2zI(dq,0) + ΔτhP*dχ2zDotI(dq,0)
		    
		    // Calculate derivatives of angular momenta time derivatives
		    dℓDotI(dq,0) = DL0I(dq)*(-1.0/V2 + L2 + 2.0*L3*V0  + 3.0*L4*v2)*vDot0 _
		    + L0*(2.0/V3 + 2.0*L3 + 6.0*L4*V0)*dvI(dq) + DℓIdv*dvDotI(dq) _
		    + L0*(DL2I(dq) + 2.0*DL3I(dq)*V0 + 3.0*DL4I(dq)*V2)*vDot0
		    dℓxDotI(dq,0) = -μ12*dχ1xDotI(dq,0) - μ22*dχ2xDotI(dq,0) _
		    + dℓDotI(dq,0)*ℓx(0)/ℓ - ℓDot0*DℓI(dq)*ℓx(0)/(ℓ*ℓ) + ℓDot0/ℓ*DℓxI(dq,0)
		    dℓyDotI(dq,0) = -μ12*dχ1yDotI(dq,0) - μ22*dχ2yDotI(dq,0) _
		    + dℓDotI(dq,0)*ℓy(0)/ℓ - ℓDot0*DℓI(dq)*ℓy(0)/(ℓ*ℓ) + ℓDot0/ℓ*DℓyI(dq,0)
		    dℓzDotI(dq,0) = -μ12*dχ1zDotI(dq,0) - μ22*dχ2zDotI(dq,0) _
		    + dℓDotI(dq,0)*ℓz(0)/ℓ - ℓDot0*dℓI(dq)*ℓz(0)/(ℓ*ℓ) + ℓDot0/ℓ*DℓzI(dq,0)
		    If dq = Dδ Then
		      dℓxDotI(dq,0) = dℓxDotI(dq,0) - μ1*χ1xDot0 + μ2*χ2xDot0
		      dℓyDotI(dq,0) = dℓyDotI(dq,0) - μ1*χ1yDot0 + μ2*χ2yDot0
		      dℓzDotI(dq,0) = dℓzDotI(dq,0) - μ1*χ1zDot0 + μ2*χ2zDot0
		    End If
		    // Evolve the derivatives of the spins
		    DℓxI(dq,1) = DℓxI(dq,0) + ΔτhP*dℓxDotI(dq,0)
		    DℓyI(dq,1) = DℓyI(dq,0) + ΔτhP*dℓyDotI(dq,0)
		    DℓzI(dq,1) = DℓzI(dq,0) + ΔτhP*dℓzDotI(dq,0)
		  Next
		  
		  // Calculate derivatives of ι, α, and αDot at t = 0
		  Var ℓxy2 As Double = ℓx(0)*ℓx(0) + ℓy(0)*ℓy(0)
		  Var ℓxy As Double = Sqrt(ℓxy2)
		  Var dℓxyIdℓx As Double = ℓx(0)/ℓxy
		  Var dℓxyIdℓy As Double = ℓy(0)/ℓxy
		  Var dℓxyI(7) As Double
		  Var αDot0 As Double = (ℓy(0)*ℓxDot0 - ℓx(0)*ℓyDot0)/(ℓx(0)*ℓx(0) + ℓy(0)*ℓy(0))
		  Var dαDotI(7,1) As Double
		  For dq As Double = 0 To DLastSpinIndex
		    dℓxyI(dq) = dℓxyIdℓx*DℓxI(dq,0) + dℓxyIdℓy*DℓyI(dq,0)
		    DιI(dq,0) = (ℓxy*DℓzI(dq,0) - ℓz(0)*dℓxyI(dq))/(ℓ*ℓ)
		    DαI(dq,0) = (ℓy(0)*DℓxI(dq,0) - ℓx(0)*DℓyI(dq,0))/ℓxy2
		    dαDotI(dq,0) = (DℓyI(dq,0)*ℓxDot0 + ℓy(0)*dℓxDotI(dq,0) - DℓxI(dq,0)*ℓyDot0 - ℓx(0)*dℓyDotI(dq,0))/ℓxy2 _
		    - (ℓy(0)*ℓxDot0 - ℓx(0)*ℓyDot0)/(ℓxy2*ℓxy2)*2.0*(ℓx(0)*DℓxI(dq,0) + ℓy(0)*DℓyI(dq,0))
		  Next
		  
		  CalculateVAndℓAtTime(ΔτhP)
		  
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
		  
		  // Get values of the derivatives of vDot at the new step
		  dvDotI(Dδ) = VCalc.DVDotIdδForLastV
		  dvDotI(Dτc) = VCalc.DVDotIdτcForLastV
		  dvDotIdχ1ℓ = VCalc.DVDotIdχ1ℓForLastV
		  dvDotI(Dχ1) = dvDotIdχ1ℓ*Cos(θ1)
		  dvDotI(Dθ1) = -dvDotIdχ1ℓ*χ1*Sin(θ1)
		  dvDotI(Dφ1) = 0.0
		  dvDotIdχ2ℓ = VCalc.DVDotIdχ2ℓForLastV
		  dvDotI(Dχ2) = dvDotIdχ2ℓ*Cos(θ2)
		  dvDotI(Dθ2) = -dvDotIdχ2ℓ*χ2*Sin(θ2)
		  dvDotI(Dφ2) = 0.0
		  
		  // Evolve the derivatives of the spin and orbital angular momenta using a more correct step
		  For dq As Integer = 0 To DLastSpinIndex
		    
		    // Calculate derivatives of the spin-dot quantities
		    dΩ1I(dq,1) = V4*(5.0*C10 + 7.0*C12*V2 + 9.0*C14*V4)*DvI(dq)/ℓ _
		    + V5*(DC10I(dq) + DC12I(dq)*V2 + DC14I(dq)*V4)/ℓ
		    dΩ1xI(dq,1) = dΩ1I(dq,1)*ℓx(1) + Ω1*(DℓxI(dq,1) - ℓx(1)*DℓI(dq)/ℓ)
		    dΩ1yI(dq,1) = dΩ1I(dq,1)*ℓy(1) + Ω1*(DℓyI(dq,1) - ℓy(1)*DℓI(dq)/ℓ)
		    dΩ1zI(dq,1) = dΩ1I(dq,1)*ℓz(1) + Ω1*(DℓzI(dq,1) - ℓz(1)*DℓI(dq)/ℓ)
		    dΩ2I(dq,1) = V4*(5.0*C20 + 7.0*C22*V2 + 9.0*C24*V4)*DvI(dq)/ℓ _
		    + V5*(DC20I(dq) + DC22I(dq)*V2 + DC24I(dq)*V4)/ℓ
		    dΩ2xI(dq,1) = dΩ2I(dq,1)*ℓx(1) + Ω2*(DℓxI(dq,1) - ℓx(1)*DℓI(dq)/ℓ)
		    dΩ2yI(dq,1) = dΩ2I(dq,1)*ℓy(1) + Ω2*(DℓyI(dq,1) - ℓy(1)*DℓI(dq)/ℓ)
		    dΩ2zI(dq,1) = dΩ2I(dq,1)*ℓz(1) + Ω2*(DℓzI(dq,1) - ℓz(1)*dℓI(dq)/ℓ)
		    dχ1xDotI(dq,1) = dΩ1yI(dq,1)*χ1z(1) - dΩ1zI(dq,1)*χ1y(1) + χ1yDot1*Dχ1zI(dq,1) - χ1zDot1*Dχ1yI(dq,1)
		    dχ1yDotI(dq,1) = dΩ1zI(dq,1)*χ1x(1) - dΩ1xI(dq,1)*χ1z(1) + χ1zDot1*Dχ1xI(dq,1) - χ1xDot1*Dχ1zI(dq,1)
		    dχ1zDotI(dq,1) = dΩ1xI(dq,1)*χ1y(1) - dΩ1yI(dq,1)*χ1x(1) + χ1xDot1*Dχ1yI(dq,1) - χ1yDot1*Dχ1xI(dq,1)
		    dχ2xDotI(dq,1) = dΩ2yI(dq,1)*χ2z(1) - dΩ2zI(dq,1)*χ2y(1) + χ2yDot1*Dχ2zI(dq,1) - χ2zDot1*Dχ2yI(dq,1)
		    dχ2yDotI(dq,1) = dΩ2zI(dq,1)*χ2x(0) - dΩ2xI(dq,1)*χ2z(1) + χ2zDot1*Dχ2xI(dq,1) - χ2xDot1*Dχ2zI(dq,1)
		    dχ2zDotI(dq,1) = dΩ2xI(dq,1)*χ2y(1) - dΩ2yI(dq,1)*χ2x(1) + χ2xDot1*Dχ2yI(dq,1) - χ2yDot1*Dχ2xI(dq,1)
		    
		    // Evolve the spins using the more accurate step
		    Dχ1xI(dq,1) = Dχ1xI(dq,0) + ΔτhP*0.5*(dχ1xDotI(dq,1) + dχ1xDotI(dq,0))
		    Dχ1yI(dq,1) = Dχ1yI(dq,0) + ΔτhP*0.5*(dχ1yDotI(dq,1) + dχ1yDotI(dq,0))
		    Dχ1zI(dq,1) = Dχ1zI(dq,0) + ΔτhP*0.5*(dχ1zDotI(dq,1) + dχ1zDotI(dq,0))
		    Dχ2xI(dq,1) = Dχ2xI(dq,0) + ΔτhP*0.5*(dχ2xDotI(dq,1) + dχ2xDotI(dq,0))
		    Dχ2yI(dq,1) = Dχ2yI(dq,0) + ΔτhP*0.5*(dχ2yDotI(dq,1) + dχ2yDotI(dq,0))
		    Dχ2zI(dq,1) = Dχ2zI(dq,0) +  ΔτhP*0.5*(dχ2zDotI(dq,1) + dχ2zDotI(dq,0))
		    
		    // Calculate derivatives of angular momenta time derivatives
		    
		    dℓDotI(dq,1) = DL0I(dq)*(-1.0/V2 + L2 + 2.0*L3*VN + 3.0*L4*V2)*vDot1 _
		    + L0*(2.0/V3 + 2.0*L3 + 6.0*L4*VN)*DvI(dq) + DℓIdv*dvDotI(dq) _
		    + L0*(DL2I(dq) + 2.0*DL3I(dq)*VN + 3.0*DL4I(dq)*V2)*vDot1
		    dℓxDotI(dq,1) = -μ12*dχ1xDotI(dq,1) - μ22*dχ2xDotI(dq,1) _
		    + dℓDotI(dq,1)*ℓx(1)/ℓ - ℓDot1*DℓI(dq)*ℓx(1)/(ℓ*ℓ) + ℓDot1/ℓ*DℓxI(dq,1)
		    dℓyDotI(dq,1) = -μ12*dχ1yDotI(dq,1) - μ22*dχ2yDotI(dq,1) _
		    + dℓDotI(dq,1)*ℓy(1)/ℓ - ℓDot1*DℓI(dq)*ℓy(1)/(ℓ*ℓ) + ℓDot1/ℓ*DℓyI(dq,1)
		    dℓzDotI(dq,1) = -μ12*dχ1zDotI(dq,1) - μ22*dχ2zDotI(dq,1) _
		    + dℓDotI(dq,1)*ℓz(1)/ℓ - ℓDot1*DℓI(dq)*ℓz(1)/(ℓ*ℓ) + ℓDot1/ℓ*DℓzI(dq,1)
		    
		    If dq = Dδ Then
		      dℓxDotI(dq,1) = dℓxDotI(dq,1) - μ1*χ1xDot1 + μ2*χ2xDot1
		      dℓyDotI(dq,1) = dℓyDotI(dq,1) - μ1*χ1yDot1 + μ2*χ2yDot1
		      dℓzDotI(dq,1) = dℓzDotI(dq,1) - μ1*χ1zDot1 + μ2*χ2zDot1
		    End If
		    
		    // Evolve the derivatives of the spins using the more accurate step
		    DℓxI(dq,1) = DℓxI(dq,0) + 0.5*ΔτhP*(dℓxDotI(dq,0) + dℓxDotI(dq,1))
		    DℓyI(dq,1) = DℓyI(dq,0) + 0.5*ΔτhP*(dℓyDotI(dq,0) + dℓyDotI(dq,1))
		    DℓzI(dq,1) = DℓzI(dq,0) + 0.5*ΔτhP*(dℓzDotI(dq,0) + dℓzDotI(dq,1))
		  Next
		  
		  // Check to see whether we have crossed the 2nd/3rd quadrant line
		  If ℓy(1) < 0.0 And ℓy(0) > 0.0 Then
		    If (ℓx(1)*ℓy(0) - ℓx(0)*ℓy(1))/(ℓy(0)-ℓy(1)) < 0.0 Then NαCycles = 1
		  ElseIf ℓy(1) > 0.0 And ℓy(0) < 0.0 Then
		    If (ℓx(1)*ℓy(0) - ℓx(0)*ℓy(1))/(ℓy(0)-ℓy(1)) < 0.0 Then NαCycles = -1
		  Else
		    NαCycles = 0
		  End If
		  α(1) = α(1) + NαCycles*6.283185307179586
		  
		  // Calculate derivatives of ι, α, and αDot at step 1
		  ℓxy2 = ℓx(1)*ℓx(1) + ℓy(1)*ℓy(1)
		  ℓxy = Sqrt(ℓxy2)
		  dℓxyIdℓx = ℓx(1)/ℓxy
		  dℓxyIdℓy = ℓy(1)/ℓxy
		  Var αDot1 As Double = (ℓy(1)*ℓxDot1 - ℓx(1)*ℓyDot1)/ℓxy2
		  For dq As Integer = 0 To DLastSpinIndex
		    dℓxyI(dq) = dℓxyIdℓx*DℓxI(dq,1) + dℓxyIdℓy*DℓyI(dq,1)
		    DιI(dq,1) = (ℓxy*DℓzI(dq,1) - ℓz(1)*dℓxyI(dq))/(ℓ*ℓ)
		    DαI(dq,1) = (ℓy(1)*DℓxI(dq,1) - ℓx(1)*DℓyI(dq,1))/ℓxy2
		    dαDotI(dq,1) = (DℓyI(dq,1)*ℓxDot1 + ℓy(1)*dℓxDotI(dq,1) - DℓxI(dq,1)*ℓyDot1 - ℓx(1)*dℓyDotI(dq,1))/ℓxy2 _
		    - (ℓy(1)*ℓxDot1 - ℓx(1)*ℓyDot1)/(ℓxy2*ℓxy2)*2.0*(ℓx(1)*DℓxI(dq,1) + ℓy(1)*DℓyI(dq,1))
		  Next
		  
		  // Initialize the precession phase
		  Ψpr(0) = 0.0
		  Ψpr(1) = -0.5*ΔτhP*(αDot0*Cos(ι(0)) + αDot1*Cos(ι(1)))
		  
		  // Calculate derivatives of the precession phase
		  For dq As Integer = 0 To DLastSpinIndex
		    DΨprI(dq,0) = 0.0
		    DΨprI(dq,1) = -0.5*ΔτhP*(dαDotI(dq,0)*Cos(ι(0)) - αDot0*Sin(ι(0))*DιI(dq,0) _
		    + dαDotI(dq,1)*Cos(ι(1)) - αDot1*Sin(ι(1))*DιI(dq,1))
		  Next
		  
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
		Private DC10I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC12I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC14I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC20I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC22I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DC24I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DL0I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DL2I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DL3I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DL4I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DSℓI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DvI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dμ12I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dμ1I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dμ22I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dμ2I(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DιI(7,1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DαI(7,1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DΣℓI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dχ1xI(7,1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dχ1yI(7,1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dχ1zI(7,1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dχ1ℓI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dχ2xI(7,1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dχ2yI(7,1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dχ2zI(7,1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Dχ2ℓI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DΨmpI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DΨprI(7,1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DℓI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DℓIdv As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DℓxI(7,1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DℓyI(7,1) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private DℓzI(7,1) As Double
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
		Private NαCycles As Integer
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
		Private Ψmp As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Ψpr(1) As Double
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
