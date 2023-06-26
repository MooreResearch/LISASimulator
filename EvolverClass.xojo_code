#tag Class
Protected Class EvolverClass
	#tag Method, Flags = &h0
		Function CalcDeriv(n1 As Double, n2 As Double, den As Double) As Double
		  '// This function gives us the numerical calculation of a derivative term. If the derivative calculation is normal, it returns the
		  '// derivative. If there are floating point errors because the values are too close, it returns 0. If we are close to having zero over 
		  '// zero, we raise an error (right now we just break, but we will need to raise an error in the future).
		  '
		  'Var diff As Double = n1 - n2
		  'Var sum As Double = n1 + n2
		  'Var epsilon As Double = 1e-12
		  '
		  'If Abs(diff) > Abs(sum)*epsilon Then
		  'Return diff/den  // if the derivative behaves normally, return what we calculate
		  'End If
		  '
		  '// If we get here, we have floating point problems
		  '
		  'epsilon = 1e-2
		  'If Abs(diff) < Abs(den)*epsilon Then
		  'Return 0  // if the difference is very small, set it to zero and return
		  'End If
		  '
		  '// If we get here, then our derivative may be close to being zero over zero
		  '
		  '// This is what we want: Raise err as DerivUndefinedError // need to define this RuntimeError subclass
		  'break // for now, just break to let us know it happened
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(myM As Double, myδ As Double, myf0 As Double, myR As Double, myβ As Double, myψangle As Double, myλ0 As Double, myΘ As Double, myΦ As Double, χ10 As Vector, χ20 As Vector, mydτ0 As Double)
		  '// The EvolverClass class is meant to perform all steps to evolve the gravitational wave from the source to the detector, where it then
		  '// computes the received waveform derivatives and loads them into the ATA matrix. After initialization, the majority of this work
		  '// happens in the DoStep method, which performs one step in time. The parameters passed in are provided by the user (see the 
		  '// user interface for explanations of what they are).
		  '
		  '//Define necessary variables
		  'M = myM
		  'δ = myδ
		  'f0 = myf0
		  'R = myR
		  'λ0 = myλ0
		  'β = myβ 
		  'ψangle = myψangle
		  'Θ = myΘ
		  'Φ = myΦ
		  'dτ0 = mydτ0               //The initial timestep--it will change with adaptive time stepping
		  ''z = myz
		  'maindτ0 = dτ0
		  '
		  'π = 3.141592653589793238
		  'η = (1-δ^2)/4
		  'v0 = (myf0* 1e-3 * M * 2 * π) ^ (1/3)
		  'Ln0 = (M^2*η)/v0
		  'Δχ = 1e-6                  //This value is used to calculate derivatives with respect to the components of χ1 and χ2
		  'Δv = 1e-6                  //This value is used to calculate dv/dv0
		  'Δδ = 1e-6                  //This value is used to calculate derivatives with respect to v and v0
		  'Vo = 9.936e-5         //Assumed constant speed of the LISA detector--DIFFERENT from v0
		  'Ω = 1.99213231e-7 //Angular frequency of Earth's orbit in units of radians per second
		  '
		  '// Masses of the two stars
		  'Var m1 As Double = (M/2)*(1+δ)
		  'Var m2 As Double = (M/2)*(1-δ)
		  '
		  '//Store spin vectors as properties
		  'χ1 = χ10
		  'χ2 = χ20
		  '
		  'Var χ1x As Double = χ1.getX
		  'Var χ2x As Double = χ2.getX
		  'Var χ1y As Double = χ1.getY
		  'Var χ2y As Double = χ2.getY
		  'Var χ1z As Double = χ1.getZ
		  'Var χ2z As Double = χ2.getZ
		  '
		  '//Get chi vector components
		  'Var x1 As Double = χ1.GetX*m1^2
		  'Var x2 As Double = χ2.GetX*m2^2
		  'Var y1 As Double = χ1.GetY*m1^2
		  'Var y2 As Double = χ2.GetY*m2^2
		  'Var z1 As Double = χ1.GetZ*m1^2
		  'Var z2 As Double = χ2.GetZ*m2^2
		  '
		  'If x1 = 0 And y1 = 0 and z1 = 0 Then
		  'noSpin1 = True
		  'End If
		  '
		  'If x2 = 0 And y2 = 0 and z2 = 0 Then
		  'noSpin2 = True
		  'End If
		  '
		  'If Not (noSpin1 and noSpin2) Then
		  '// Initialize α and ι
		  'α0 = ATan2( (-y1 - y2), (-x1 - x2) )
		  '
		  'Var ιb0 As Double = -ASin((x1+x2)/(Ln0*Cos(α0)))
		  '
		  '//Checks to see if we're in "impossible" parameter space
		  'If Abs(z1 + z2) > Ln0*Cos(ιb0) And z1 + z2 < 0 Then
		  '// If yes, we need to invert y and z coordinates (rotate coord. system about x-axis) to get back to "possible" parameter space
		  'ι0 = π - ιb0
		  'if Abs(ι0) < 10^-8 then     //Checking to see if ι0 is zero or close enough, then setting α0 to zero
		  'ι0 = 0
		  'α0 = 0
		  'else
		  'α0 = ATan2( (y1 + y2), (-x1 - x2) )     //Normal calculation for α0 for nonzero ι0
		  'end if
		  'χ1 = New Vector(x1, -y1, -z1)     //Defining χ1 and χ2 vectors based on components
		  'χ2 = New Vector(x2, -y2, -z2)
		  'wasInverted = True  //need to store whether inversion happened or not b/c derivatives of ι0 will have a sign difference in each case
		  '//Reset variables to inverted quantities
		  'x1 = χ1.GetX
		  'x2 = χ2.GetX
		  'y1 = χ1.GetY
		  'y2 = χ2.GetY
		  'z1 = χ1.GetZ
		  'z2 = χ2.GetZ
		  'Else
		  '//if not, we can proceed as usual
		  'ι0 = ιb0
		  'if Abs(ι0) < 10^-8 then     //Same checking as above
		  'ι0 = 0
		  'α0 = 0
		  'else
		  'α0 = ATan2( (-y1 - y2), (-x1 - x2) )
		  'end if
		  'wasInverted = False
		  'End If 
		  '
		  '//Finding the magnitudes of χ1ℓ and χ2ℓ using the components of χ1 and χ2, ι, and α
		  'χ1ℓ = χ1x*Cos(α0)*Sin(ι0) + χ1y*Sin(α0)*Sin(ι0) + χ1z*Cos(ι0)
		  'χ2ℓ = χ2x*Cos(α0)*Sin(ι0) + χ2y*Sin(α0)*Sin(ι0) + χ2z*Cos(ι0)
		  '
		  '//Define partial derivatives of χ1ℓ and χ2ℓ with respect to chi components (x1, y1, z1, x2, y2, z2)
		  '//Some values of derivatives will depend on whether we inverted the coordinates 
		  'dχ1ℓdχ20z = 0
		  'dχ2ℓdχ10z = 0
		  '
		  'If wasInverted Then
		  'dχ1ℓdχ10x = -(2*x1 + x2)/Ln0 + z1*(x1 + x2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ1ℓdχ10y = -(2*y1 + y2)/Ln0 + z1*(y1 + y2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ1ℓdχ10z = -Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ1ℓdχ20x = -x1/Ln0 + z1*(x1 + x2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ1ℓdχ20y = -y1/Ln0 + z1*(y1 + y2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ2ℓdχ10x = -x2/Ln0 + z2*(x1 + x2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ2ℓdχ10y = -y2/Ln0 + z2*(y1 + y2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ2ℓdχ20x = -(2*x2 + x1)/Ln0 + z2*(x1 + x2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ2ℓdχ20y = -(2*y2 + y1)/Ln0 + z2*(y1 + y2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ2ℓdχ20z = -Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'Else
		  'dχ1ℓdχ10x = -(2*x1 + x2)/Ln0 - z1*(x1 + x2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ1ℓdχ10y = -(2*y1 + y2)/Ln0 - z1*(y1 + y2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ1ℓdχ10z = Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ1ℓdχ20x = -x1/Ln0 - z1*(x1 + x2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ1ℓdχ20y = -y1/Ln0 - z1*(y1 + y2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ2ℓdχ10x = -x2/Ln0 - z2*(x1 + x2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ2ℓdχ10y = -y2/Ln0 - z2*(y1 + y2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ2ℓdχ20x = -(2*x2 + x1)/Ln0 - z2*(x1 + x2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ2ℓdχ20y = -(2*y2 + y1)/Ln0 - z2*(y1 + y2)/Ln0^2*1/Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ2ℓdχ20z = Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'End If
		  '
		  '
		  '//Define partial derivatives of χs and χa with respect to chi components (x1, y1, z1, x2, y2, z2)
		  'dχsdχ10x = dχsdχ1ℓ*dχ1ℓdχ10x + dχsdχ2ℓ*dχ2ℓdχ10x
		  'dχsdχ10y = dχsdχ1ℓ*dχ1ℓdχ10y + dχsdχ2ℓ*dχ2ℓdχ10y
		  'dχsdχ10z = dχsdχ1ℓ*dχ1ℓdχ10z + dχsdχ2ℓ*dχ2ℓdχ10z
		  'dχsdχ20x = dχsdχ1ℓ*dχ1ℓdχ20x + dχsdχ2ℓ*dχ2ℓdχ20x
		  'dχsdχ20y = dχsdχ1ℓ*dχ1ℓdχ20y + dχsdχ2ℓ*dχ2ℓdχ20y
		  'dχsdχ20z = dχsdχ1ℓ*dχ1ℓdχ20z + dχsdχ2ℓ*dχ2ℓdχ20z
		  '
		  'dχadχ10x = dχadχ1ℓ*dχ1ℓdχ10x + dχadχ2ℓ*dχ2ℓdχ10x
		  'dχadχ10y = dχadχ1ℓ*dχ1ℓdχ10y + dχadχ2ℓ*dχ2ℓdχ10y
		  'dχadχ10z = dχadχ1ℓ*dχ1ℓdχ10z + dχadχ2ℓ*dχ2ℓdχ10z
		  'dχadχ20x = dχadχ1ℓ*dχ1ℓdχ20x + dχadχ2ℓ*dχ2ℓdχ20x
		  'dχadχ20y = dχadχ1ℓ*dχ1ℓdχ20y + dχadχ2ℓ*dχ2ℓdχ20y
		  'dχadχ20z = dχadχ1ℓ*dχ1ℓdχ20z + dχadχ2ℓ*dχ2ℓdχ20z
		  'Else
		  'ι0 = 0
		  'α0 = 0
		  'dχ1ℓdχ10z = Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2))
		  'dχ2ℓdχ20z = Sqrt(1 - ((x1+x2)^2+(y1+y2)^2)/(Ln0^2)) 
		  'End If
		  '
		  'mainEquation = New VEquationSolver(δ, χ1ℓ, χ2ℓ)         //Set up the main v equation solver, which will provide the values of our v-related evolving parameters in the DoStep method
		  '
		  '//Perform "pre-initialization," only instantiating quantities used for calculating ideal initial timestep.
		  'mainSpinEvolver = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, χ1x, χ1y, χ1z, χ2x, χ2y, χ2z)
		  'vN = v0
		  '
		  'SetIdealdτv(vN)
		  'Var dτIdeal As Double = FindIdealdτ(True)     //Getting reported value from FindIdealdτ
		  'Var p As Integer 
		  'If dτIdeal >= mydτ0 Then
		  'p = (Log(dτIdeal/mydτ0)/Log(2))\1  //finds the power of 2 that's least smaller than dτIdeal/mydτ0
		  'Else 
		  'p = (Log(dτIdeal/mydτ0)/Log(2))\1 - 1 //off-by-one error introduced 
		  'End If
		  'dτ0 = (2^p)*mydτ0  //set actual initial timestep
		  '
		  'τSpin = 0    // set "spin clock" to zero
		  'τSpin = τSpin + dτ0
		  'dτP = dτ0  // set dτP at initial time step value 
		  '
		  '//Now, do full initializations to set the first two values for each evolution array
		  'InitV
		  'InitDvdδ
		  'InitDvdχ1ℓ
		  'InitDvdχ2ℓ
		  'InitDvdv0
		  'InitSpinVariables
		  'InitψAndDerivs
		  '
		  'FullTurns = mainSpinEvolver.FullTurns
		  
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoStep(dτ As Double)
		  '// The stepper takes a past and present value of various parameters and, using the centered difference equation,
		  '// calculates the future value of these parameters. It uses an adaptive time stepping technique that allows us to initially set
		  '// the past value before stepping forward to the future. We do this calculation and set the previous "future" values to the present
		  '// before performing a step. It also calls the SpinDoStep method of the thirteen instances of SpinEvolver, thereby evolving the spin 
		  '// variables (and the derivatives of α and ι, which are calculated based on the side instances) for the next step. Finally, it performs 
		  '// a step in the wave phase and its derivatives, and then calls hDrvtvs and DetectorFunctions to finish out the step.
		  '
		  '// Calculate new past values using interpolation
		  'vP = (1 - dτ/dτP)*vF + (dτ/dτP)*vN   
		  '
		  'dvdχ1ℓP = (1 - dτ/dτP)*dvdχ1ℓF + (dτ/dτP)*dvdχ1ℓN
		  'dvdχ2ℓP = (1 - dτ/dτP)*dvdχ2ℓF + (dτ/dτP)*dvdχ2ℓN
		  'dvdδP = (1 - dτ/dτP)*dvdδF + (dτ/dτP)*dvdδN
		  'vBigP = (1 - dτ/dτP)*vBigF + (dτ/dτP)*vBigN
		  'vSmallP = (1 - dτ/dτP)*vSmallF + (dτ/dτP)*vSmallN
		  'ψRP = (1 - dτ/dτP)*ψRF + (dτ/dτP)*ψRN
		  'dψRdδP = (1 - dτ/dτP)*dψRdδF + (dτ/dτP)*dψRdδN
		  'dψRdv0P = (1 - dτ/dτP)*dψRdv0F + (dτ/dτP)*dψRdv0N
		  'dψRdχ1ℓP = (1 - dτ/dτP)*dψRdχ1ℓF + (dτ/dτP)*dψRdχ1ℓN
		  'dψRdχ2ℓP = (1 - dτ/dτP)*dψRdχ2ℓF + (dτ/dτP)*dψRdχ2ℓN
		  'dψRdΘP = (1 - dτ/dτP)*dψRdΘF + (dτ/dτP)*dψRdΘN
		  'dψRdΦP = (1 - dτ/dτP)*dψRdΦF + (dτ/dτP)*dψRdΦN
		  'dψRdzP = (1 - dτ/dτP)*dψRdzF + (dτ/dτP)*dψRdzN
		  '
		  '// Reassign "future" values to present values
		  'vN = vF
		  '
		  'dvdδN = dvdδF
		  'dvdχ1ℓN = dvdχ1ℓF
		  'dvdχ2ℓN = dvdχ2ℓF
		  'vBigN = vBigF
		  'vSmallN = vSmallF
		  'ψRN = ψRF
		  'dψRdδN = dψRdδF
		  'dψRdv0N = dψRdv0F
		  'dψRdχ1ℓN = dψRdχ1ℓF
		  'dψRdχ2ℓN = dψRdχ2ℓF
		  'dψRdΘN = dψRdΘF
		  'dψRdΦN = dψRdΦF
		  'dψRdzN = dψRdzF   
		  'dvdv0N = dvdv0F    
		  '
		  'dιdχ10xN = dιdχ10xF
		  'dιdχ10yN = dιdχ10yF
		  'dιdχ10zN = dιdχ10zF
		  'dιdχ20xN = dιdχ20xF
		  'dιdχ20yN = dιdχ20yF
		  'dιdχ20zN = dιdχ20zF
		  'dαdχ10xN = dαdχ10xF
		  'dαdχ10yN = dαdχ10yF
		  'dαdχ10zN = dαdχ10zF
		  'dαdχ20xN = dαdχ20xF
		  'dαdχ20yN = dαdχ20yF
		  'dαdχ20zN = dαdχ20zF
		  '
		  'If Not (noSpin1 and noSpin2) Then 
		  'ιN = ιF
		  'αAccN = αAccF
		  'oldψangleN = oldψangleF
		  'End If
		  '
		  '// Now, do step
		  'vF = vP + 2*dτ*mainEquation.Getvdot(vN)  // Finding the future value of v using the centered difference equation
		  '
		  'dvdδF = dvdδP + 2*dτ*mainEquation.GetDvdδdot(vN, dvdδN)   // Finding the future value of dvdδ using the centered difference equation
		  '
		  'dvdχ1ℓF = dvdχ1ℓP + 2*dτ*mainEquation.GetDvdχ1ℓdot(vN, dvdχ1ℓN)    // Finding the future value of dvdχ1ℓ using the centered difference equation
		  '
		  'dvdχ2ℓF = dvdχ2ℓP + 2*dτ*mainEquation.GetDvdχ2ℓdot(vN, dvdχ2ℓN)    // Finding the future value of dvdχ2ℓ using the centered difference equation
		  '
		  'vBigF = vBigP + 2*dτ*mainEquation.GetVdot(vBigN)              //Finding the future value of vBig using the centered difference equation
		  'vSmallF = vSmallP + 2*dτ*mainEquation.GetVdot(vSmallN)   //Finding the future value of vSmall using the centered difference equation
		  'dvdv0F = (vBigF - vSmallF)/(2*Δv)                                           //Computing the future value of dv/dv0 using the centered difference equation
		  '
		  '//Then, handle the spin variable stepping
		  'If Not (noSpin1 and noSpin2) Then 
		  'mainSpinEvolver.SpinDoStep(dτP, dτ, vN)         //Evolve the main spin EvolverClass
		  'ιF = mainSpinEvolver.ιF                       //Set the proper ι
		  'αF = mainSpinEvolver.αF                    //Set the proper α
		  'αAcc = mainSpinEvolver.αAccF
		  'αAccF = αAcc
		  'χ1hatF = mainSpinEvolver.χ1hatF       //Set the proper χ1hatN
		  'χ2hatF = mainSpinEvolver.χ2hatF      //Set the proper χ2hatN
		  'χ1magF = mainSpinEvolver.χ1mag
		  'χ2magF = mainSpinEvolver.χ2mag
		  'LNhatF = mainSpinEvolver.LNhatF
		  'oldψangleF = mainSpinEvolver.oldψangle
		  'End If
		  '
		  '// Set up the side spin EvolverClasss
		  'sideSpinEvolver1.SpinDoStep(dτP, dτ, vN)                                                          //Evolve the first side EvolverClass that adds a little to χ1x
		  'sideSpinEvolver2.SpinDoStep(dτP, dτ, vN)                                                         //Evolve the second side EvolverClass that subtracts a little from χ1x
		  'dιdχ10xF = CalcDeriv(sideSpinEvolver1.ιF, sideSpinEvolver2.ιF, 2*Δχ)        //Compute the derivative of ι using the centered difference equation
		  'dαdχ10xF = CalcDeriv(sideSpinEvolver1.αAccF, sideSpinEvolver2.αAccF, 2*Δχ)     //Compute the derivative of α using the centered difference equation
		  'dαdotdχ10xN = CalcDeriv(sideSpinEvolver1.αdotN, sideSpinEvolver2.αdotN, 2*Δχ)
		  'αAcc1 = sideSpinEvolver1.αAccF
		  'αAcc2 = sideSpinEvolver2.αAccF
		  '
		  '//Repeat the steps above for the other five parameters
		  'sideSpinEvolver3.SpinDoStep(dτP, dτ, vN)
		  'sideSpinEvolver4.SpinDoStep(dτP, dτ, vN)
		  'dιdχ10yF = CalcDeriv(sideSpinEvolver3.ιF, sideSpinEvolver4.ιF, 2*Δχ)
		  'dαdχ10yF = CalcDeriv(sideSpinEvolver3.αAccF, sideSpinEvolver4.αAccF, 2*Δχ)
		  'dαdotdχ10yN = CalcDeriv(sideSpinEvolver3.αdotN, sideSpinEvolver4.αdotN, 2*Δχ)
		  '
		  'sideSpinEvolver5.SpinDoStep(dτP, dτ, vN)
		  'sideSpinEvolver6.SpinDoStep(dτP, dτ, vN)
		  'dιdχ10zF = CalcDeriv(sideSpinEvolver5.ιF, sideSpinEvolver6.ιF, 2*Δχ)
		  'dαdχ10zF = CalcDeriv(sideSpinEvolver5.αAccF, sideSpinEvolver6.αAccF, 2*Δχ)
		  'dαdotdχ10zN = CalcDeriv(sideSpinEvolver5.αdotN, sideSpinEvolver6.αdotN, 2*Δχ)
		  '
		  'sideSpinEvolver7.SpinDoStep(dτP, dτ, vN)
		  'sideSpinEvolver8.SpinDoStep(dτP, dτ, vN)
		  'dιdχ20xF = CalcDeriv(sideSpinEvolver7.ιF, sideSpinEvolver8.ιF, 2*Δχ)
		  'dαdχ20xF = CalcDeriv(sideSpinEvolver7.αAccF, sideSpinEvolver8.αAccF, 2*Δχ)
		  'dαdotdχ20xN = CalcDeriv(sideSpinEvolver7.αdotN, sideSpinEvolver8.αdotN, 2*Δχ)
		  '
		  'sideSpinEvolver9.SpinDoStep(dτP, dτ, vN)
		  'sideSpinEvolver10.SpinDoStep(dτP, dτ, vN)
		  'dιdχ20yF = CalcDeriv(sideSpinEvolver9.ιF, sideSpinEvolver10.ιF, 2*Δχ)
		  'dαdχ20yF = CalcDeriv(sideSpinEvolver9.αAccF, sideSpinEvolver10.αAccF, 2*Δχ)
		  'dαdotdχ20yN = CalcDeriv(sideSpinEvolver9.αdotN, sideSpinEvolver10.αdotN, 2*Δχ)
		  '
		  'sideSpinEvolver11.SpinDoStep(dτP, dτ, vN)
		  'sideSpinEvolver12.SpinDoStep(dτP, dτ, vN)
		  'dιdχ20zF = CalcDeriv(sideSpinEvolver11.ιF, sideSpinEvolver12.ιF, 2*Δχ)
		  'dαdχ20zF = CalcDeriv(sideSpinEvolver11.αAccF, sideSpinEvolver12.αAccF, 2*Δχ)
		  'dαdotdχ20zN = CalcDeriv(sideSpinEvolver11.αdotN, sideSpinEvolver12.αdotN, 2*Δχ)
		  '
		  '//And repeat the steps above for the other necessary derivatives
		  'dιdδN = CalcDeriv(sideSpinEvolver13.ιF, sideSpinEvolver14.ιF, 2*Δδ)      //Calculate the "now" derivative of iota from the last step's "future" values of iota
		  'dαdδN = CalcDeriv(sideSpinEvolver13.αAccF, sideSpinEvolver14.αAccF, 2*Δδ)  //Calculate the "now" derivative of alpha from the last step's "future" values of alpha
		  'sideSpinEvolver13.SpinDoStep(dτP, dτ, vN)
		  'sideSpinEvolver14.SpinDoStep(dτP, dτ, vN)
		  'dαdotdδN = CalcDeriv(sideSpinEvolver13.αdotN, sideSpinEvolver14.αdotN, 2*Δδ)
		  'dιdδF = CalcDeriv(sideSpinEvolver13.ιF, sideSpinEvolver14.ιF, 2*Δδ)
		  'dαdδF = CalcDeriv(sideSpinEvolver13.αAccF, sideSpinEvolver14.αAccF, 2*Δδ)
		  '
		  'dιdv0N = CalcDeriv(sideSpinEvolver15.ιF, sideSpinEvolver16.ιF, 2*Δv)
		  'dαdv0N = CalcDeriv(sideSpinEvolver15.αAccF, sideSpinEvolver16.αAccF, 2*Δv)  //Calculate the "now" derivative of alpha from the last step's "future" values of alpha
		  'sideSpinEvolver15.SpinDoStep(dτP, dτ, vN)
		  'sideSpinEvolver16.SpinDoStep(dτP, dτ, vN)
		  'dαdotdv0N = CalcDeriv(sideSpinEvolver15.αdotN, sideSpinEvolver16.αdotN, 2*Δv)
		  'dιdv0F = CalcDeriv(sideSpinEvolver15.ιF, sideSpinEvolver16.ιF, 2*Δv)
		  'dαdv0F = CalcDeriv(sideSpinEvolver15.αAccF, sideSpinEvolver16.αAccF, 2*Δv)
		  '
		  '//Now, calculate a step in the wave phase and the derivatives
		  'Var αdotN As Double
		  'If Not (noSpin1 and noSpin2) Then 
		  'αdotN = mainSpinEvolver.αdotN         //Pull the value of αdot from the main spin EvolverClass. NOTE: this value is calculated in the "present" and not the "future"
		  'End 
		  'Var vdotN As Double = mainEquation.GetVdot(vN)     //Calculating vdot at vN
		  'Var dψdτN As Double = vN - Cos(ιN)*αdotN - 2*vN^2*(Log(vN/v0) + 1)*vdotN     //Compute the time derivative of the phase
		  'Var tRN As Double = M*(z + 1)*τSpin            //Compute tRN, the current retarded time, based on the running total of the tau time.
		  'ψRF = ψRP + 2*dτ*(1 + Vo*Sin(Θ)*Sin(Ω*tRN + Φ))*dψdτN       //Finally, perform the step using the adapted centered difference equation that takes into account retarded time
		  '
		  'Var dψdτ_dδN As Double = dvdδN + Sin(ιN)*dιdδN*αdotN - Cos(ιN)*dαdotdδN _
		  '- 4*vN*dvdδN*(Log(vN/v0) + 2)*vdotN - 2*vN^2*(Log(vN/v0) + 1)*mainEquation.GetDvdδdot(vN, dvdδN)     //Calculate the current delta derivative of ψdot
		  'dψRdδF = dψRdδP + 2*dτ*(1 + Vo*Sin(Θ)*Sin(Ω*tRN + Φ))*dψdτ_dδN      //And perform the step in the delta derivative of the phase
		  '
		  'Var dvdtBig As Double = mainEquation.GetVdot(vBigN)     //Calculating vdot at v + Δv
		  'Var dvdtSmall As Double = mainEquation.GetVdot(vSmallN)     //Calculating vdot at v - Δv
		  'Var dvdv0dotN As Double = (dvdtBig - dvdtSmall)/(2*Δv)      //Finding dvdv0dotN using a centered difference approximation
		  'Var dψdτ_dv0N As Double = dvdv0N + Sin(ιN)*dιdv0N*αdotN - Cos(ιN)*dαdotdv0N _
		  '- 4*vN*dvdv0N*(Log(vN/v0) + 2)*vdotN - 2*vN^2*(Log(vN/v0) + 1)*dvdv0dotN     //Calculate the current v0 derivative of ψdot
		  'dψRdv0F = dψRdv0P + 2*dτ*(1 + Vo*Sin(Θ)*Sin(Ω*tRN + Φ))*dψdτ_dv0N     //And perform the step in the v0 derivative of the phase
		  '
		  '//check if could be zero
		  'Var dαdotdχ1ℓN As Double
		  'dιdχ1ℓN = Getdιdχℓ(1, "N")     //Finding the current χ1ℓ derivative of ι using the chain rule
		  'dαdotdχ1ℓN =  Getdαdotdχℓ(1, "N")     //Finiding the current χ1ℓ derivative of αdot using the chain rule
		  'Var dψdτ_dχ1ℓN As Double = dvdχ1ℓN + Sin(ιN)*dιdχ1ℓN*αdotN - Cos(ιN)*dαdotdχ1ℓN _
		  '- 4*vN*dvdχ1ℓN*(Log(vN/v0) + 2)*vdotN - 2*vN^2*(Log(vN/v0) + 1)*mainEquation.GetDvdχ1ℓdot(vN, dvdχ1ℓN)     //Calculate the current χ1ℓ derivative of ψdot
		  'dψRdχ1ℓF = dψRdχ1ℓP + 2*dτ*(1 + Vo*Sin(Θ)*Sin(Ω*tRN + Φ))*dψdτ_dχ1ℓN     //And perform the step in the χ1ℓ derivative of the phase
		  '
		  '//check if could be zero
		  'Var dαdotdχ2ℓN As Double 
		  'dιdχ2ℓN = Getdιdχℓ(2, "N")    //Finding the current χ2ℓ derivative of ι using the chain rule
		  'dαdotdχ2ℓN =  Getdαdotdχℓ(2, "N")    //Finding the current χ2ℓ derivative of αdot using the chain rule
		  'Var dψdτ_dχ2ℓN As Double = dvdχ2ℓN + Sin(ιN)*dιdχ2ℓN*αdotN - Cos(ιN)*dαdotdχ2ℓN _
		  '- 4*vN*dvdχ2ℓN*(Log(vN/v0) + 2)*vdotN - 2*vN^2*(Log(vN/v0) + 1)*mainEquation.GetDvdχ2ℓdot(vN, dvdχ2ℓN)     //Calculate the current χ2ℓ derivative of ψdot
		  'dψRdχ2ℓF = dψRdχ2ℓP + 2*dτ*(1 + Vo*Sin(Θ)*Sin(Ω*tRN + Φ))*dψdτ_dχ2ℓN     //And perform the step in the χ2ℓ derivative of the phase
		  '
		  'dψRdΘF = dψRdΘP + 2*dτ*(Vo*Cos(Θ)*Sin(Ω*tRN + Φ))*dψdτN     //Perform the step in the Θ derivative of the phase
		  '
		  'dψRdΦF = dψRdΦP + 2*dτ*(Vo*Sin(Θ)*Cos(Ω*tRN + Φ))*dψdτN     //Perform the step in the Φ derivative of the phase
		  '
		  'dψRdzF = dψRdzP - 2*dτ*(z/(1 + z))*(1 + Vo*Sin(Θ)*Sin(Ω*tRN + Φ))*dψdτN     //Perform the step in the z derivative of the phase
		  '
		  '//Update value of EvolverClass's "clock"
		  'τSpin = τSpin + dτ
		  '
		  '// Calculate ideal time-step based on rate at which v is changing
		  'SetIdealdτv(vF)
		  '
		  'FullTurns = mainSpinEvolver.FullTurns
		  '
		  '
		  '//Store current time step dτ as past time step dτP
		  'dτP = dτ
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function FindIdealdτ(init As Boolean = False) As Double
		  '// This method exists to find the ideal time step dτ that we want to use to calculate our actual time step, dτReal.
		  '// It does so by finding the smallest recommended time step in the SpinEvolver class, and then reporting that value.
		  '// Then, the EvolverClass constructor calls this method to get this value.
		  '
		  '//Calculate ideal time step dτ based on rates of change of v, LN, χ1, and χ2
		  'Var dτLN As Double = mainSpinEvolver.dτLN
		  'Var dτχ1 As Double = mainSpinEvolver.dτχ1
		  'Var dτχ2 As Double = mainSpinEvolver.dτχ2
		  '
		  '//If any of the suggested time steps are zero, set them to infinity so we don't consider them in the minimum calculation
		  'If dτLN = 0.0 Then
		  'dτLN = INF
		  'End If
		  'If dτχ1 = 0.0 Then
		  'dτχ1 = INF
		  'End If
		  'If dτχ2 = 0.0 Then
		  'dτχ2 = INF
		  'End If
		  '
		  'If Not init Then
		  'Var dτLN_1 As Double = sideSpinEvolver1.dτLN
		  'Var dτχ1_1 As Double = sideSpinEvolver1.dτχ1
		  'Var dτχ2_1 As Double = sideSpinEvolver1.dτχ2
		  '
		  'Var dτLN_2 As Double = sideSpinEvolver2.dτLN
		  'Var dτχ1_2 As Double = sideSpinEvolver2.dτχ1
		  'Var dτχ2_2 As Double = sideSpinEvolver2.dτχ2
		  '
		  'Var dτLN_3 As Double = sideSpinEvolver3.dτLN
		  'Var dτχ1_3 As Double = sideSpinEvolver3.dτχ1
		  'Var dτχ2_3 As Double = sideSpinEvolver3.dτχ2
		  '
		  'Var dτLN_4 As Double = sideSpinEvolver4.dτLN
		  'Var dτχ1_4 As Double = sideSpinEvolver4.dτχ1
		  'Var dτχ2_4 As Double = sideSpinEvolver4.dτχ2
		  '
		  'Var dτLN_5 As Double = sideSpinEvolver5.dτLN
		  'Var dτχ1_5 As Double = sideSpinEvolver5.dτχ1
		  'Var dτχ2_5 As Double = sideSpinEvolver5.dτχ2
		  '
		  'Var dτLN_6 As Double = sideSpinEvolver6.dτLN
		  'Var dτχ1_6 As Double = sideSpinEvolver6.dτχ1
		  'Var dτχ2_6 As Double = sideSpinEvolver6.dτχ2
		  '
		  'Var dτLN_7 As Double = sideSpinEvolver7.dτLN
		  'Var dτχ1_7 As Double = sideSpinEvolver7.dτχ1
		  'Var dτχ2_7 As Double = sideSpinEvolver7.dτχ2
		  '
		  'Var dτLN_8 As Double = sideSpinEvolver8.dτLN
		  'Var dτχ1_8 As Double = sideSpinEvolver8.dτχ1
		  'Var dτχ2_8 As Double = sideSpinEvolver8.dτχ2
		  '
		  'Var dτLN_9 As Double = sideSpinEvolver9.dτLN
		  'Var dτχ1_9 As Double = sideSpinEvolver9.dτχ1
		  'Var dτχ2_9 As Double = sideSpinEvolver9.dτχ2
		  '
		  'Var dτLN_10 As Double = sideSpinEvolver10.dτLN
		  'Var dτχ1_10 As Double = sideSpinEvolver10.dτχ1
		  'Var dτχ2_10 As Double = sideSpinEvolver10.dτχ2
		  '
		  'Var dτLN_11 As Double = sideSpinEvolver11.dτLN
		  'Var dτχ1_11 As Double = sideSpinEvolver11.dτχ1
		  'Var dτχ2_11 As Double = sideSpinEvolver11.dτχ2
		  '
		  'Var dτLN_12 As Double = sideSpinEvolver12.dτLN
		  'Var dτχ1_12 As Double = sideSpinEvolver12.dτχ1
		  'Var dτχ2_12 As Double = sideSpinEvolver12.dτχ2
		  '
		  'Var dτLN_13 As Double = sideSpinEvolver13.dτLN
		  'Var dτχ1_13 As Double = sideSpinEvolver13.dτχ1
		  'Var dτχ2_13 As Double = sideSpinEvolver13.dτχ2
		  '
		  'Var dτLN_14 As Double = sideSpinEvolver14.dτLN
		  'Var dτχ1_14 As Double = sideSpinEvolver14.dτχ1
		  'Var dτχ2_14 As Double = sideSpinEvolver14.dτχ2
		  '
		  'Var dτLN_15 As Double = sideSpinEvolver15.dτLN
		  'Var dτχ1_15 As Double = sideSpinEvolver15.dτχ1
		  'Var dτχ2_15 As Double = sideSpinEvolver15.dτχ2
		  '
		  'Var dτLN_16 As Double = sideSpinEvolver16.dτLN
		  'Var dτχ1_16 As Double = sideSpinEvolver16.dτχ1
		  'Var dτχ2_16 As Double = sideSpinEvolver16.dτχ2
		  '
		  'If dτLN_1 = 0.0 Then
		  'dτLN_1 = INF
		  'End If
		  'If dτχ1_1 = 0.0 Then
		  'dτχ1_1 = INF
		  'End If
		  'If dτχ2_1 = 0.0 Then
		  'dτχ2_1 = INF
		  'End If
		  '
		  'If dτLN_2 = 0.0 Then
		  'dτLN_2 = INF
		  'End If
		  'If dτχ1_2 = 0.0 Then
		  'dτχ1_2 = INF
		  'End If
		  'If dτχ2_2 = 0.0 Then
		  'dτχ2_2 = INF
		  'End If
		  '
		  'If dτLN_3 = 0.0 Then
		  'dτLN_3 = INF
		  'End If
		  'If dτχ1_3 = 0.0 Then
		  'dτχ1_3 = INF
		  'End If
		  'If dτχ2_3 = 0.0 Then
		  'dτχ2_3 = INF
		  'End If
		  '
		  'If dτLN_4 = 0.0 Then
		  'dτLN_4 = INF
		  'End If
		  'If dτχ1_4 = 0.0 Then
		  'dτχ1_4 = INF
		  'End If
		  'If dτχ2_4 = 0.0 Then
		  'dτχ2_4 = INF
		  'End If
		  '
		  'If dτLN_5 = 0.0 Then
		  'dτLN_5 = INF
		  'End If
		  'If dτχ1_5 = 0.0 Then
		  'dτχ1_5 = INF
		  'End If
		  'If dτχ2_5 = 0.0 Then
		  'dτχ2_5 = INF
		  'End If
		  '
		  'If dτLN_6 = 0.0 Then
		  'dτLN_6 = INF
		  'End If
		  'If dτχ1_6 = 0.0 Then
		  'dτχ1_6 = INF
		  'End If
		  'If dτχ2_6 = 0.0 Then
		  'dτχ2_6 = INF
		  'End If
		  '
		  'If dτLN_7 = 0.0 Then
		  'dτLN_7 = INF
		  'End If
		  'If dτχ1_7 = 0.0 Then
		  'dτχ1_7 = INF
		  'End If
		  'If dτχ2_7 = 0.0 Then
		  'dτχ2_7 = INF
		  'End If
		  '
		  'If dτLN_8 = 0.0 Then
		  'dτLN_8 = INF
		  'End If
		  'If dτχ1_8 = 0.0 Then
		  'dτχ1_8 = INF
		  'End If
		  'If dτχ2_8 = 0.0 Then
		  'dτχ2_8 = INF
		  'End If
		  '
		  'If dτLN_9 = 0.0 Then
		  'dτLN_9 = INF
		  'End If
		  'If dτχ1_9 = 0.0 Then
		  'dτχ1_9 = INF
		  'End If
		  'If dτχ2_9 = 0.0 Then
		  'dτχ2_9 = INF
		  'End If
		  '
		  'If dτLN_10 = 0.0 Then
		  'dτLN_10 = INF
		  'End If
		  'If dτχ1_10 = 0.0 Then
		  'dτχ1_10 = INF
		  'End If
		  'If dτχ2_10 = 0.0 Then
		  'dτχ2_10 = INF
		  'End If
		  '
		  'If dτLN_11 = 0.0 Then
		  'dτLN_11 = INF
		  'End If
		  'If dτχ1_11 = 0.0 Then
		  'dτχ1_11 = INF
		  'End If
		  'If dτχ2_11 = 0.0 Then
		  'dτχ2_11 = INF
		  'End If
		  '
		  'If dτLN_12 = 0.0 Then
		  'dτLN_12 = INF
		  'End If
		  'If dτχ1_12 = 0.0 Then
		  'dτχ1_12 = INF
		  'End If
		  'If dτχ2_12 = 0.0 Then
		  'dτχ2_12 = INF
		  'End If
		  '
		  'If dτLN_13 = 0.0 Then
		  'dτLN_13 = INF
		  'End If
		  'If dτχ1_13 = 0.0 Then
		  'dτχ1_13 = INF
		  'End If
		  'If dτχ2_13 = 0.0 Then
		  'dτχ2_13 = INF
		  'End If
		  '
		  'If dτLN_14 = 0.0 Then
		  'dτLN_14 = INF
		  'End If
		  'If dτχ1_14 = 0.0 Then
		  'dτχ1_14 = INF
		  'End If
		  'If dτχ2_14 = 0.0 Then
		  'dτχ2_14 = INF
		  'End If
		  '
		  'If dτLN_15 = 0.0 Then
		  'dτLN_15 = INF
		  'End If
		  'If dτχ1_15 = 0.0 Then
		  'dτχ1_15 = INF
		  'End If
		  'If dτχ2_15 = 0.0 Then
		  'dτχ2_15 = INF
		  'End If
		  '
		  'If dτLN_16 = 0.0 Then
		  'dτLN_16 = INF
		  'End If
		  'If dτχ1_16 = 0.0 Then
		  'dτχ1_16 = INF
		  'End If
		  'If dτχ2_16 = 0.0 Then
		  'dτχ2_16 = INF
		  'End If
		  '
		  'Var dτIdeal As Double = Min(dτv, dτLN, dτχ1, dτχ2, dτLN_1, dτχ1_1, dτχ2_1, dτLN_2, dτχ1_2, dτχ2_2, dτLN_3, dτχ1_3, dτχ2_3, dτLN_4, dτχ1_4, dτχ2_4, dτLN_5, dτχ1_5, dτχ2_5, dτLN_6, dτχ1_6, dτχ2_6, dτLN_7, dτχ1_7, dτχ2_7, dτLN_8, dτχ1_8, dτχ2_8, dτLN_9, dτχ1_9, dτχ2_9, dτLN_10, dτχ1_10, dτχ2_10, dτLN_11, dτχ1_11, dτχ2_11, dτLN_12, dτχ1_12, dτχ2_12, dτLN_13, dτχ1_13, dτχ2_13, dτLN_14, dτχ1_14, dτχ2_14, dτLN_15, dτχ1_15, dτχ2_15, dτLN_16, dτχ1_16, dτχ2_16)
		  'Return dτIdeal
		  'Else
		  'Var dτIdeal As Double = Min(dτv, dτLN, dτχ1, dτχ2)
		  'Return dτIdeal
		  'End If
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Getdιdχℓ(drvtv as integer, stage as string) As Double
		  '// This function calculates the partial derivatives of iota with respect to the χℓ terms using the chain rule. It must make sure
		  '// that each derivative is nonzero before attempting to divide. It also takes the "stage" as a parameter, which tells us which 
		  '// derivatives to use (initial, now, or future).
		  '
		  'Var dιdχ1ℓ As Double
		  'Var dιdχ2ℓ As Double
		  '
		  'If stage = "0" Then
		  'If drvtv = 1 Then
		  'dιdχ1ℓ = dιdχ10z0/(dχ1ℓdχ10z)
		  'If dχ1ℓdχ10x <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ10x0/(dχ1ℓdχ10x)
		  'End If
		  'If dχ1ℓdχ10y <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ10y0/(dχ1ℓdχ10y)
		  'End If
		  'If dχ1ℓdχ20x <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ20x0/(dχ1ℓdχ20x)
		  'End If
		  'If dχ1ℓdχ20y <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ20y0/(dχ1ℓdχ20y)
		  'End If
		  'return dιdχ1ℓ
		  'ElseIf drvtv = 2 Then
		  'dιdχ2ℓ = dιdχ20z0/(dχ2ℓdχ20z)
		  'If dχ2ℓdχ10x <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ10x0/(dχ2ℓdχ10x)
		  'End If
		  'If dχ2ℓdχ10y <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ10y0/(dχ2ℓdχ10y)
		  'End If
		  'If dχ2ℓdχ20x <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ20x0/(dχ2ℓdχ20x)
		  'End If
		  'If dχ2ℓdχ20y <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ20y0/(dχ2ℓdχ20y)
		  'End If
		  'return dιdχ2ℓ
		  'End If
		  '
		  'ElseIf stage = "N" Then
		  'If drvtv = 1 Then
		  'dιdχ1ℓ = dιdχ10zN/(dχ1ℓdχ10z)
		  'If dχ1ℓdχ10x <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ10xN/(dχ1ℓdχ10x)
		  'End If
		  'If dχ1ℓdχ10y <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ10yN/(dχ1ℓdχ10y)
		  'End If
		  'If dχ1ℓdχ20x <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ20xN/(dχ1ℓdχ20x)
		  'End If
		  'If dχ1ℓdχ20y <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ20yN/(dχ1ℓdχ20y)
		  'End If
		  'return dιdχ1ℓ
		  'ElseIf drvtv = 2 Then
		  'dιdχ2ℓ = dιdχ20zN/(dχ2ℓdχ20z)
		  'If dχ2ℓdχ10x <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ10xN/(dχ2ℓdχ10x)
		  'End If
		  'If dχ2ℓdχ10y <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ10yN/(dχ2ℓdχ10y)
		  'End If
		  'If dχ2ℓdχ20x <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ20xN/(dχ2ℓdχ20x)
		  'End If
		  'If dχ2ℓdχ20y <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ20yN/(dχ2ℓdχ20y)
		  'End If
		  'return dιdχ2ℓ
		  'End If
		  '
		  'ElseIf stage = "F" Then
		  'If drvtv = 1 Then
		  'dιdχ1ℓ = dιdχ10zF/(dχ1ℓdχ10z)
		  'If dχ1ℓdχ10x <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ10xF/(dχ1ℓdχ10x)
		  'End If
		  'If dχ1ℓdχ10y <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ10yF/(dχ1ℓdχ10y)
		  'End If
		  'If dχ1ℓdχ20x <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ20xF/(dχ1ℓdχ20x)
		  'End If
		  'If dχ1ℓdχ20y <> 0 Then
		  'dιdχ1ℓ = dιdχ1ℓ + dιdχ20yF/(dχ1ℓdχ20y)
		  'End If
		  'return dιdχ1ℓ
		  'ElseIf drvtv = 2 Then
		  'dιdχ2ℓ = dιdχ20zF/(dχ2ℓdχ20z)
		  'If dχ2ℓdχ10x <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ10xF/(dχ2ℓdχ10x)
		  'End If
		  'If dχ2ℓdχ10y <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ10yF/(dχ2ℓdχ10y)
		  'End If
		  'If dχ2ℓdχ20x <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ20xF/(dχ2ℓdχ20x)
		  'End If
		  'If dχ2ℓdχ20y <> 0 Then
		  'dιdχ2ℓ = dιdχ2ℓ + dιdχ20yF/(dχ2ℓdχ20y)
		  'End If
		  'return dιdχ2ℓ
		  'End If
		  'End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Getdαdotdχℓ(drvtv As integer, stage As string) As Double
		  '// This function calculates the partial derivatives of αdot with respect to the χℓ terms using the chain rule. It must make sure
		  '// that each derivative is nonzero before attempting to divide. It also takes the "stage" as a parameter, which tells us which 
		  '// derivatives to use (initial or now).
		  '
		  '
		  'Var dαdotdχ1ℓ As Double
		  'Var dαdotdχ2ℓ As Double
		  '
		  'If stage = "0" Then
		  'If drvtv = 1 Then
		  'dαdotdχ1ℓ = dαdotdχ10z0/(dχ1ℓdχ10z)
		  'If dχ1ℓdχ10x <> 0 Then
		  'dαdotdχ1ℓ = dαdotdχ1ℓ + dαdotdχ10x0/(dχ1ℓdχ10x)
		  'End If
		  'If dχ1ℓdχ10y <> 0 Then
		  'dαdotdχ1ℓ = dαdotdχ1ℓ + dαdotdχ10y0/(dχ1ℓdχ10y)
		  'End If
		  'If dχ1ℓdχ20x <> 0 Then
		  'dαdotdχ1ℓ = dαdotdχ1ℓ + dαdotdχ20x0/(dχ1ℓdχ20x)
		  'End If
		  'If dχ1ℓdχ20y <> 0 Then
		  'dαdotdχ1ℓ = dαdotdχ1ℓ + dαdotdχ20y0/(dχ1ℓdχ20y)
		  'End If
		  'return dαdotdχ1ℓ
		  'ElseIf drvtv = 2 Then
		  'dαdotdχ2ℓ = dαdotdχ20z0/(dχ2ℓdχ20z)
		  'If dχ2ℓdχ10x <> 0 Then
		  'dαdotdχ2ℓ = dαdotdχ2ℓ + dαdotdχ10x0/(dχ2ℓdχ10x)
		  'End If
		  'If dχ2ℓdχ10y <> 0 Then
		  'dαdotdχ2ℓ = dαdotdχ2ℓ + dαdotdχ10y0/(dχ2ℓdχ10y)
		  'End If
		  'If dχ2ℓdχ20x <> 0 Then
		  'dαdotdχ2ℓ = dαdotdχ2ℓ + dαdotdχ20x0/(dχ2ℓdχ20x)
		  'End If
		  'If dχ2ℓdχ20y <> 0 Then
		  'dαdotdχ2ℓ = dαdotdχ2ℓ + dαdotdχ20y0/(dχ2ℓdχ20y)
		  'End If
		  'return dαdotdχ2ℓ
		  'End If
		  'ElseIf stage = "N" Then
		  'If drvtv = 1 Then
		  'dαdotdχ1ℓ = dαdotdχ10zN/(dχ1ℓdχ10z)
		  'If dχ1ℓdχ10x <> 0 Then
		  'dαdotdχ1ℓ = dαdotdχ1ℓ + dαdotdχ10xN/(dχ1ℓdχ10x)
		  'End If
		  'If dχ1ℓdχ10y <> 0 Then
		  'dαdotdχ1ℓ = dαdotdχ1ℓ + dαdotdχ10yN/(dχ1ℓdχ10y)
		  'End If
		  'If dχ1ℓdχ20x <> 0 Then
		  'dαdotdχ1ℓ = dαdotdχ1ℓ + dαdotdχ20xN/(dχ1ℓdχ20x)
		  'End If
		  'If dχ1ℓdχ20y <> 0 Then
		  'dαdotdχ1ℓ = dαdotdχ1ℓ + dαdotdχ20yN/(dχ1ℓdχ20y)
		  'End If
		  'return dαdotdχ1ℓ
		  'ElseIf drvtv = 2 Then
		  'dαdotdχ2ℓ = dαdotdχ20zN/(dχ2ℓdχ20z)
		  'If dχ2ℓdχ10x <> 0 Then
		  'dαdotdχ2ℓ = dαdotdχ2ℓ + dαdotdχ10xN/(dχ2ℓdχ10x)
		  'End If
		  'If dχ2ℓdχ10y <> 0 Then
		  'dαdotdχ2ℓ = dαdotdχ2ℓ + dαdotdχ10yN/(dχ2ℓdχ10y)
		  'End If
		  'If dχ2ℓdχ20x <> 0 Then
		  'dαdotdχ2ℓ = dαdotdχ2ℓ + dαdotdχ20xN/(dχ2ℓdχ20x)
		  'End If
		  'If dχ2ℓdχ20y <> 0 Then
		  'dαdotdχ2ℓ = dαdotdχ2ℓ + dαdotdχ20yN/(dχ2ℓdχ20y)
		  'End If
		  'return dαdotdχ2ℓ
		  'End If
		  'End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Getdαdχℓ(drvtv as integer, stage as string) As Double
		  '// This function calculates the partial derivatives of α with respect to the χℓ terms using the chain rule. It must make sure
		  '// that each derivative is nonzero before attempting to divide. It also takes the "stage" as a parameter, which tells us which 
		  '// derivatives to use (now or future).
		  '
		  'Var dαdχ1ℓ As Double
		  'Var dαdχ2ℓ As Double
		  '
		  'If stage = "N" Then
		  'If drvtv = 1 Then
		  'dαdχ1ℓ = dαdχ10zN/(dχ1ℓdχ10z)
		  'If dχ1ℓdχ10x <> 0 Then
		  'dαdχ1ℓ = dαdχ1ℓ + dαdχ10xN/(dχ1ℓdχ10x)
		  'End If
		  'If dχ1ℓdχ10y <> 0 Then
		  'dαdχ1ℓ = dαdχ1ℓ + dαdχ10yN/(dχ1ℓdχ10y)
		  'End If
		  'If dχ1ℓdχ20x <> 0 Then
		  'dαdχ1ℓ = dαdχ1ℓ + dαdχ20xN/(dχ1ℓdχ20x)
		  'End If
		  'If dχ1ℓdχ20y <> 0 Then
		  'dαdχ1ℓ = dαdχ1ℓ + dαdχ20yN/(dχ1ℓdχ20y)
		  'End If
		  'return dαdχ1ℓ
		  'ElseIf drvtv = 2 Then
		  'dαdχ2ℓ = dαdχ20zN/(dχ2ℓdχ20z)
		  'If dχ2ℓdχ10x <> 0 Then
		  'dαdχ2ℓ = dαdχ2ℓ + dαdχ10xN/(dχ2ℓdχ10x)
		  'End If
		  'If dχ2ℓdχ10y <> 0 Then
		  'dαdχ2ℓ = dαdχ2ℓ + dαdχ10yN/(dχ2ℓdχ10y)
		  'End If
		  'If dχ2ℓdχ20x <> 0 Then
		  'dαdχ2ℓ = dαdχ2ℓ + dαdχ20xN/(dχ2ℓdχ20x)
		  'End If
		  'If dχ2ℓdχ20y <> 0 Then
		  'dαdχ2ℓ = dαdχ2ℓ + dαdχ20yN/(dχ2ℓdχ20y)
		  'End If
		  'return dαdχ2ℓ
		  'End If
		  'ElseIf stage = "F" Then
		  'If drvtv = 1 Then
		  'dαdχ1ℓ = dαdχ10zF/(dχ1ℓdχ10z)
		  'If dχ1ℓdχ10x <> 0 Then
		  'dαdχ1ℓ = dαdχ1ℓ + dαdχ10xF/(dχ1ℓdχ10x)
		  'End If
		  'If dχ1ℓdχ10y <> 0 Then
		  'dαdχ1ℓ = dαdχ1ℓ + dαdχ10yF/(dχ1ℓdχ10y)
		  'End If
		  'If dχ1ℓdχ20x <> 0 Then
		  'dαdχ1ℓ = dαdχ1ℓ + dαdχ20xF/(dχ1ℓdχ20x)
		  'End If
		  'If dχ1ℓdχ20y <> 0 Then
		  'dαdχ1ℓ = dαdχ1ℓ + dαdχ20yF/(dχ1ℓdχ20y)
		  'End If
		  'return dαdχ1ℓ
		  'ElseIf drvtv = 2 Then
		  'dαdχ2ℓ = dαdχ20zF/(dχ2ℓdχ20z)
		  'If dχ2ℓdχ10x <> 0 Then
		  'dαdχ2ℓ = dαdχ2ℓ + dαdχ10xF/(dχ2ℓdχ10x)
		  'End If
		  'If dχ2ℓdχ10y <> 0 Then
		  'dαdχ2ℓ = dαdχ2ℓ + dαdχ10yF/(dχ2ℓdχ10y)
		  'End If
		  'If dχ2ℓdχ20x <> 0 Then
		  'dαdχ2ℓ = dαdχ2ℓ + dαdχ20xF/(dχ2ℓdχ20x)
		  'End If
		  'If dχ2ℓdχ20y <> 0 Then
		  'dαdχ2ℓ = dαdχ2ℓ + dαdχ20yF/(dχ2ℓdχ20y)
		  'End If
		  'return dαdχ2ℓ
		  'End If
		  'End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetStepRatio(dτIdeal As Double, dτMain As Double) As Double
		  '// Given an ideal time step and a main time step, this method returns the closest ratio that is a multiple of 1/2 (always rounding down)
		  '
		  'Var p As double 
		  'p = Floor(Log(dτIdeal/dτMain)/Log(2))
		  'return 2^p
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitDvdv0()
		  '// This method is called by the EvolverClass constructor, and it initializes dv/dv0 by setting up the first two values.
		  '// It defines a slightly bigger and a slightly smaller v0 in order to run two parallel cases of v. Then, at any time 
		  '// step we can calculate dv/dv0 using the values of vBig and vSmall.
		  '
		  'Var v0Big As Double = v0 + Δv        //Define a slightly bigger v0
		  'Var v0Small As Double = v0 - Δv     //Define a slightly smaller v0
		  '
		  'vBigN = v0Big                                     //Set the initial value of the present vBig to be the inital value of vBig, v0Big
		  'vSmallN = v0Small                             //Set the initial value of the present vSmall to be the inital value of vSmall, v0Small
		  'dvdv00 = CalcDeriv(v0Big, v0Small, 2*Δv)  //Calculate the initial value of dv/dv0 using the centered difference equation
		  '
		  'Var dvdtBig As Double = mainEquation.GetVdot(v0Big)          //Call the vdot equation necessary for making the first step in vBig
		  'Var dvdtSmall As Double = mainEquation.GetVdot(v0Small)  //Call the vdot equation necessary for making the first step in vSmall
		  '
		  'dvdv0dot0 = CalcDeriv(dvdtBig, dvdtSmall, 2*Δv)     // Finding dvdv0dot, which we need in InitψAndDerivs
		  '
		  'Var vBig1 As Double = v0Big + dτ0*dvdtBig              //Perform the first step in vBig using a non-centered difference
		  'Var vSmall1 As Double = v0Small + dτ0*dvdtSmall   //Perform the first step in vSmall using a non-centered difference
		  'vBigF = vBig1                                                               //Set the future value of vBig to be the value we calculated from the first step
		  'vSmallF = vSmall1                                                        //Set the future value of vSmall to be the value we calculated from the first step
		  'dvdv0F = CalcDeriv(vBigF, vSmallF, 2*Δv)                           //Calculate the "future" value of dv/dv0 using the centered difference equation
		  'dvdv0N = dvdv00
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitDvdδ()
		  '// This method is called by the EvolverClass constructor, and it initializes dvdδ by setting up the first two values.
		  '
		  '//The first value is found by using the centered difference for a small change in δ to find the initial dvdδdot, and then 
		  '//by plugging in this value and v0 to an equation for dvdδ to find the initial value of dvdδdot.
		  'Var Δδ As double = 1e-6    //Define a small change in δ to use in the centered difference equation
		  'Var smallδ As Double = δ - Δδ     //Define the smaller δ value
		  'Var bigδ As Double = δ + Δδ        //Define the larger δ value
		  '
		  'Var smallδEq As New VEquationSolver(smallδ, χ1ℓ, χ2ℓ)      //Set up a new equation solver that will use the smaller value of δ
		  'Var bigδEq As New VEquationSolver(bigδ, χ1ℓ, χ2ℓ)             //Set up a new equation solver that will use the larger value of δ
		  '
		  'dvdδdot0 = CalcDeriv(bigδEq.GetVdot(v0), smallδEq.GetVdot(v0), 2*Δδ)          //Perform the centered difference approximation to find the derivative
		  '
		  'Var dvdδ0 As Double = mainEquation.GetDvdδ(v0, dvdδdot0)       //Set the value of the initial dvdδ based on an equation that depends on v and dvdδdot
		  '
		  '//The second value is found by using a non-centered difference approximation to find the future value of dvdδ from only the present value of dvdδ and the present value of dvdδdot
		  'Var dvdδdot As Double = mainEquation.GetDvdδdot(v0, dvdδ0)     //Call the dvdδdot equation necessary for making the first step in dvdδ
		  '
		  'Var dvdδ1 As Double = dvdδ0 + dτ0*dvdδdot           //Perform the step in dvdδ using a non-centered difference
		  '
		  'dvdδN = dvdδ0     //Set the present value of dvdδ to be the initial value we calculated
		  'dvdδF = dvdδ1     //Set the future value of dvdδ to be the second value we calculated
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitDvdχ1ℓ()
		  
		  '// This method is called by the EvolverClass constructor, and it initializes dvdχ1ℓ by setting up the first two values.
		  '
		  '//The first value is found by using the centered difference for a small change in χ1ℓ to find the initial dvdχ1ℓdot, and then 
		  '//by plugging in this value and v0 to an equation for dvdχ1ℓ to find the initial value of dvdχ1ℓdot.
		  'Var Δχ1ℓ As Double = 1e-6    //Define a small change in χ1ℓ to use in the centered difference equation
		  'Var smallχ1ℓ As Double = χ1ℓ - Δχ1ℓ     //Define the smaller χ1ℓ value
		  'Var bigχ1ℓ As Double = χ1ℓ + Δχ1ℓ        //Define the larger χ1ℓ value
		  '
		  'Var smallχ1ℓEq As New VEquationSolver(δ, smallχ1ℓ, χ2ℓ)      //Set up a new equation solver that will use the smaller value of χ1ℓ
		  'Var bigχ1ℓEq As New VEquationSolver(δ, bigχ1ℓ, χ2ℓ)             //Set up a new equation solver that will use the larger value of χ1ℓ
		  '
		  'dvdχ1ℓdot0 = CalcDeriv(bigχ1ℓEq.GetVdot(v0), smallχ1ℓEq.GetVdot(v0), 2*Δχ1ℓ)          //Perform the centered difference approximation to find the derivative
		  '
		  'Var dvdχ1ℓ0 As Double = mainEquation.GetDvdχ1ℓ(v0, dvdχ1ℓdot0)       //Set the value of the initial dvdχ1ℓ based on an equation that depends on v and dvdχ1ℓdot
		  '
		  '//The second value is found by using a non-centered difference approximation to find the future value of dvdχ1ℓ from only the present value of dvdχ1ℓ and the present value of dvdχ1ℓdot
		  'Var dvdχ1ℓdot As Double = mainEquation.GetDvdχ1ℓdot(v0, dvdχ1ℓ0)     //Call the dvdχ1ℓdot equation necessary for making the first step in dvdχ1ℓ
		  '
		  'Var dvdχ1ℓ1 As Double = dvdχ1ℓ0 + dτ0*dvdχ1ℓdot       //Perform the step in dvdχ1ℓ using a non-centered difference
		  '
		  'dvdχ1ℓN = dvdχ1ℓ0     //Set the present value of dvdχ1ℓ to be the initial value we calculated
		  'dvdχ1ℓF = dvdχ1ℓ1     //Set the future value of dvdχ1ℓ to be the second value we calculated
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitDvdχ2ℓ()
		  '
		  '// This method is called by the EvolverClass constructor, and it initializes dvdχ2ℓ by setting up the first two values.
		  '
		  '//The first value is found by using the centered difference for a small change in χ2ℓ to find the initial dvdχ2ℓdot, and then 
		  '//by plugging in this value and v0 to an equation for dvdχ2ℓ to find the initial value of dvdχ2ℓdot.
		  'Var Δχ2ℓ As double = 1e-6    //Define a small change in χ2ℓ to use in the centered difference equation
		  'Var smallχ2ℓ As Double = χ2ℓ - Δχ2ℓ     //Define the smaller χ2ℓ value
		  'Var bigχ2ℓ As Double = χ2ℓ + Δχ2ℓ        //Define the larger χ2ℓ value
		  '
		  'Var smallχ2ℓEq As New VEquationSolver(δ, χ1ℓ, smallχ2ℓ)      //Set up a new equation solver that will use the smaller value of χ2ℓ
		  'Var bigχ2ℓEq As New VEquationSolver(δ, χ1ℓ, bigχ2ℓ)             //Set up a new equation solver that will use the larger value of χ2ℓ
		  '
		  'dvdχ2ℓdot0 = CalcDeriv(bigχ2ℓEq.GetVdot(v0), smallχ2ℓEq.GetVdot(v0), 2*Δχ2ℓ)          //Perform the centered difference approximation to find the derivative
		  '
		  'Var dvdχ2ℓ0 As Double = mainEquation.GetDvdχ2ℓ(v0, dvdχ2ℓdot0)       //Set the value of the initial dvdχ2ℓ based on an equation that depends on v and dvdχ2ℓdot
		  '
		  '//The second value is found by using a non-centered difference approximation to find the future value of dvdχ2ℓ from only the present value of dvdχ2ℓ and the present value of dvdχ2ℓdot
		  'Var dvdχ2ℓdot As Double = mainEquation.GetDvdχ2ℓdot(v0, dvdχ2ℓ0)     //Call the dvdχ2ℓdot equation necessary for making the first step in dvdχ2ℓ
		  '
		  'Var dvdχ2ℓ1 As Double = dvdχ2ℓ0 + dτ0*dvdχ2ℓdot       //Perform the step in dvdχ2ℓ using a non-centered difference
		  '
		  'dvdχ2ℓN = dvdχ2ℓ0     //Set the present value of dvdχ2ℓ to be the initial value we calculated
		  'dvdχ2ℓF = dvdχ2ℓ1     //Set the future value of dvdχ2ℓ to be the second value we calculated
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitSpinVariables()
		  '// This method initializes the spin variables and the derivatives of ι and α by setting up the thirteen parallel instances of SpinEvolver 
		  '// and then pulling necessary values to update the initial two values of the spin variables and derivatives. Most of these are then sent
		  '// to the Main Window when this method is called in the EvolverClass constructor, except for a few which are used to initialize the phase in 
		  '// the InitψAndDerivs method.
		  '
		  '//Define the components of the χ vectors
		  'Var x1 As Double = χ1.GetX
		  'Var x2 As Double = χ2.GetX
		  'Var y1 As Double = χ1.GetY
		  'Var y2 As Double = χ2.GetY
		  'Var z1 As Double = χ1.GetZ
		  'Var z2 As Double = χ2.GetZ
		  '
		  '//Set up the main spin EvolverClass and pull the values calculated in its constructor (the first two values for each parameter)
		  'mainSpinEvolver = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1, y1, z1, x2, y2, z2)
		  'If Not (noSpin1 and noSpin2) Then
		  'ιF = mainSpinEvolver.ιF
		  'ιN = ι0
		  'αF = mainSpinEvolver.αF
		  'αAccN = α0
		  'αAcc = mainSpinEvolver.αAccF
		  'αAccF = αAcc
		  'LNhat0 = mainSpinEvolver.LNhatN
		  'LNhatF = mainSpinEvolver.LNhatF
		  'LNHatMag = mainSpinEvolver.LNhatMag
		  'oldψangleF = mainSpinEvolver.oldψangle
		  'oldψangleN = mainSpinEvolver.oldψangle0
		  'Else
		  'ιF = ι0
		  'ιN = ι0
		  'αF =α0
		  'αAccN = α0
		  'αAcc = α0
		  'αAccF = α0
		  'LNhat0 = mainSpinEvolver.LNhatN
		  'LNhatF = LNhat0
		  'oldψangleN = mainSpinEvolver.oldψangle0
		  'oldψangleF = oldψangleN
		  'End If
		  '
		  'If Not noSpin1 Then
		  'χ1hat0 = mainSpinEvolver.χ1hatN
		  'χ1hatF = mainSpinEvolver.χ1hatF
		  'χ1mag0 = mainSpinEvolver.χ1mag
		  'χ1magF = mainSpinEvolver.χ1mag
		  'Else
		  'χ1hat0 = mainSpinEvolver.χ1hatN
		  'χ1hatF = χ1hat0
		  'χ1mag0 = mainSpinEvolver.χ1mag
		  'χ1magF = χ1mag0
		  'End If
		  '
		  'If Not noSpin2 Then
		  'χ2hat0 = mainSpinEvolver.χ2hatN
		  'χ2hatF = mainSpinEvolver.χ2hatF
		  'χ2mag0 = mainSpinEvolver.χ2mag
		  'χ2magF = mainSpinEvolver.χ2mag
		  'Else
		  'χ2hat0 = mainSpinEvolver.χ2hatN
		  'χ2hatF = χ2hat0
		  'χ2mag0 = mainSpinEvolver.χ2mag
		  'χ2magF = χ2mag0
		  'End If
		  '
		  '
		  '//Set up the twelve side spin EvolverClasss and use the values calculated to compute the first two values of each derivative with the centered difference equation
		  'sideSpinEvolver1 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1 + Δχ, y1, z1, x2, y2, z2)      //Set up one side EvolverClass with a little added to x1
		  'sideSpinEvolver2 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1 - Δχ, y1, z1, x2, y2, z2)      //Set up the other side EvolverClass with a little subtracted from x1
		  'dιdχ10x0 = CalcDeriv(sideSpinEvolver1.ι0, sideSpinEvolver2.ι0, 2*Δχ)                                  //Use the centered difference equation to calculate the first value of dι/dx1 (same process for those below)
		  'dαdχ10x0 = CalcDeriv(sideSpinEvolver1.α0, sideSpinEvolver2.α0, 2*Δχ)
		  'dιdχ10xF = CalcDeriv(sideSpinEvolver1.ιF, sideSpinEvolver2.ιF, 2*Δχ)
		  'dαdχ10xF = CalcDeriv(sideSpinEvolver1.αAccF, sideSpinEvolver2.αAccF, 2*Δχ)
		  'dαdotdχ10x0 = CalcDeriv(sideSpinEvolver1.αdot0, sideSpinEvolver2.αdot0, 2*Δχ)
		  'dιdχ10xN = dιdχ10x0
		  'dαdχ10xN = dαdχ10x0
		  'αAcc1 = sideSpinEvolver1.αAccF
		  'αAcc2 = sideSpinEvolver2.αAccF
		  '
		  '//Repeat the process above for the other five parameters
		  'sideSpinEvolver3 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1, y1 + Δχ, z1, x2, y2, z2)
		  'sideSpinEvolver4 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1, y1 - Δχ, z1, x2, y2, z2)
		  'dιdχ10y0 = CalcDeriv(sideSpinEvolver3.ι0, sideSpinEvolver4.ι0, 2*Δχ)
		  'dαdχ10y0 = CalcDeriv(sideSpinEvolver3.α0, sideSpinEvolver4.α0, 2*Δχ)
		  'dιdχ10yF = CalcDeriv(sideSpinEvolver3.ιF, sideSpinEvolver4.ιF, 2*Δχ)
		  'dαdχ10yF = CalcDeriv(sideSpinEvolver3.αAccF, sideSpinEvolver4.αAccF, 2*Δχ)
		  'dαdotdχ10y0 = CalcDeriv(sideSpinEvolver2.αdot0, sideSpinEvolver3.αdot0, 2*Δχ)
		  'dιdχ10yN = dιdχ10y0
		  'dαdχ10yN = dαdχ10y0
		  '
		  'sideSpinEvolver5 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1, y1, z1 + Δχ, x2, y2, z2)
		  'sideSpinEvolver6 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1, y1, z1 - Δχ, x2, y2, z2)
		  'dιdχ10z0 = CalcDeriv(sideSpinEvolver5.ι0, sideSpinEvolver6.ι0, 2*Δχ)
		  'dαdχ10z0 = CalcDeriv(sideSpinEvolver5.α0, sideSpinEvolver6.α0, 2*Δχ)
		  'dαdotdχ10z0 = CalcDeriv(sideSpinEvolver5.αdot0, sideSpinEvolver6.αdot0, 2*Δχ)
		  'dιdχ10zF = CalcDeriv(sideSpinEvolver5.ιF, sideSpinEvolver6.ιF, 2*Δχ)
		  'dαdχ10zF = CalcDeriv(sideSpinEvolver5.αAccF, sideSpinEvolver6.αAccF, 2*Δχ)
		  'dιdχ10zN = dιdχ10z0
		  'dαdχ10zN = dαdχ10z0
		  '
		  'sideSpinEvolver7 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1, y1, z1, x2 + Δχ, y2, z2)
		  'sideSpinEvolver8 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1, y1, z1, x2 - Δχ, y2, z2)
		  'dιdχ20x0 = CalcDeriv(sideSpinEvolver7.ι0, sideSpinEvolver8.ι0, 2*Δχ)
		  'dαdχ20x0 = CalcDeriv(sideSpinEvolver7.α0, sideSpinEvolver8.α0, 2*Δχ)
		  'dιdχ20xF = CalcDeriv(sideSpinEvolver7.ιF, sideSpinEvolver8.ιF, 2*Δχ)
		  'dαdχ20xF = CalcDeriv(sideSpinEvolver7.αAccF, sideSpinEvolver8.αAccF, 2*Δχ)
		  'dαdotdχ20x0 = CalcDeriv(sideSpinEvolver7.αdot0, sideSpinEvolver8.αdot0, 2*Δχ)
		  'dιdχ20xN = dιdχ20x0
		  'dαdχ20xN = dαdχ20x0
		  '
		  'sideSpinEvolver9 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1, y1, z1, x2, y2 + Δχ, z2)
		  'sideSpinEvolver10 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1, y1, z1, x2, y2 - Δχ, z2)
		  'dιdχ20y0 = CalcDeriv(sideSpinEvolver9.ι0, sideSpinEvolver10.ι0, 2*Δχ)
		  'dαdχ20y0 = CalcDeriv(sideSpinEvolver9.α0, sideSpinEvolver10.α0, 2*Δχ)
		  'dιdχ20yF = CalcDeriv(sideSpinEvolver9.ιF, sideSpinEvolver10.ιF, 2*Δχ)
		  'dαdχ20yF = CalcDeriv(sideSpinEvolver9.αAccF, sideSpinEvolver10.αAccF, 2*Δχ)
		  'dαdotdχ20y0 = CalcDeriv(sideSpinEvolver9.αdot0, sideSpinEvolver10.αdot0, 2*Δχ)
		  'dιdχ20yN = dιdχ20y0
		  'dαdχ20yN = dαdχ20y0
		  '
		  'sideSpinEvolver11 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1, y1, z1, x2, y2, z2 + Δχ)
		  'sideSpinEvolver12 = New SpinEvolver(v0, dτ0, δ, Ln0, ι0, α0, x1, y1, z1, x2, y2, z2 - Δχ)
		  'dιdχ20z0 = CalcDeriv(sideSpinEvolver11.ι0, sideSpinEvolver12.ι0, 2*Δχ)
		  'dαdχ20z0 = CalcDeriv(sideSpinEvolver11.α0, sideSpinEvolver12.α0, 2*Δχ)
		  'dιdχ20zF = CalcDeriv(sideSpinEvolver11.ιF, sideSpinEvolver12.ιF, 2*Δχ)
		  'dαdχ20zF = CalcDeriv(sideSpinEvolver11.αAccF, sideSpinEvolver12.αAccF, 2*Δχ)
		  'dαdotdχ20z0 = CalcDeriv(sideSpinEvolver11.αdot0, sideSpinEvolver12.αdot0, 2*Δχ)
		  'dιdχ20zN = dιdχ20z0
		  'dαdχ20zN = dαdχ20z0
		  '
		  '//Now, do a similar process to find the derivatives of alphadot and iota with respect to delta (to be used to find the phase derivatives)
		  'sideSpinEvolver13 = New SpinEvolver(v0, dτ0, δ + Δδ, Ln0, ι0, α0, x1, y1, z1, x2, y2, z2)
		  'sideSpinEvolver14 = New SpinEvolver(v0, dτ0, δ - Δδ, Ln0, ι0, α0, x1, y1, z1, x2, y2, z2)
		  'dιdδ0 = CalcDeriv(sideSpinEvolver13.ι0, sideSpinEvolver14.ι0, 2*Δδ)
		  'dαdδ0 = CalcDeriv(sideSpinEvolver13.α0, sideSpinEvolver14.α0, 2*Δδ)        //Find the initial value of dα/dδ using the centered difference of the two parallel cases (and initial alpha values)
		  'dαdotdδ0 = CalcDeriv(sideSpinEvolver13.αdot0, sideSpinEvolver14.αdot0, 2*Δδ)
		  '
		  '//And again with v0 (again, to be used to find the phase derivatives)
		  'sideSpinEvolver15 = New SpinEvolver(v0 + Δv, dτ0, δ, Ln0, ι0, α0, x1, y1, z1, x2, y2, z2)
		  'sideSpinEvolver16 = New SpinEvolver(v0 - Δv, dτ0, δ, Ln0, ι0, α0, x1, y1, z1, x2, y2, z2)
		  'dιdv00 = CalcDeriv(sideSpinEvolver15.ι0, sideSpinEvolver16.ι0, 2*Δv)
		  'dαdv00 = CalcDeriv(sideSpinEvolver15.α0, sideSpinEvolver16.α0, 2*Δv)        //Find the initial value of dα/dv0 using the centered difference of the two parallel cases (and initial alpha values)
		  'dαdotdv00 = CalcDeriv(sideSpinEvolver15.αdot0, sideSpinEvolver16.αdot0, 2*Δv)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitV()
		  '// This method is called by the EvolverClass constructor, and it initializes v by setting up the first two values.
		  '
		  'vN = v0 //Set the initial value of the past v to be the inital value of v, v0. This value of v0 comes from the main program as one of the initial parameters
		  '
		  'Var dvdt As Double = mainEquation.GetVdot(v0) //Call the vdot equation necessary for making the first step in v
		  '
		  'Var v1 As Double = v0 + dτ0*dvdt    //Perform the step in v using a non-centered difference
		  '
		  'vF = v1    //Set the present value of v to be the value we calculated from the first step
		  '
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitψAndDerivs()
		  '// This method initializes the wave phase along with its derivatives by calculating the first two values of each. We find the
		  '// first phase or derivative value the same way as we do in the DoStep method, but the second value is found with a non-centered
		  '// difference approximation using the initial value of each phase or derivative. Then, this method is called in the EvolverClass
		  '// constructor to get the initial phase derivatives, so that we can keep getting phase and phase derivative values in the DoStep method.
		  '
		  '//First initialize the phase
		  'Var ψR0 As Double = λ0        //The initial phase is λ0 by definition
		  'ψRN = ψR0      //Set the "now" value of the phase to be the initial value
		  'Var αdot0 As Double
		  'If Not (noSpin1 and noSpin2) Then
		  'αdot0 = mainSpinEvolver.αdot0       //Pull the initial value of αdot from the main spin EvolverClass
		  'End If
		  'Var vdot0 As Double = mainEquation.GetVdot(v0)
		  'Var dψdτ0 As Double = v0 - Cos(ι0)*αdot0 - 2*v0^2*(Log(v0/v0) + 1)*vdot0       //Calculate the time derivative of the phase at tau = 0
		  'Var tR0 As Double = 0 //From tR = M*(z+1)*τSpin, but τSpin starts at zero
		  'ψRF = ψR0 + dτ0*(1 + Vo*Sin(Θ)*Sin(Ω*tR0 + Φ))*dψdτ0     //Finally, perform the step in the phase using the adapted centered difference equation
		  '
		  '//Then initialize the derivatives
		  'Var dψRdδ0 As Double = 0      //Set the initial δ derivative of the phase to zero
		  'dψRdδN = dψRdδ0     //Make the current δ derivative this initial value
		  'Var dψdτ_dδ0 As Double = dvdδN + Sin(ι0)*dιdδ0*αdot0 - Cos(ι0)*dαdotdδ0 _
		  '- 4*v0*dvdδN*(Log(v0/v0) + 2)*vdot0 - 2*v0^2*(Log(v0/v0) + 1)*dvdδdot0     //Calculate the initial δ derivative of ψdot
		  'dψRdδF = dψRdδ0 + dτ0*(1 + Vo*Sin(Θ)*Sin(Ω*tR0 + Φ))*dψdτ_dδ0     //And perform the first step to get the second value of ψdot
		  '
		  'Var dψRdv00 As Double = 0     //Set the initial v0 derivative of the phase to zero
		  'dψRdv0N = dψRdv00     //Make the current v0 derivative this initial value
		  'Var dψdτ_dv00 As Double = dvdv00 + Sin(ι0)*dιdv00*αdot0 - Cos(ι0)*dαdotdv00 _
		  '- 4*v0*dvdv00*(Log(v0/v0) + 2)*vdot0 - 2*v0^2*(Log(v0/v0) + 1)*dvdv0dot0     //Calculate the initial v0 derivative of ψdot
		  'dψRdv0F = dψRdv00 + dτ0*(1 + Vo*Sin(Θ)*Sin(Ω*tR0 + Φ))*dψdτ_dv00     //And perform the first step to get the second value of the v0 derivative of the phase
		  '
		  'Var dψRdχ1ℓ0 As Double = 0     //Set the initial χ1ℓ derivative of the phase to zero
		  'dψRdχ1ℓN = dψRdχ1ℓ0     //Make the current χ1ℓ derivative this initial value
		  '//Finding the initial χ1ℓ derivative of ι and αdot using the chain rule
		  'Var dιdχ1ℓ0 As Double = Getdιdχℓ(1, "0")
		  'Var dαdotdχ1ℓ0 As Double = Getdαdotdχℓ(1, "0")
		  'Var dψdτ_dχ1ℓ0 As Double = dvdχ1ℓN + Sin(ι0)*dιdχ1ℓ0*αdot0 - Cos(ι0)*dαdotdχ1ℓ0 _
		  '- 4*v0*dvdχ1ℓN*(Log(v0/v0) + 2)*vdot0 - 2*v0^2*(Log(v0/v0) + 1)*dvdχ1ℓdot0     //Calculate the initial χ1ℓ derivative of ψdot
		  'dψRdχ1ℓF = dψRdχ1ℓ0 + dτ0*(1 + Vo*Sin(Θ)*Sin(Ω*tR0 + Φ))*dψdτ_dχ1ℓ0     //And perform the first step to get the second value of the χ1ℓ derivative of the phase
		  '
		  'Var dψRdχ2ℓ0 As Double = 0     //Set the initial χ2ℓ derivative of the phase to zero
		  'dψRdχ2ℓN = dψRdχ2ℓ0     //Make the current χ2ℓ derivative this initial value
		  '//Finding the initial χ2ℓ derivative of ι and αdot using the chain rule
		  'Var dιdχ2ℓ0 As Double = Getdιdχℓ(2, "0")
		  'Var dαdotdχ2ℓ0 As Double = Getdαdotdχℓ(2, "0")
		  'Var dψdτ_dχ2ℓ0 As Double = dvdχ2ℓN + Sin(ι0)*dιdχ2ℓ0*αdot0 - Cos(ι0)*dαdotdχ2ℓ0 _
		  '- 4*v0*dvdχ2ℓN*(Log(v0/v0) + 2)*vdot0 - 2*v0^2*(Log(v0/v0) + 1)*dvdχ2ℓdot0     //Calculate the initial χ2ℓ derivative of ψdot
		  'dψRdχ2ℓF = dψRdχ2ℓ0 + dτ0*(1 + Vo*Sin(Θ)*Sin(Ω*tR0 + Φ))*dψdτ_dχ2ℓ0     //And perform the first step to get the second value of the χ2ℓ derivative of the phase
		  '
		  'Var dψRdΘ0 As Double = 0     //Set the initial Θ derivative of the phase to zero
		  'dψRdΘN = dψRdΘ0     //Make the current Θ derivative this initial value
		  'dψRdΘF = dψRdΘ0 + dτ0*(Vo*Cos(Θ)*Sin(Ω*tR0 + Φ))*dψdτ0     //Perform the first step to get the second value of the Θ derivative of the phase
		  '
		  'Var dψRdΦ0 As Double = 0     //Set the initial Φ derivative of the phase to zero
		  'dψRdΦN = dψRdΦ0     //Make the current Φ derivative this initial value
		  'dψRdΦF = dψRdΦ0 + dτ0*(Vo*Sin(Θ)*Cos(Ω*tR0 + Φ))*dψdτ0     //Perform the first step to get the second value of the Φ derivative of the phase
		  '
		  'Var dψRdz0 As Double = 0     //Set the initial z derivative of the phase to zero
		  'dψRdzN = dψRdz0     //Make the current z derivative this initial value
		  'dψRdzF = dψRdz0 - dτ0*(z/(1 + z))*(1 + Vo*Sin(Θ)*Sin(Ω*tR0 + Φ))*dψdτ0     //Perform the first step to get the second value of the z derivative of the phase
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ReadyToGo() As Boolean
		  '// ***this method is a remnant from the summer '22 work, when we didn't want the program to run if there was no spin evolution.
		  '
		  '// This method checks to see if the initial ι values and its derivatives are zero. If so, then
		  '// we don't want the program to run, so we return False and have this value called in the 
		  '// CreateArrays method.
		  '
		  'if Abs(ι0) < 10^-8 then     //Checking to see if ι is zero (or close enough to zero, since floating point issues may occur)
		  'ι0 = 0    //Setting ι to actually be zero if it is a floating point issue
		  'end if
		  '
		  'if Abs(dιdχ10x0) < 10^-8 then
		  'dιdχ10x0 = 0
		  'end if
		  '
		  'if Abs(dιdχ10y0) < 10^-8 then
		  'dιdχ10y0 = 0
		  'end if
		  '
		  'if Abs(dιdχ10z0) < 10^-8 then
		  'dιdχ10z0 = 0
		  'end if
		  '
		  'if Abs(dιdχ20x0) < 10^-8 then
		  'dιdχ20x0 = 0
		  'end if
		  '
		  'if Abs(dιdχ20y0) < 10^-8 then
		  'dιdχ20y0 = 0
		  'end if
		  '
		  'if Abs(dιdχ20z0) < 10^-8 then
		  'dιdχ20z0 = 0
		  'end if
		  '
		  'if ι0 = 0 And dιdχ10x0 = 0 And dιdχ10y0 = 0 And dιdχ10z0 = 0 _
		  'And dιdχ20x0 = 0 And dιdχ20y0 = 0 And dιdχ20z0 = 0 then     //If the initial values of ι and all its derivatives are zero...
		  'Return False     //Return false (so the program won't run) 
		  'else
		  'Return True     //Otherwise, ι is changing in some regard, so we want the program to run
		  'end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ReturnValues(τMain As Double) As Double()
		  '// This method is how the EvolverClass class and Main class communicate with each other. The Main class calls this method, passing
		  '// in a current time, and the EvolverClass class evolves until that time or interpolates at that time to return the appropriate values. This 
		  '// method is also where the EvolverClass class is calling functions to calculate ideal time steps and actually calling the DoStep function.
		  '
		  'Var returnList() As Double
		  '
		  'Var dτIdeal As Double = FindIdealdτ
		  'Var ratio As Double = GetStepRatio(dτIdeal, maindτ0)
		  'Var dτReal As Double = maindτ0*ratio
		  'Var τSpinSteps As Integer
		  'Var τMainSteps As Integer 
		  '
		  '// Put the spin and main times in terms of steps instead of time to better compare them (using the smaller time step as a unit)
		  'If dτReal < maindτ0 Then
		  'τSpinSteps = Round(τSpin/dτReal)
		  'τMainSteps = Round(τMain/dτReal)
		  'Else
		  'τSpinSteps = Round(τSpin/maindτ0)
		  'τMainSteps = Round(τMain/maindτ0)
		  'End If
		  '
		  '
		  'If τMainSteps = τSpinSteps Then  // if we are at the requested time, simply return our current values
		  'returnList.Add(vF)
		  'returnList.Add(dvdv0F)
		  'returnList.Add(dvdδF)
		  'returnList.Add(dvdχ1ℓF)
		  'returnList.Add(dvdχ2ℓF)
		  'returnList.Add(ιF)
		  'returnList.Add(αAccF)
		  'returnList.Add(dιdv0F)
		  'returnList.Add(dαdv0F)
		  'returnList.Add(dιdδF)
		  'returnList.Add(dαdδF)
		  'returnList.Add(Getdιdχℓ(1, "F"))
		  'returnList.Add(Getdιdχℓ(2, "F"))
		  'returnList.Add(Getdαdχℓ(1, "F"))
		  'returnList.Add(Getdαdχℓ(2, "F"))
		  'returnList.Add(ψRF)
		  'returnList.Add(dψRdv0F)
		  'returnList.Add(dψRdδF)
		  'returnList.Add(dψRdΘF)
		  'returnList.Add(dψRdΦF)
		  'returnList.Add(dψRdχ1ℓF)
		  'returnList.Add(dψRdχ2ℓF)
		  'returnList.Add(oldψangleF)
		  '
		  'ElseIf τMainSteps > τSpinSteps And ratio = 1 Then  // if we are only one step behind the requested time
		  'DoStep(dτReal)  // perform the step
		  '// and return the appropriate values
		  'returnList.Add(vF)
		  'returnList.Add(dvdv0F)
		  'returnList.Add(dvdδF)
		  'returnList.Add(dvdχ1ℓF)
		  'returnList.Add(dvdχ2ℓF)
		  'returnList.Add(ιF)
		  'returnList.Add(αAccF)
		  'returnList.Add(dιdv0F)
		  'returnList.Add(dαdv0F)
		  'returnList.Add(dιdδF)
		  'returnList.Add(dαdδF)
		  'returnList.Add(Getdιdχℓ(1, "F"))
		  'returnList.Add(Getdιdχℓ(2, "F"))
		  'returnList.Add(Getdαdχℓ(1, "F"))
		  'returnList.Add(Getdαdχℓ(2, "F"))
		  'returnList.Add(ψRF)
		  'returnList.Add(dψRdv0F)
		  'returnList.Add(dψRdδF)
		  'returnList.Add(dψRdΘF)
		  'returnList.Add(dψRdΦF)
		  'returnList.Add(dψRdχ1ℓF)
		  'returnList.Add(dψRdχ2ℓF)
		  'returnList.Add(oldψangleF)
		  '
		  'ElseIf τMainSteps > τSpinSteps And ratio < 1 Then  // if we are many steps behind the requested time
		  'While τSpinSteps < τMainSteps  // step until we are at the requested time, calculating a new ideal step each time
		  'DoStep(dτReal)
		  'dτIdeal = FindIdealdτ
		  'ratio = GetStepRatio(dτIdeal, maindτ0)
		  'dτReal = maindτ0*ratio
		  'τSpinSteps = Round(τSpin/dτReal)
		  'τMainSteps = Round(τMain/dτReal)
		  '
		  'Wend
		  '// and return the appropriate values
		  'returnList.Add(vF)
		  'returnList.Add(dvdv0F)
		  'returnList.Add(dvdδF)
		  'returnList.Add(dvdχ1ℓF)
		  'returnList.Add(dvdχ2ℓF)
		  'returnList.Add(ιF)
		  'returnList.Add(αAccF)
		  'returnList.Add(dιdv0F)
		  'returnList.Add(dαdv0F)
		  'returnList.Add(dιdδF)
		  'returnList.Add(dαdδF)
		  'returnList.Add(Getdιdχℓ(1, "F"))
		  'returnList.Add(Getdιdχℓ(2, "F"))
		  'returnList.Add(Getdαdχℓ(1, "F"))
		  'returnList.Add(Getdαdχℓ(2, "F"))
		  'returnList.Add(ψRF)
		  'returnList.Add(dψRdv0F)
		  'returnList.Add(dψRdδF)
		  'returnList.Add(dψRdΘF)
		  'returnList.Add(dψRdΦF)
		  'returnList.Add(dψRdχ1ℓF)
		  'returnList.Add(dψRdχ2ℓF)
		  'returnList.Add(oldψangleF)
		  '
		  'ElseIf ratio > 1 Then  // if the spin time step is larger than the main time step
		  'If  τMainSteps > τSpinSteps Then  // if we are behind the requested time, step forward. Otherwise, no need to step.
		  'DoStep(dτReal)
		  'τSpinSteps = Round(τSpin/maindτ0)
		  'End If
		  '// interpolate the appropriate values (approximating the change over the step to be linear) and return them.
		  'Var intrpltRatio As Double = 1/(τSpinSteps - (τMainSteps - 1))
		  'returnList.Add(vN + intrpltRatio*(vF - vN))
		  'returnList.Add(dvdv0N + intrpltRatio*(dvdv0F - dvdv0N))
		  'returnList.Add(dvdδN + intrpltRatio*(dvdδF - dvdδN))
		  'returnList.Add(dvdχ1ℓN + intrpltRatio*(dvdχ1ℓF - dvdχ1ℓN))
		  'returnList.Add(dvdχ2ℓN + intrpltRatio*(dvdχ2ℓF - dvdχ2ℓN))
		  'returnList.Add(ιN + intrpltRatio*(ιF - ιN))
		  'returnList.Add(αAccN + intrpltRatio*(αAccF - αAccN)) 
		  'returnList.Add(dιdv0N + intrpltRatio*(dιdv0F - dιdv0N))
		  'returnList.Add(dαdv0N + intrpltRatio*(dαdv0F - dαdv0N))
		  'returnList.Add(dιdδN + intrpltRatio*(dιdδF - dιdδN))
		  'returnList.Add(dαdδN + intrpltRatio*(dαdδF - dαdδN))
		  'returnList.Add(dιdχ1ℓN + intrpltRatio*(Getdιdχℓ(1, "F") - dιdχ1ℓN)) 
		  'returnList.Add(dιdχ2ℓN + intrpltRatio*(Getdιdχℓ(2, "F") - dιdχ2ℓN)) 
		  'Var dαdχ1ℓN As Double = Getdαdχℓ(1, "N")
		  'Var dαdχ2ℓN As Double = Getdαdχℓ(2, "N")
		  'returnList.Add(dαdχ1ℓN + intrpltRatio*(Getdαdχℓ(1, "F") - dαdχ1ℓN))
		  'returnList.Add(dαdχ2ℓN + intrpltRatio*(Getdαdχℓ(2, "F") - dαdχ2ℓN))
		  'returnList.Add(ψRN + intrpltRatio*(ψRF - ψRN))
		  'returnList.Add(dψRdv0N + intrpltRatio*(dψRdv0F - dψRdv0N))
		  'returnList.Add(dψRdδN + intrpltRatio*(dψRdδF - dψRdδN))
		  'returnList.Add(dψRdΘN + intrpltRatio*(dψRdΘF - dψRdΘN))
		  'returnList.Add(dψRdΦN + intrpltRatio*(dψRdΦF - dψRdΦN))
		  'returnList.Add(dψRdχ1ℓN + intrpltRatio*(dψRdχ1ℓF - dψRdχ1ℓN))
		  'returnList.Add(dψRdχ2ℓN + intrpltRatio*(dψRdχ2ℓF - dψRdχ2ℓN))
		  'returnList.Add(oldψangleN + intrpltRatio*(oldψangleF - oldψangleN)) 
		  '
		  'Else  // there should be no other possibility. Something is wrong if we get here.
		  'Var aaa As Double = FindIdealdτ
		  'Var aa As Double = maindτ0
		  'Var a As Double = GetStepRatio(dτIdeal, dτ0)
		  'break 
		  'End If
		  '
		  '
		  'return returnList
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetIdealdτv(v As Double)
		  'Var ε1 As Double = 1.0E-4 // this is the small fraction we want to limit changes in v to
		  'dτv = (1 - v)*ε1/mainEquation.GetVdot(v)
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		ATA(14,14) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cosβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		cosΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdv00 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdv0dot0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdv0F As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdv0N As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdδdot0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdδF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdδN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdδP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdχ1ℓdot0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdχ1ℓF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdχ1ℓN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdχ1ℓP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdχ2ℓdot0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdχ2ℓF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdχ2ℓN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dvdχ2ℓP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dzd(14) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdv00 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdv0F As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdv0N As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdδ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdδF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdδN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ10x0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ10xF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ10xN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ10y0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ10yF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ10yN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ10z0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ10zF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ10zN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ1ℓN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ20x0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ20xF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ20xN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ20y0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ20yF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ20yN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ20z0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ20zF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ20zN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dιdχ2ℓN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdv00 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdv0N As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdδ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdδN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ10x0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ10xN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ10y0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ10yN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ10z0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ10zN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ20x0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ20xN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ20y0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ20yN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ20z0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdotdχ20zN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdv00 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdv0F As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdv0N As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdδ0 As double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdδF As double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdδN As double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ10x0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ10xF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ10xN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ10y0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ10yF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ10yN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ10z0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ10zF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ10zN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ20x0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ20xF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ20xN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ20y0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ20yF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ20yN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ20z0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ20zF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dαdχ20zN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dηdδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτLN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτv As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτχ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτχ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ1ℓdχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ1ℓdχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ1ℓdχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ1ℓdχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ1ℓdχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ1ℓdχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ2ℓdχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ2ℓdχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ2ℓdχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ2ℓdχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ2ℓdχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχ2ℓdχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχadχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdχ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dχsdχ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdv0F As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdv0N As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdv0P As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdzF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdzN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdzP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdδF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdδN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdδP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdΘF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdΘN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdΘP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdΦF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdΦN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdΦP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdχ1ℓF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdχ1ℓN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdχ1ℓP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdχ2ℓF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdχ2ℓN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dψRdχ2ℓP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		f0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		f2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FullTurns As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		h As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		hc As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		hp As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		INF As Double = 1e90
	#tag EndProperty

	#tag Property, Flags = &h0
		K As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ln0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		LNhat0 As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		LNhatF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		LNhatMag As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		M As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		maindτ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		mainEquation As VEquationSolver
	#tag EndProperty

	#tag Property, Flags = &h0
		mainSpinEvolver As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		nDetectors As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		noSpin1 As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		noSpin2 As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		oldψangleF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		oldψangleN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		PNOrder As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		QuarterTurns As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		R As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver1 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver10 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver11 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver12 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver13 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver14 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver15 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver16 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver2 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver3 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver4 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver5 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver6 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver7 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver8 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sideSpinEvolver9 As SpinEvolver
	#tag EndProperty

	#tag Property, Flags = &h0
		sinβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		sinΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		sn2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		tpot As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		v0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		vBigF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		vBigN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		vBigP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		vF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		vN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Vo As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		vP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		vSmallF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		vSmallN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		vSmallP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		wasInverted As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		z As Double
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
		αAcc As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αAcc1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αAcc2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αAccF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αAccN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		δ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Δv As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Δδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Δχ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ζ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		η As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		λ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		π As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		τSpin As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Φ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1 As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1hat0 As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1hatF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1mag0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1magF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2 As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2hat0 As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2hatF As Vector
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2mag0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2magF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2ℓ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ψangle As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ψRF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ψRN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ψRP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ω As Double
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
			Name="δ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ1ℓ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2ℓ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dηdδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdχ1ℓ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdχ2ℓ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadχ1ℓ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadχ2ℓ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="vP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdδP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdχ1ℓP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdχ2ℓP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdδN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdχ1ℓN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdχ2ℓN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="vN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dτ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="v0"
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
			Name="αF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Δχ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Ln0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ10x0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ10xF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ10x0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ10xF"
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
			Name="dιdχ10y0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ10z0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ10yF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ10zF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ10y0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ10z0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ10yF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ10zF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ20x0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ20xF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ20y0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ20yF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ20z0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ20zF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ20x0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ20xF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ20y0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ20yF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ20z0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ20zF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ1ℓdχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ1ℓdχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ1ℓdχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ1ℓdχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ1ℓdχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ1ℓdχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ2ℓdχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ2ℓdχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ2ℓdχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ2ℓdχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ2ℓdχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχ2ℓdχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχadχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dχsdχ20z"
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
			Name="vBigP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="vBigN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="vSmallP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdv0F"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdv00"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="vSmallN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Δv"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ψRN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ψRP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdδ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdδN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Δδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdδ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdδdot0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdv00"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdv0N"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdv00"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdv0dot0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdδF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdχ1ℓF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdχ2ℓF"
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
			Name="dτP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dτv"
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
			Name="vF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="τSpin"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="vBigF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="vSmallF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ψRF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Vo"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="λ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Θ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Ω"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Φ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="M"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdδF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdδN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdδN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdδP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdv0N"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdv0P"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdv0F"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdv0N"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdχ1ℓN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ10z0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdχ1ℓdot0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdχ1ℓF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ10zN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdχ1ℓP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdχ2ℓdot0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdχ2ℓN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdχ2ℓP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdχ2ℓF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdΘN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdΘP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdΘF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdΦN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdΦF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdΦP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdzN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdzP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dψRdzF"
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
			Name="wasInverted"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ1mag0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2mag0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ1magF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2magF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="INF"
			Visible=false
			Group="Behavior"
			InitialValue="1e90"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdv00"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdv0N"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdδ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdδN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
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
			Name="αAcc"
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
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdv0F"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdδF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdv0F"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdδF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="R"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="f0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cosβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="sinβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="sn2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ψangle"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="tpot"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="nDetectors"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="f2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cosΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="sinΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PNOrder"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="K"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ζ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="h"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="hp"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="hc"
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
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="oldψangleF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dvdv0N"
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
			Name="αAccN"
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
			Name="dιdχ10zN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ10zN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="oldψangleN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="maindτ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="noSpin1"
			Visible=false
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="noSpin2"
			Visible=false
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αAcc1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αAcc2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ1ℓN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ2ℓN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ10xN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ10yN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ20xN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ20yN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dιdχ20zN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ10xN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ10yN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ20xN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ20yN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdχ20zN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ10x0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ10y0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ20z0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ20x0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ20y0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ10xN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ10yN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ20zN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ20xN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dαdotdχ20yN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
