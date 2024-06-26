#tag Class
Protected Class WaveBuilderClass
	#tag Method, Flags = &h0
		Sub AssembleDerivatives()
		  // These constants help us build the detector functions
		  Static cos2ψ As Double = Cos(2*Parameters.ψ)
		  Static sin2ψ As Double = Sin(2*Parameters.ψ)
		  Static σ1 As Double = 1.5*Parameters.π + 2.0*Parameters.ρ0
		  Static σ2 As Double = (4/3)*Parameters.π + σ1
		  
		  Static dpc1 As Double = 3.0*Sqrt(3.0)/128.0
		  Static dpc2 As Double = Sqrt(3.0)/128.0
		  Static dpc3 As Double = 3.0/32.0
		  Static dxc1 As Double = Sqrt(3.0)/32.0
		  Static dxc2 As Double = 3.0/32.0
		  
		  Static sΘ As Double = Sin(Parameters.Θ)
		  Static cΘ As Double = Cos(Parameters.Θ)
		  Static s2Θ As Double = 2*sΘ*cΘ
		  Static c2Θ As Double = cΘ*cΘ - sΘ*sΘ
		  
		  // Now start calculating detector functions
		  Var ρ As Double = Parameters.GMΩe*τrDN
		  Var s210 As Double = Sin(2.0*ρ - σ1)
		  Var s012 As Double = Sin(σ1 - 2.0*Parameters.Φ)
		  Var s412 As Double = Sin(4.0*ρ - σ1 - 2.0*Parameters.Φ)
		  Var s311 As Double = Sin(3.0*ρ - σ1 - Parameters.Φ)
		  Var s111 As Double = Sin(ρ - σ1 + Parameters.Φ)
		  Var c012 As Double = Cos(σ1 - 2.0*Parameters.Φ)
		  Var c412 As Double = Cos(4.0*ρ - σ1 -2.0*Parameters.Φ)
		  Var c311 As Double = Cos(3.0*ρ - σ1 - Parameters.Φ)
		  Var c111 As Double = Cos(ρ - σ1 + Parameters.Φ)
		  
		  Var dp As Double = dpc1*(-6.0*s210 + 9.0*s012 - s412) + dpc2*c2Θ*(18.0*s210 + 9.0*s012 - s412) - dpc3*s2Θ*(s311 - 3.0*s111)
		  Var dx As Double = dxc1*cΘ*(9.0*c012 - c412) - dxc2*sΘ*(s311 - 3.0*s111)
		  Var ddpdΘ As Double = -2.0*dpc2*s2Θ*(18.0*s210 + 9.0*s012 - s412) - 2.0*dpc3*c2Θ*(s311 - 3.0*s111)
		  Var ddxdΘ As Double = -dxc1*sΘ*(9.0*c012 - c412) - dxc2*cΘ*(s311 - 3.0*s111)
		  Var ddpdΦ As Double = dpc1*(-18.0*c012 + 2.0*c412) + dpc2*c2Θ*(-18.0*c012 + 2.0*c412) + dpc3*s2Θ*(c311 - 3.0*c111)
		  Var ddxdΦ As Double = dxc1*cΘ*(18.0*s012 - 2.0*s412) - dxc2*sΘ*(c311 + 3.0*c111)
		  Var fp1 As Double = cos2ψ*dp - sin2ψ*dx
		  Var fx1 As Double = sin2ψ*dp + cos2ψ*dx
		  Var dfp1dΘ As Double = cos2ψ*ddpdΘ - sin2ψ*ddxdΘ
		  Var dfx1dΘ As Double = sin2ψ*ddpdΘ + cos2ψ*ddxdΘ
		  Var dfp1dΦ As Double = cos2ψ*ddpdΦ - sin2ψ*ddxdΦ
		  Var dfx1dΦ As Double = sin2ψ*ddpdΦ + cos2ψ*ddxdΦ
		  
		  // repeat the whole thing again for detector 2
		  Var fp2 As Double
		  Var fx2 As Double
		  Var dfp2dΘ As Double
		  Var dfx2dΘ As Double
		  Var dfp2dΦ As Double
		  Var dfx2dΦ As Double
		  If Parameters.Detectors = 2 Then
		    // Note that if we don't have 2 detectors, then the variables above will all be zero.
		    s210 = Sin(2.0*ρ - σ2)
		    s012 = Sin(σ2 - 2.0*Parameters.Φ)
		    s412 = Sin(4.0*ρ - σ2 - 2.0*Parameters.Φ)
		    s311 = Sin(3.0*ρ - σ2 - Parameters.Φ)
		    s111 = Sin(ρ - σ2 - Parameters.Φ)
		    c012 = Cos(σ2 - 2.0*Parameters.Φ)
		    c412 = Cos(4.0*ρ - σ2 -2.0*Parameters.Φ)
		    c311 = Cos(3.0*ρ - σ2 - Parameters.Φ)
		    c111 = Cos(ρ - σ2 + Parameters.Φ)
		    
		    dp = dpc1*(-6.0*s210 + 9.0*s012 - s412) + dpc2*c2Θ*(18.0*s210 + 9.0*s012 - s412) - dpc3*s2Θ*(s311 - 3.0*s111)
		    dx = dxc1*cΘ*(9.0*c012 - c412) - dxc2*sΘ*(s311 - 3.0*s111)
		    ddpdΘ = -2.0*dpc2*s2Θ*(18.0*s210 + 9.0*s012 - s412) - 2.0*dpc3*c2Θ*(s311 - 3.0*s111)
		    ddxdΘ = -dxc1*sΘ*(9.0*c012 - c412) - dxc2*cΘ*(s311 - 3.0*s111)
		    ddpdΦ = dpc1*(-18.0*c012 + 2.0*c412) + dpc2*c2Θ*(-18.0*c012 + 2.0*c412) + dpc3*s2Θ*(c311 - 3.0*c111)
		    ddxdΦ = dxc1*cΘ*(18.0*s012 - 2.0*s412) - dxc2*sΘ*(c311 + 3.0*c111)
		    fp2 = cos2ψ*dp - sin2ψ*dx
		    fx2 = sin2ψ*dp + cos2ψ*dx
		    dfp2dΘ = cos2ψ*ddpdΘ - sin2ψ*ddxdΘ
		    dfx2dΘ = sin2ψ*ddpdΘ + cos2ψ*ddxdΘ
		    dfp2dΦ = cos2ψ*ddpdΦ - sin2ψ*ddxdΦ
		    dfx2dΦ = sin2ψ*ddpdΦ + cos2ψ*ddxdΦ
		  End If
		  Var fp As Double = fp1 + fp2
		  Var fx As Double = fx1 + fx2
		  
		  '// Assemble the base case situation
		  'ΨrDP = ΨrDN // store the current received phase value as the past value before we update
		  'ΨDP = ΨDN // and the same for the source phase
		  'DΨrDΘDP = DΨrDΘDN  // and the same for the phase derivatives
		  'DΨrDΦDP = DΨrDΦDN
		  'GetDataAtDetectorStep(SourceEvolverBase) // get the data from the base case at the present step
		  '// now we need to update the phase. The method just above will get ΨDN
		  '// note that we have set up τrDN and τrDP in t DidDetectorStepOK method)he
		  'Var orbitArg As Double = Parameters.GMΩe*0.5*(τrDN + τrDP) - Parameters.Φ
		  'ΨrDN = ΨrDP + (1.0 + VeSinΘ*Sin(orbitArg))*(ΨDN - ΨDP)  // update the received phase to the present
		  'DΨrDΘDN = DΨrDΘDP + VeCosΘ*Sin(orbitArg)*(ΨDN - ΨDP)  // update the derivative with respect to Θ
		  'DΨrDΦDN = DΨrDΦDP - VeSinΘ*Cos(orbitArg)*(ΨDN - ΨDP)  // update the derivative with respect to Φ
		  '// Now that we have the current value of the received phase, we can calculate the wave factors, amplitudes, and assemble the wave
		  'CalculateWaveFactors
		  'CalculateAmplitudes
		  'SumSourceH(W)  // this will put the total plus and  cross polarizations into HP and HX
		  '
		  '// Store valuable variables for later use
		  'Var hpBase As Double = HP
		  'Var hxBase As Double = HX
		  'Var hBase As Double = fp*HP + fx*HX
		  'SumSourceH(DWDα)
		  'Var dHDα As Double = fp*HP + fx*HX
		  'SumSourceH(DWDΨ)
		  'Var dHDΨr As Double = fp*HP + fx*HX
		  '// This gets the derivative of the amplitude part of the wave with respect to V
		  'SumSourceH(W, True)
		  'Var dHDV As Double = fp*HP + fx*HX
		  '
		  '// Calculate the derivative with respect to M (this is the easy one!)
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.M)) Then
		  'DHDq(Integer(CaseInfoClass.Param.M)) = hBase
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.M)) = 0.0
		  'End If
		  '
		  '// Calculate the derivative with respect to ψ (this is the next easiest!)
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.psi)) Then
		  'DHDq(Integer(CaseInfoClass.Param.psi)) = 2.0*(-fx*hpBase + fp*hxBase)
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.psi)) = 0.0
		  'End If
		  '
		  '// in the case of λ0, DΨrDλ0 = 1, so the following is the correct total derivative.
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.lambda0)) Then
		  'DHDq(Integer(CaseInfoClass.Param.lambda0)) = dHDΨr
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.lambda0)) = 0.0
		  'End If
		  '
		  '// We can also use the above items to calculate the derivative with respect to Θ
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.theta)) Then
		  'DHDq(Integer(CaseInfoClass.Param.theta)) = dfp1dΘ*hpBase + dfp2dΘ*hpBase + dfx1dΘ*hxBase + dfx2dΘ*hxBase _
		  '+ dHDΨr*DΨrDΘDN
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.theta)) = 0.0
		  'End If
		  '
		  '// and the derivative with respect to Φ
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.phi)) Then
		  'DHDq(Integer(CaseInfoClass.Param.phi)) = dfp1dΦ*hpBase + dfp2dΦ*hpBase + dfx1dΦ*hxBase + dfx2dΦ*hxBase _
		  '+ dHDΨr*DΨrDΦDN
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.phi)) = 0.0
		  'End If
		  '
		  '// Now we will start calculating derivatives that involve derivatives of the wave amplitudes
		  '
		  'Var originalValue As Double
		  'Var originalValue2 As Double
		  'Var hPlus As Double
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.beta)) Then
		  '// First, calculate the derivative with respect to β
		  'originalValue = Cosβ // Store these values for safekeeping
		  'originalValue2 = Sinβ
		  'Cosβ = CosβPlus  // Reset their values to the plus tweaked versions
		  'Sinβ = SinβPlus
		  'CalculateAmplitudes  // calculate amplitudes using these tweaked values
		  'SumSourceH(W)  // and calculate the waves with these amplitudes
		  'hPlus= fp*HP + fx*HX  // save the results for later
		  'Cosβ = CosβMinus  // now reset the values of Cosβ, Sinβ to the minus tweaked version
		  'Sinβ = SinβMinus
		  'CalculateAmplitudes // calculate the amplitudes using these tweaked values
		  'SumSourceH(W) // and calculate the waves
		  'Cosβ = originalValue  // restore the original values of of Cosβ, Sinβ, so that no harm is done
		  'Sinβ = originalValue2
		  'DHDq(Integer(CaseInfoClass.Param.beta)) = (hPlus - fp*HP - fx*HX)*IDεForβ  // This gives us the complete β-derivative
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.beta)) = 0.0
		  'End If
		  '
		  '// Most of the remaining parameters require all or nearly all the following amplitude derivatives
		  '// because varying the parameters have either implicit or explicit effects on the quantities that
		  '// the wave amplitudes depend on.
		  '
		  '// Calculate amplitude derivative with respect to ι
		  'Var ε As Double = 1.0e-5
		  'originalValue = ιDN
		  'ιDN = ιDN + ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'hPlus = fp*HP + fx*HX
		  'ιDN = originalValue - ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'ιDN = originalValue
		  'Var dHDι As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  '
		  '// Calculate amplitude derivative with respect to δ
		  'ε = 1.0e-5  // (one can reset this for individual cases if desired without affecting others)
		  'originalValue = δ
		  'originalValue2 = η
		  'δ = δ + ε
		  'η = 0.25*(1.0-δ*δ)
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'hPlus = fp*HP + fx*HX
		  'δ = originalValue - ε
		  'η = 0.25*(1.0-δ*δ)
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'δ = originalValue
		  'η = originalValue2
		  'Var dHDδ As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  '
		  '// Calculate amplitude derivative with respect to χax
		  'ε = 1.0e-5
		  'originalValue = χaxDN
		  'χaxDN = χaxDN + ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'hPlus = fp*HP + fx*HX
		  'χaxDN = originalValue - ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'χaxDN = originalValue
		  'Var dHDχax As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  '
		  '// Calculate amplitude derivative with respect to χay
		  'ε = 1.0e-5
		  'originalValue = χayDN
		  'χayDN = χayDN + ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'hPlus = fp*HP + fx*HX
		  'χayDN = originalValue - ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'χayDN = originalValue
		  'Var dHDχay As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  '
		  '// Calculate amplitude derivative with respect to χaz
		  'ε = 1.0e-5
		  'originalValue = χazDN
		  'χazDN = χazDN + ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'hPlus = fp*HP + fx*HX
		  'χazDN = originalValue - ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'χazDN = originalValue
		  'Var dHDχaz As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  '
		  '// Calculate amplitude derivative with respect to χsx
		  'ε = 1.0e-5
		  'originalValue = χsxDN
		  'χsxDN = χsxDN + ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'hPlus = fp*HP + fx*HX
		  'χsxDN = originalValue - ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'χsxDN = originalValue
		  'Var dHDχsx As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  '
		  '// Calculate amplitude derivative with respect to χsy
		  'ε = 1.0e-5
		  'originalValue = χsyDN
		  'χsyDN = χsyDN + ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'hPlus = fp*HP + fx*HX
		  'χsyDN = originalValue - ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'χsyDN = originalValue
		  'Var dHDχsy As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  '
		  '// Calculate amplitude derivative with respect to χsz
		  'ε = 1.0e-5
		  'originalValue = χszDN
		  'χszDN = χszDN + ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'hPlus = fp*HP + fx*HX
		  'χszDN = originalValue - ε
		  'CalculateAmplitudes
		  'SumSourceH(W)
		  'χszDN = originalValue
		  'Var dHDχsz As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  '
		  '// We need to define a bunch of variables to be used later
		  'Var ιPlus As Double
		  'Var αPlus As Double
		  'Var ΨrPlus As Double
		  'Var VPlus As Double
		  'Var χaxPlus As Double
		  'Var χayPlus As Double
		  'Var χazPlus As Double
		  'Var χsxPlus As Double
		  'Var χsyPlus As Double
		  'Var χszPlus As Double
		  '// These variables will hold derivatives
		  'Var dαDq As Double
		  'Var dΨrDq As Double
		  'Var dVDq As Double 
		  'Var dιDq As Double
		  'Var dχaxDq As Double
		  'Var dχayDq As Double
		  'Var dχazDq As Double
		  'Var dχsxDq As Double
		  'Var dχsyDq As Double
		  'Var dχszDq As Double
		  '
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.R)) Then
		  '// Calculate the derivative with respect to q = lnR
		  'dαDq = -(αDN - α0)*Parameters.OneI1pZ*DZDlnR
		  'dΨrDq = -(ΨrDN - Parameters.λ0)*Parameters.OneI1pZ*DZDlnR
		  ''dVDq = -(VDN - Parameters.V0)*Parameters.OneI1pZ*DZDlnR
		  'dιDq = -(ιDN - ι0)*Parameters.OneI1pZ*DZDlnR
		  'dχaxDq = -(χaxDN - χax0)*Parameters.OneI1pZ*DZDlnR
		  'dχayDq = -(χayDN - χay0)*Parameters.OneI1pZ*DZDlnR
		  'dχazDq = -(χazDN - χaz0)*Parameters.OneI1pZ*DZDlnR
		  'dχsxDq = -(χsxDN - χsx0)*Parameters.OneI1pZ*DZDlnR
		  'dχsyDq = -(χsyDN - χsy0)*Parameters.OneI1pZ*DZDlnR
		  'dχszDq = -(χszDN - χsz0)*Parameters.OneI1pZ*DZDlnR
		  '
		  '// Now, we put it all together (The first term is actually the derivative of h0 with respect to lnR).
		  'DHDq(Integer(CaseInfoClass.Param.R))  = -hBase + dHDα*dαDq + dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  '+ dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  '+ dHDχsx*dχsxDq + dHDχsy*dχsyDq+ dHDχsz*dχszDq
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.R))  = 0.0
		  'End If
		  '
		  ''If Parameters.SolveFor(Integer(CaseInfoClass.Param.V0)) Then
		  ''// Calculate the derivative with respect to q = lnV0
		  ''GetDataAtDetectorStep(SourceEvolverV0Plus)
		  ''ιPlus = ιDN
		  ''αPlus = αDN
		  ''ΨrPlus = ΨrDN
		  ''VPlus = VDN
		  ''χaxPlus = χaxDN
		  ''χayPlus = χayDN
		  ''χazPlus = χazDN
		  ''χsxPlus = χsxDN
		  ''χsyPlus = χsyDN
		  ''χszPlus = χszDN
		  ''GetDataAtDetectorStep(SourceEvolverV0Minus)
		  ''dαDq = (αPlus - αDN)*IDεForV0
		  ''dΨrDq = (ΨrPlus - ΨrDN)*IDεForV0
		  ''dιDq = (ιPlus - ιDN)*IDεForV0
		  ''dVDq = (VPlus - VDN)*IDεForV0
		  ''dχaxDq = (χaxPlus - χaxDN)*IDεForV0
		  ''dχayDq = (χayPlus - χayDN)*IDεForV0
		  ''dχazDq = (χazPlus - χazDN)*IDεForV0
		  ''dχsxDq = (χsxPlus - χsxDN)*IDεForV0
		  ''dχsyDq = (χsyPlus - χsyDN)*IDεForV0
		  ''dχszDq = (χszPlus - χszDN)*IDεForV0
		  ''// Put it all together
		  ''DHDq(Integer(CaseInfoClass.Param.V0)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  ''+ dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  ''+ dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  ''Else
		  ''DHDq(Integer(CaseInfoClass.Param.V0)) = 0.0
		  ''End If
		  '
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.delta)) Then
		  '// Calculate the derivative with respect to q = δ
		  'GetDataAtDetectorStep(SourceEvolverδPlus)
		  'ιPlus = ιDN
		  'αPlus = αDN
		  'ΨrPlus = ΨrDN
		  'VPlus = VDN
		  'χaxPlus = χaxDN
		  'χayPlus = χayDN
		  'χazPlus = χazDN
		  'χsxPlus = χsxDN
		  'χsyPlus = χsyDN
		  'χszPlus = χszDN
		  'GetDataAtDetectorStep(SourceEvolverδMinus)
		  'dαDq = (αPlus - αDN)*IDεForδ
		  'dΨrDq = (ΨrPlus - ΨrDN)*IDεForδ
		  'dVDq = (VPlus - VDN)*IDεForδ
		  'dιDq = (ιPlus - ιDN)*IDεForδ
		  'dχaxDq = (χaxPlus - χaxDN)*IDεForδ
		  'dχayDq = (χayPlus - χayDN)*IDεForδ
		  'dχazDq = (χazPlus - χazDN)*IDεForδ
		  'dχsxDq = (χsxPlus - χsxDN)*IDεForδ
		  'dχsyDq = (χsyPlus - χsyDN)*IDεForδ
		  'dχszDq = (χszPlus - χszDN)*IDεForδ
		  '// Put it all together
		  'DHDq(Integer(CaseInfoClass.Param.delta)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  '+ dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  '+ dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq + dHDδ
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.delta)) = 0.0
		  'End If
		  '
		  ''// Code to display values
		  ''Var ιPlusStr as String = Format(ιPlus, "0.00000000000000e+00")
		  ''Var αPlusStr as String = Format(αPlus, "0.00000000000000e+00")
		  ''Var ΨrPlusStr as String = Format(ΨrPlus, "0.00000000000000e+00")
		  ''Var VPlusStr as String = Format(VPlus, "0.00000000000000e+00")
		  ''Var ιDNStr as String = Format(ιDN, "0.00000000000000e+00")
		  ''Var αDNStr as String = Format(αDN, "0.00000000000000e+00")
		  ''Var ΨrDNStr as String = Format(ΨrDN, "0.00000000000000e+00")
		  ''Var VDNStr as String = Format(VDN, "0.00000000000000e+00")
		  ''Var dHDιStr as String = Format(dHDι, "0.00000000000000e+00")
		  ''Var dHDαStr as String = Format(dHDα, "0.00000000000000e+00")
		  ''Var dHDΨrStr as String = Format(dHDΨr, "0.00000000000000e+00")
		  ''Var dHDVStr as String = Format(dHDV, "0.00000000000000e+00")
		  ''Var dHDM1Str as String = Format(DHDq(Integer(Item.M1)), "0.00000000000000e+00")
		  '
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi10x)) Then
		  '// Calculate the derivative with respect to q = χ10x
		  'GetDataAtDetectorStep(SourceEvolverχ10xPlus)
		  'ιPlus = ιDN
		  'αPlus = αDN
		  'ΨrPlus = ΨrDN
		  'VPlus = VDN
		  'χaxPlus = χaxDN
		  'χayPlus = χayDN
		  'χazPlus = χazDN
		  'χsxPlus = χsxDN
		  'χsyPlus = χsyDN
		  'χszPlus = χszDN
		  'GetDataAtDetectorStep(SourceEvolverχ10xMinus)
		  'dαDq = (αPlus - αDN)*IDεForχ10x
		  'dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ10x
		  'dVDq = (VPlus - VDN)*IDεForχ10x
		  'dιDq = (ιPlus - ιDN)*IDεForχ10x
		  'dχaxDq = (χaxPlus - χaxDN)*IDεForχ10x
		  'dχayDq = (χayPlus - χayDN)*IDεForχ10x
		  'dχazDq = (χazPlus - χazDN)*IDεForχ10x
		  'dχsxDq = (χsxPlus - χsxDN)*IDεForχ10x
		  'dχsyDq = (χsyPlus - χsyDN)*IDεForχ10x
		  'dχszDq = (χszPlus - χszDN)*IDεForχ10x
		  '// Put it all together
		  'DHDq(Integer(CaseInfoClass.Param.chi10x)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  '+ dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  '+ dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.chi10x)) = 0.0
		  'End If
		  '
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi10y)) Then
		  '// Calculate the derivative with respect to q = χ10y
		  'GetDataAtDetectorStep(SourceEvolverχ10yPlus)
		  'ιPlus = ιDN
		  'αPlus = αDN
		  'ΨrPlus = ΨrDN
		  'VPlus = VDN
		  'χaxPlus = χaxDN
		  'χayPlus = χayDN
		  'χazPlus = χazDN
		  'χsxPlus = χsxDN
		  'χsyPlus = χsyDN
		  'χszPlus = χszDN
		  'GetDataAtDetectorStep(SourceEvolverχ10yMinus)
		  'dαDq = (αPlus - αDN)*IDεForχ10y
		  'dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ10y
		  'dVDq = (VPlus - VDN)*IDεForχ10y
		  'dιDq = (ιPlus - ιDN)*IDεForχ10y
		  'dχaxDq = (χaxPlus - χaxDN)*IDεForχ10y
		  'dχayDq = (χayPlus - χayDN)*IDεForχ10y
		  'dχazDq = (χazPlus - χazDN)*IDεForχ10y
		  'dχsxDq = (χsxPlus - χsxDN)*IDεForχ10y
		  'dχsyDq = (χsyPlus - χsyDN)*IDεForχ10y
		  'dχszDq = (χszPlus - χszDN)*IDεForχ10y
		  '// Put it all together
		  'DHDq(Integer(CaseInfoClass.Param.chi10y)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  '+ dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  '+ dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.chi10y)) = 0.0
		  'End If
		  '
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi10z)) Then
		  '// Calculate the derivative with respect to q = χ10z
		  'GetDataAtDetectorStep(SourceEvolverχ10zPlus)
		  'ιPlus = ιDN
		  'αPlus = αDN
		  'ΨrPlus = ΨrDN
		  'VPlus = VDN
		  'χaxPlus = χaxDN
		  'χayPlus = χayDN
		  'χazPlus = χazDN
		  'χsxPlus = χsxDN
		  'χsyPlus = χsyDN
		  'χszPlus = χszDN
		  'GetDataAtDetectorStep(SourceEvolverχ10zMinus)
		  'dαDq = (αPlus - αDN)*IDεForχ10z
		  'dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ10z
		  'dVDq = (VPlus - VDN)*IDεForχ10z
		  'dιDq = (ιPlus - ιDN)*IDεForχ10z
		  'dχaxDq = (χaxPlus - χaxDN)*IDεForχ10z
		  'dχayDq = (χayPlus - χayDN)*IDεForχ10z
		  'dχazDq = (χazPlus - χazDN)*IDεForχ10z
		  'dχsxDq = (χsxPlus - χsxDN)*IDεForχ10z
		  'dχsyDq = (χsyPlus - χsyDN)*IDεForχ10z
		  'dχszDq = (χszPlus - χszDN)*IDεForχ10z
		  '// Put it all together
		  'DHDq(Integer(CaseInfoClass.Param.chi10z)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  '+ dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  '+ dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.chi10z)) = 0.0
		  'End If
		  '
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi20x)) Then
		  '// Calculate the derivative with respect to q = χ20x
		  'GetDataAtDetectorStep(SourceEvolverχ20xPlus)
		  'ιPlus = ιDN
		  'αPlus = αDN
		  'ΨrPlus = ΨrDN
		  'VPlus = VDN
		  'χaxPlus = χaxDN
		  'χayPlus = χayDN
		  'χazPlus = χazDN
		  'χsxPlus = χsxDN
		  'χsyPlus = χsyDN
		  'χszPlus = χszDN
		  'GetDataAtDetectorStep(SourceEvolverχ20xMinus)
		  'dαDq = (αPlus - αDN)*IDεForχ20x
		  'dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ20x
		  'dVDq = (VPlus - VDN)*IDεForχ20x
		  'dιDq = (ιPlus - ιDN)*IDεForχ20x
		  'dχaxDq = (χaxPlus - χaxDN)*IDεForχ20x
		  'dχayDq = (χayPlus - χayDN)*IDεForχ20x
		  'dχazDq = (χazPlus - χazDN)*IDεForχ20x
		  'dχsxDq = (χsxPlus - χsxDN)*IDεForχ20x
		  'dχsyDq = (χsyPlus - χsyDN)*IDεForχ20x
		  'dχszDq = (χszPlus - χszDN)*IDεForχ20x
		  '// Put it all together
		  'DHDq(Integer(CaseInfoClass.Param.chi20x)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  '+ dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  '+ dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.chi20x)) = 0.0
		  'End If
		  '
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi20y)) Then
		  '// Calculate the derivative with respect to q = χ20y
		  'GetDataAtDetectorStep(SourceEvolverχ20yPlus)
		  'ιPlus = ιDN
		  'αPlus = αDN
		  'ΨrPlus = ΨrDN
		  'VPlus = VDN
		  'χaxPlus = χaxDN
		  'χayPlus = χayDN
		  'χazPlus = χazDN
		  'χsxPlus = χsxDN
		  'χsyPlus = χsyDN
		  'χszPlus = χszDN
		  'GetDataAtDetectorStep(SourceEvolverχ20yMinus)
		  'dαDq = (αPlus - αDN)*IDεForχ20y
		  'dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ20y
		  'dVDq = (VPlus - VDN)*IDεForχ20y
		  'dιDq = (ιPlus - ιDN)*IDεForχ20y
		  'dχaxDq = (χaxPlus - χaxDN)*IDεForχ20y
		  'dχayDq = (χayPlus - χayDN)*IDεForχ20y
		  'dχazDq = (χazPlus - χazDN)*IDεForχ20y
		  'dχsxDq = (χsxPlus - χsxDN)*IDεForχ20y
		  'dχsyDq = (χsyPlus - χsyDN)*IDεForχ20y
		  'dχszDq = (χszPlus - χszDN)*IDεForχ20y
		  '// Put it all together
		  'DHDq(Integer(CaseInfoClass.Param.chi20y)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  '+ dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  '+ dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.chi20y)) = 0.0
		  'End If
		  '
		  'If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi20z)) Then
		  '// Calculate the derivative with respect to q = χ20z
		  'GetDataAtDetectorStep(SourceEvolverχ20zPlus)
		  'ιPlus = ιDN
		  'αPlus = αDN
		  'ΨrPlus = ΨrDN
		  'VPlus = VDN
		  'χaxPlus = χaxDN
		  'χayPlus = χayDN
		  'χazPlus = χazDN
		  'χsxPlus = χsxDN
		  'χsyPlus = χsyDN
		  'χszPlus = χszDN
		  'GetDataAtDetectorStep(SourceEvolverχ20zMinus)
		  'dαDq = (αPlus - αDN)*IDεForχ20z
		  'dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ20z
		  'dVDq = (VPlus - VDN)*IDεForχ20z
		  'dιDq = (ιPlus - ιDN)*IDεForχ20z
		  'dχaxDq = (χaxPlus - χaxDN)*IDεForχ20z
		  'dχayDq = (χayPlus - χayDN)*IDεForχ20z
		  'dχazDq = (χazPlus - χazDN)*IDεForχ20z
		  'dχsxDq = (χsxPlus - χsxDN)*IDεForχ20z
		  'dχsyDq = (χsyPlus - χsyDN)*IDεForχ20z
		  'dχszDq = (χszPlus - χszDN)*IDεForχ20z
		  '// Put it all together
		  'DHDq(Integer(CaseInfoClass.Param.chi20z)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  '+ dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  '+ dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  'Else
		  'DHDq(Integer(CaseInfoClass.Param.chi20z)) = 0.0
		  'End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateAmplitudes()
		  // Now calculate all wave amplitudes
		  
		  // Calculate some useful trig functions of angle ι
		  Var c2 As Double = Cos(ιDN)
		  Var s2 As Double = Sin(ιDN)
		  Var c1 As Double = Cos(0.5*ιDN)
		  Var s1 As Double = Sin(0.5*ιDN)
		  Var c3 As Double = c2*c1 - s2*s1
		  Var s3 As Double = s2*c1 + c2*s1
		  Var c4 As Double = c2*c2-s2*s2
		  Var s4 As Double = 2*c2*s2
		  Var c5 As Double = c4*c1 - s4*s1
		  Var s5 As Double = s4*c1 + c4*s1
		  Var c6 As Double = c5*c1 - s5*s1
		  Var s6 As Double = s5*c1 + c5*s1
		  Var c7 As Double = c6*c1 - s6*s1
		  Var s7 As Double = s6*c1 + c6*s1
		  Var c8 As Double = c7*c1 - s7*s1
		  Var s8 As Double = s7*c1 + c7*s1
		  Var c9 As Double = c8*c1 - s8*s1
		  Var s9 As Double = s8*c1 + c8*s1
		  Var c10 As Double = c9*c1 - s9*s1
		  Var s10 As Double = s9*c1 + c9*s1
		  Var c1p2 As Double = c1*c1
		  Var c1p3 As Double = c1p2*c1
		  Var c1p4 As Double = c1p3*c1
		  Var c1p5 As Double = c1p4*c1
		  Var c1p6 As Double = c1p5*c1
		  Var c1p7 As Double = c1p6*c1
		  Var c1p8 As Double = c1p7*c1
		  Var c1p9 As Double = c1p8*c1
		  Var c1p10 As Double = c1p9*c1
		  Var s1p2 As Double = s1*s1
		  Var s1p3 As Double = s2*s1
		  Var s1p4 As Double = s3*s1
		  Var s1p5 As Double = s4*s1
		  Var s1p6 As Double = s5*s1
		  Var s1p7 As Double = s6*s1
		  Var s1p8 As Double = s7*s1
		  Var s1p9 As Double = s8*s1
		  Var s1p10 As Double = s9*s1
		  Var c2p2 As Double = c2*c2
		  Var c2p3 As Double = c2p2*c2
		  Var c2p4 As Double = c2p3*c2
		  Var s2p2 As Double = s2*s2
		  Var s2p3 As Double = s2p2*s2
		  Var s2p4 As Double = s2p3*s2
		  Var s2p5 As Double = s2p4*s2
		  
		  // Define local β trig functions
		  Var cβ As Double = Cosβ
		  Var sβ As Double = Sinβ
		  Var c2β As Double = cβ*cβ - sβ*sβ
		  Var s2β As Double = 2*sβ*cβ
		  Var c3β As Double = c2β*cβ - s2β*sβ
		  Var s3β As Double = s2β*cβ + c2β*sβ
		  Var c4β As Double = c3β*cβ - s3β*sβ
		  Var s4β As Double = s3β*cβ + c3β*sβ
		  Var c5β As Double = c4β*cβ - s4β*sβ
		  Var s5β As Double = s4β*cβ + c4β*sβ
		  Var cβ2 As Double = cβ*cβ
		  Var cβ3 As Double = c2β*cβ
		  Var sβ2 As Double = sβ*sβ
		  Var sβ3 As Double = s2β*sβ
		  
		  // Amplitudes for H0P
		  A(0) = (-1.5 - 0.5*c2β)*c1p4
		  A(1) = -2.0*c1p3*s2β*s1
		  A(2) =  2.0*s1p3*s2β*c1
		  A(3) = (-1.5 - 0.5*c2β)*s1p4
		  A(4) = -1.5*sβ2*s2p2
		  
		  // Amplitudes for H0X
		  A(129) = 4.0*sβ*c1*s1p3
		  A(130) = -2.0*cβ*s1p4
		  A(131) =  -4.0*sβ*c1p3*s1
		  A(132) =  -2.0*cβ*c1p4
		  
		  If Parameters.PNOrder > 0 Then
		    
		    // Amplitude factors for H1P
		    
		    A(5) = δ*c1p6*(-45/32 *sβ - 9/32 * s3β)
		    A(6) = δ*c1p2*(-175/256 *sβ + c2*(87/64 *sβ - 5/64 * s3β) + c4*(-5/256 * sβ + 15/256 * s3β) + 13/256 *s3β)
		    A(7) = δ*s1p2*(175/256 *sβ + c2*(87/64 *sβ - 5/64 * s3β) + c4*(5/256 * sβ - 15/256 * s3β) - 13/256 *s3β)
		    A(8) = δ*c1p4*s1p2*(-5/32 *sβ - 1/32 *s3β)
		    A(9) = δ*c1p4*s1p2*(-45/32 *sβ + 135/32 *s3β)
		    A(10) = δ*c1p2*s1p4*(45/32 *sβ - 135/32 *s3β)
		    A(11) = δ*c1p2*s1p4*(5/32 *sβ + 1/32 *s3β)
		    A(12) = δ*s1p6*sβ*(27/16 + 9/16 *c2β)
		    A(13) = δ*45/16 * s2p3*cβ*sβ2
		    A(14) = δ*((-85/256 *cβ - 1/128 *cβ*c2β - 1/32 *cβ*c2β*c2 - 3/128 *cβ*c2β*c4)*s2 - 11/64*cβ*s4 - 1/256*cβ*s6)
		    A(15) = δ*((45/256 *cβ + 81/128*cβ*c2β + 27/32 *cβ*c2β*c2 + 27/128 *cβ*c2β*c4)*s2 + 9/64*cβ*s4 + 9/256 *cβ*s6)
		    A(16) = δ*((1/256 * cβ*c2β - 85/256 *cβ)*s2 + (11/64 * cβ + 1/64 *cβ*c2β)*s4 - (1/256 *cβ + 3/256 *cβ*c2β)*s6)
		    A(17) = δ*((45/256 *cβ + 135/256 *cβ*c2β)*s2 - (9/64 *cβ + 27/64 *cβ*c2β)*s4 + (9/256 *cβ + 27/256 *cβ*c2β)*s6)
		    A(18) = δ*(1/64 *cβ*sβ2*s2 + 5/64 *cβ*sβ2*s6)
		    
		    // Ampitude factors for H1X
		    
		    A(136) = δ*(-1/64 *cβ*sβ + 43/128 *cβ*c2*sβ - 23/128 *c4*s2β + 5/256 *c6*s2β)
		    A(137) = δ*((-1 -1/4 *c2β)*c1 + 1/4 *c2β*c1*c2)*s1p3
		    A(138) = δ*(1/8 *c1p2*s2β*s1p4)
		    A(139) = δ*(1/2 *sβ2*s4)
		    A(140) = δ*(1/64 *cβ*sβ + 43/128 *cβ*c2*sβ + 23/128 *c4*s2β + 5/256 *c6*s2β)
		    A(141) = δ*((-1 - 1/4 *c2β)*c1p3 - 1/4 *c2β*c1p3*c2)*s1
		    A(142) = -δ*(1/8 *c1p4*s2β*s1p2)
		    A(143) = δ*(45/8 *c1p4*s2β*s1p2)
		    A(144) = δ*(9/2 *c2β*c1p5*s1)
		    A(145) = -δ*(9/8 *c1p6*s2β)
		    A(146) = (4*sβ + 28/3 *s3β - η*(12*sβ + 28*s3β))*c1p3*s1p5
		    A(147) = (η*(4*cβ + 28*c3β - (4/3 *cβ + 28/3 *c3β)))*c1p2*s1p6
		    A(148) = ((4/3 *sβ - 4*s3β) + η*(-4*sβ + 12*s3β))*c1*s1p7
		    
		  End If
		  If Parameters.PNOrder > 1 Then
		    
		    // Amplitude factors for H2P
		    
		    A(19) = (59/16 + 5/2 *c2β - 3/16 * c4β + (5/24 - 11/6 * c2β + 7/24 *c4β)*c2 - (5/48 + 1/12 *c2β + 7/48 *c4β)*c4)*c1p4 _
		    + (-25/16 - 13/3 *c2β + 9/16 *c4β + (-5/8 + 11/2 *c2β - 7/8 *c4β)*c2 + (5/16 + 1/4 *c2β + 7/16 *c4β)*c4)*η*c1p4
		    A(20) = (6 + 2*c2β)*η*c1p8*sβ2 - (2 + 2/3 *c2β)*c1p8*sβ2
		    A(21) = 32*(1/3 - η)*cβ3*c1p7*sβ*s1
		    A(22) = ((1/6 *c2β - 5/6)*s2β - 2/3 *cβ2*c2*s2β + η*((5/2 - 1/2 *c2β)*s2β + 2 *cβ2*c2*s2β))*c1p5*s1
		    A(23) = (-(10/3 + 8/3 *c2β + 14/3 *c4β) + η*(10 + 8 *c2β + 14*c4β))*c1p6*s1p2
		    A(24) = 1/2*(-(1 + 1/3 *c2β) + η*(3 + c2β))*c1p6*sβ2*s1p2
		    A(25) = (8/3 - 56/3 *c2β + η*(56*c2β - 8))*c1p5*s2β*s1p3
		    A(26) = η*(c1*(16/3 *s2β + 31/4 *c2*s2β + 1/4 *c4*s2β - 19/16 *s4β) - 7/8*c3*s4β - 7/16*c5*s4β)*s1p3 _
		    +(c1*(-6*s2β - 31/12*c2*s2β - 1/12*c3*s2β + 19/48*s4β) + 7/24*c3*s4β + 7/48*c5*s4β)*s1p3            
		    // double check
		    A(27) = (59/16 + 5/2 *c2β - 3/16 *c4β - (5/24 - 11/6 *c2β + 7/24 *c4β)*c2 - (5/48 + 1/12 *c2β + 7/48 *c4β)*c4)*s1p4 _
		    + η*(-25/16 - 13/3 *c2β + 9/16 *c4β + (5/8 - 11/2*c2β + 7/8*c4β)*c2 + (5/16 + 1/4*c2β + 7/16 *c4β)*c4)*s1p4
		    A(28) = (56/3 *c2β - 8/3 + η*(8 - 56*c2β))*c1p3*s2β*s1p5
		    // Interesting "space between number and ["
		    A(29) = ((5/6 - 1/6 *c2β)*s2β - 2/3*cβ2*c2*s2β + η*((-5/2 + 1/2*c2β)*s2β + 2*cβ2*c2*s2β))*c1*s1p5
		    A(30) = (-(10/3 + 8/3 *c2β + 14/3 *c4β) + η*(10 + 8*c2β + 14*c4β))*c1p2*s1p6
		    A(31) = (-(1/2 + 1/6 *c2β) + η*(3/2 + 1/2 *c2β))*c1p2*sβ2*s1p6
		    A(32) = 32*(η - 1/3)*cβ3*c1*sβ*s1p7
		    A(33) = (η*(6 + 2 *c2β) - (2 + 2/3 *c2β))*sβ2*s1p8
		    A(34) = 1/32*(1/3*(349 - 25 *c2β)*sβ2 - (25 + 35*c2β)*c4*sβ2) + η*((25*c2β - 45)*sβ2 + (25 + 35*c2β)*c4*sβ2)*s2p2
		    A(35) = 1/4*(η*(25 + 35 *c2β) - 1/3 *(25 - 35 *c2β))*sβ2*s2p4
		    A(36) = c1p3*(6 *s2β - 31/12 *c2*s2β + 1/12 *c4*s2β - 19/48 *s4β)*s1 + 7/24 *c1p3*s4β*s3 - 7/48 *c1p3*s4β*s5 _
		    + η*(c1p3*(-16/3 *s2β + 31/4 *c2*s2β - 1/4 *c4*s2β + 19/16 *s4β)*s1 - 7/8 *c1p3*s4β*s3 + 7/16 *c1p3*s4β*s5)
		    
		    // Subscripts
		    A(37) = χaxDN*cβ*c1p2 - χazDN*c1p2*sβ
		    A(38) = χaxDN*(cβ/2 - cβ/2 *c2) - χazDN*sβ*s1p2
		    A(39) = -χayDN*cβ*s1p2
		    A(40) = -χayDN*sβ*s2
		    A(41) = -χayDN*cβ*c1p2
		    A(42) = δ*(χsxDN*cβ*c1p2 - χszDN*c1p2*sβ)
		    A(43) = δ*(χsxDN*(cβ/2 - cβ/2 *c2) - χszDN*sβ*s1p2)
		    A(44) = -δ*(χsyDN*cβ*s1p2)
		    A(45) = -δ*(χsyDN*sβ*s2)
		    A(46) = -δ*(χsyDN*cβ*c1p2)
		    
		    //Amplitude factors for H2X
		    
		    A(146) = (4*sβ + 28/3 *s3β - η*(12*sβ + 28*s3β))*c1p3*s1p5
		    A(147) = (η*(4*cβ + 28*c3β - (4/3 *cβ + 28/3 *c3β)))*c1p2*s1p6
		    A(148) = ((4/3 *sβ - 4*s3β) + η*(-4*sβ + 12*s3β))*c1*s1p7
		    A(149) = (8*η - 8/3)*cβ*sβ*s1p8
		    A(150) = c1*(-79/8 *sβ + c2*(3/4 *sβ - 19/12 *s3β) + c4*(1/8 *sβ + 7/24 *s3β) - 3/8 *s3β)*s1p3 _
		    + η*c1*(103/24 *sβ - c4*(3/8 *sβ + 7/8 *s3β) + 9/8 *s3β + c2*(-9/4 *sβ + 19/4 *s3β))*s1p3
		    A(151) = (47/8 *cβ + 1/8 *c3β + (7/6 *cβ + 1/6 *c3β)*c2 - (1/24 *cβ + 7/24 *c3β)*c4 _
		    + η*(-119/24 *cβ - 3/8 *c3β - (7/2 *cβ + 1/2 *c3β)*c2 + (1/8 *cβ + 7/8 *c3β)*c4))*s1p4
		    A(152) = (4/3 *sβ - (1/3 + c2β)*c2*sβ + η*(-4*sβ + (1 + 3*c2β)*c2*sβ))*c1*s1p5
		    A(153) = (2*η - 2/3)*cβ*c1p2*sβ2*s1p6
		    A(154) = (15/2 *η - 5/2)*cβ*c2*sβ2*s2p2
		    A(155) = c1p3*(79/8 *sβ + c2*(3/4 *sβ - 19/12 *s3β) - c4*(1/8 *sβ + 7/24 *s3β) + 3/8 *s3β)*s1 _
		    + η*c1p3*(-103/24 *sβ + c4*(3/8 *sβ + 7/8 *s3β) - 9/8 *s3β + c2*(-9/4 *sβ + 19/4 *c3β))*s1
		    A(156) = c1p4*(47/8 *cβ + 1/8 *c3β - (7/6 *cβ + 1/6 *c3β)*c2 - (1/24 *cβ + 7/24 *c3β)*c4) _
		    + η*c1p4*(-119/24 *cβ - 3/8 *c3β + (7/2 *cβ + 1/2 *c3β)*c2 + (1/8 *cβ + 7/8 *c3β)*c4)
		    A(157) = (-4/3 *sβ - (1/3 + c2β)*c2*sβ + η*(4*sβ + (1 + 3*c2β)*c2*sβ))*c1p5*s1
		    A(158) = (2*η - 2/3)*cβ*c1p6*sβ2*s1p2
		    A(159) = (η*(12*sβ + 28*s3β) - (4*sβ + 28/3 *s3β))*c1p5*s1p3
		    A(160) = (η*(4*cβ + 28*c3β) - (4/3 *cβ + 28/3 *c3β))*c1p6*s1p2
		    A(161) = (8/3 + 8*c2β - η*(8 + 24*c2β))*c1p7*sβ*s1
		    A(162) = (8*η - 8/3)*cβ*c1p8*sβ2
		    
		    //Subscripts
		    
		    A(163) = χayDN*(1/2 + 1/2 *c2)
		    A(164) = χayDN*s1p2
		    A(165) = χaxDN*(1/2 *cβ2 - 1/2 *cβ2*c2) + χazDN*(-1/2 *cβ*sβ + 1/2 *cβ*c2*sβ)
		    A(166) = χaxDN*cβ*sβ*s2 - χazDN*sβ2*s2
		    A(167) = χaxDN*(1/2 *cβ2 + 1/2 *cβ2*c2) + χazDN*(-1/2 *cβ*sβ - 1/2 *cβ*c2*sβ)
		    A(168) = δ*(χsyDN*(1/2 + 1/2 *c2))
		    A(169) = δ*(χsyDN*s1p2)
		    A(170) = δ*(χsxDN*(1/2 *cβ2 - 1/2 *cβ2*c2) + χszDN*(-1/2 *cβ*sβ + 1/2 *cβ*c2*sβ))
		    A(171) = δ*(χsxDN*cβ*sβ*s2 - χszDN*sβ2*s2)
		    A(172) = δ*(χsxDN*(1/2 *cβ2 + 1/2 *cβ2*c2) + χszDN*(-1/2 *cβ*sβ - 1/2 *cβ*c2*sβ))
		    
		  End If
		  
		  If Parameters.PNOrder > 2 then
		    
		    // Ampitude factors for H3P
		    
		    A(47) = -(3*π + π*c2β)*c1p4
		    A(48) = -4*π*c1p3*s2β*s1
		    A(49) = 4*π*c1*s2β*s1p3
		    A(50) = -(3*π + π*c2β)*s1p4
		    A(51) = -3*π*sβ2*s2p2
		    A(52) = δ*(η*(625/128 + 625/384 *c2β) - (625/256 + 625/768 *c2β))*c1p10*sβ3
		    // I simplified the powers, is that okay?
		    A(53) = δ*(η*c1p2*( -7449/16384 *sβ - 331/32768 *s3β + c4*(337/12288 *sβ - 47/8192 *s3β - 21/8192 *s5β) _
		    + c8*(7/49152 *sβ + 7/32768 *s3β - 35/32768 *s5β) + c6*(-59/6144 *sβ - 91/4096 *s3β + 7/4096 *s5β) _
		    + c2*(1873/2048 *sβ + 19/4096 *s3β + 35/12288 *s5β) - 155/98304 *s5β) + c1p2*(43723/98304 *sβ _
		    - 9653/65536 *s3β + c2*(-10675/12288 *sβ + 1901/8192 *s3β - 35/24576 *s5β)  + c6*(59/12288 *sβ _
		    + 91/8192 *s3β - 7/8192 *s5β) + c8*(-7/98304 *sβ - 7/65536 *s3β + 35/65536 *s5β) + c4*(1103/24576 *sβ _
		    - 2833/16384 *s3β + 21/16384 *s5β) + 155/196608 *s5β))
		    A(54) = δ*(c1p6*(39249/8192 *sβ + 38331/16384 *s3β - c4*(1701/8192 *sβ + 3159/16384 *s3β + 3645/16384 *s5β) _
		    + c2*(2403/2048 *sβ - 6399/4096 *s3β + 2187/4096 *s5β) - 5751/16384 *s5β) + η*c1p6*(-4689/4096 *sβ _
		    - 24507/8192 *s3β + c2*(-2403/1024 *sβ + 6399/2048 *s3β - 2187/2048 *s5β) + c4*(1701/4096 *sβ _
		    + 3159/8192 *s3β + 3645/8192 *s5β) + 5751/8192 *s5β))
		    A(55) = δ*((11875/768 *cβ + 3125/768 *c3β - η*(11875/384 *cβ + 3125/384 *c3β))*c1p9*sβ2*s1)
		    A(56) = δ*(((-351/256 *cβ + 243/256 *cβ*c2β)*sβ2 - (567/256 *cβ + 405/256 *cβ*c2β)*c2*sβ2 _
		    + η*((351/128 *cβ - 243/128 *cβ*c2β)*sβ2 + (567/128 *cβ + 405/128 *cβ*c2β)*c2*sβ2))*c1p7*s1)
		    A(57) = δ*((η*(243/128 + 81/128 *c2β) - (243/256 + 81/256 *c2β))*c1p8*sβ3*s1p2)
		    A(58) = δ*((-43723/98304 *sβ + 9653/65536 *s3β + c2*(-10675/12288 *sβ + 1901/8192 *s3β _
		    - 35/24576 *s5β) + c4*(-1103/24576 *sβ + 2833/16384 *s3β - 21/16384 *s5β) + c6*(59/12288 *sβ _
		    + 91/8192 *s3β - 7/8192 *s5β) + c8*(7/98304 *sβ + 7/65536 *s3β - 35/65536 *s5β) _
		    -155/196608 *s5β)*s1p2 + η*(7449/16384 *sβ + 331/32768 *s3β + c8*(-7/49152 *sβ _
		    - 7/32768 *s3β + 35/32768 *s5β) + c6*(-59/6144 *sβ - 91/4096 *s3β + 7/4096 *s5β) _
		    + c4*(-337/12288 *sβ + 47/8192 *s3β + 21/8192 *s5β) + c2*(1873/2048 *sβ _
		    + 19/4096 *s3β + 35/12288 *s5β) + 155/98304 *s5β)*s1p2)
		    A(59) = δ*(c1p4*(1675/4096 *sβ + 825/8192 *s3β - c4*(7/4096 *sβ + 13/8192 *s3β + 15/8192 *s5β) _
		    + c2*(27/1024 *sβ - 151/2048 *s3β + 3/2048 *s5β) - 13/8192 *s5β)*s1p2 + η*c1p4*(245/2048 *sβ _
		    - 57/4096 *s3β + c2*(-27/512 *sβ + 151/1024 *s3β - 3/1024 *s5β) + c4*(7/2048 *sβ + 13/4096 *s3β _
		    + 15/4096 *s5β) + 13/4096 *s5β)*s1p2)
		    A(60) = δ*((η*(4375/512 *sβ + 8125/1024 *s3β + 9375/1024 *s5β) - (4375/1024 *sβ _
		    + 8125/2048 *s3β + 9375/2048 *s5β))*c1p8*s1p2)
		    A(61) = δ*(c1p4*(20475/4096 *sβ - 149391/8192 *s3β + c2*(2187/1024 *sβ + 10017/2048 *s3β _
		    - 1701/2048 *s5β) + 7371/8192 *s5β + c4*(-567/4096 *sβ - 1701/8192 *s3β + 8505/8192 *s5β))*s1p2 _
		    + η*c1p4*(-3195/2048 *sβ + 45711/4096 *s3β + c4*(567/2048 *sβ + 1701/4096 *s3β - 8505/4096 *s5β) _
		    - 7371/4096 *s5β + c2*(-2187/512 *sβ - 10017/1024 *s3β + 1701/1024 *s5β))*s1p2)
		    A(62) = δ*((4375/384 *cβ + 625/256 *c3β + 3125/256 *c5β - η*(4375/192 *cβ + 625/128 *c3β + 3125/128 *c5β))*c1p7*s1p3)
		    A(63) = δ*(c1p5*((-37/384 *cβ + 1/384 *cβ*c2β)*sβ2 - (7/384 *cβ + 5/384 *cβ*c2β)*c2*sβ2)*s1p3 _
		    + η*c1p5*((37/192 *cβ - 1/192 *cβ*c2β)*sβ2 + (7/192 *cβ + 5/192 *cβ*c2β)*c2*sβ2)*s1p3)
		    A(64) = δ*((η*(1/64 + 1/192 *c2β) - (1/128 + 1/384 *c2β))*c1p6*sβ3*s1p4)
		    A(65) = δ*(η*c1p2*(-245/2048 *sβ + 57/4096 *s3β - c4*(7/2048 *sβ + 13/4096 *s3β + 15/4096 *s5β) _
		    + c2*(-27/512 *sβ + 151/1024 *s3β - 3/1024 *s5β) - 13/4096 *s5β)*s1p4 + c1p2*(-1675/4096 *sβ _
		    - 825/8192 *s3β + c2*(27/1024 *sβ - 151/2048 *s3β + 3/2048 *s5β) + c4/4096*(7*sβ + 13*s3β + 15*s5β)*s1p4))
		    A(66) = δ*(4375*η*c1p6*(1/768 *sβ + 1/512 *s3β - 5/512 *s5β)*s1p4 + 4375*c1p6*(-1/1536 *sβ - 1/1024 *s3β + 5/1024 *s5β)*s1p4)
		    A(67) = δ*(c1p2*(-20475/4096 *sβ + 149391/8192 *s3β + c4/4096 *(567 *sβ + 1701/2 *s3β - 8505/2 *s5β) _
		    + c2/2048 *(4374 *sβ + 10017 *s3β - 1701 *s5β) - 7371/8192 *s5β)*s1p4 + η*c1p2*(3195/2048 *sβ _
		    - 45711/4096 *s3β + 7371/4096 *s5β + c2*(-2187/512 *sβ - 10017/1024 *s3β + 1701/1024 *s5β) _
		    + c4*(-567/2048 *sβ - 1701/4096 *s3β + 8505/4096 *s5β))*s1p4)
		    A(68) = δ*(η*c1p3*((37/192 *cβ - 1/192 *cβ*c2β)*sβ2 - (7/192 *cβ + 5/192 *cβ*c2β)*c2*sβ2)*s1p5 _
		    + c1p3*((-37/384 *cβ + 1/384 *cβ*c2β)*sβ2 + (7/384 *cβ + 5/384 *cβ*c2β)*c2*sβ2)*s1p5)
		    A(69) = δ*(1/128 + 1/384 *c2β - η*(1/64 + 1/192 *c2β))*c1p4*sβ3*s1p6
		    A(70) = δ*(η*((14067/4096 + 4689/1024 *c2β - 5751/4096 *c4β)*sβ + (-297/1024 + 1053/256 *c2β _
		    - 2187/1024 *c4β)*c2*sβ - (5103/4096 + 1701/1024 *c2β + 3645/4096 *c4β)*c4*sβ)*s1p6 _
		    + ((-55539/8192 - 8145/2048 *c2β + 5751/8192 *c4β)*sβ + (297/2048 - 1053/512 *c2β _
		    + 2187/2048 *c4β)*c2*sβ + (5103/8192 + 1701/2048 *c2β + 3645/8192 *c4β)*c4*sβ)*s1p6)
		    // The multiplication location
		    A(71) = δ*(c1p4*(4375/1536 *sβ + 4375/1024 *s3β - 21875/1024 *s5β)*s1p6 _
		    + η*c1p4*(-4375/768 *sβ - 4375/512 *s3β + 21875/512 *s5β)*s1p6)
		    A(72) = δ*((4375/384 *cβ + 625/256 *c3β + 3125/256 *c5β - η*(4375/192 *cβ _
		    + 625/128 *c3β + 3125/128 *c5β))*c1p3*s1p7)
		    A(73) = δ*(η*c1*((351/128 *cβ - 243/128 *cβ*c2β)*sβ2 - (567/128 *cβ + 405/128 *cβ*c2β)*c2*sβ2)*s1p7 _
		    + c1*((-351/256 *cβ + 243/256 *cβ*c2β)*sβ2 + (567/256 *cβ + 405/256 *cβ*c2β)*c2*sβ2)*s1p7)
		    A(74) = δ*((243/256 - 81/256 *c2β - η*(243/128 + 81/128 *c2β))*c1p2*sβ3*s1p8)
		    A(75) = δ*((4375/1024 *sβ + 8125/2048 *s3β + 9375/2048 *s5β - η*(4375/512 *sβ + 8125/1024 *s3β + 9375/1024 *s5β))*c1p2*s1p8)
		    // Here is when Dr. Moore and i realized that using the '[ ]' is totally wrong, instead i should just replace them with normal parenthesis.
		    A(76) = δ*((11875/768 *cβ + 3125/768 *c3β - η*(11875/384 *cβ + 3125/384 *c3β))*c1*sβ2*s1p9)
		    A(77) = δ*((625/256 + 625/768 *c2β - η*(625/128 + 625/384 *c2β))*sβ3*s1p10)
		    A(78) = δ*(η*((10197/20488 *cβ - 3969/2048 *cβ*c2β)*sβ2 - (1701/2048 *cβ + 5103/2048 *cβ*c2β)*c4*sβ2)*s2p3 _
		    + ((-44757/4096 *cβ + 3969/4096 *cβ*c2β)*sβ2 + (1701/4096 *cβ + 5103/4096 *cβ*c2β)*c4*sβ2)*s2p3)
		    A(79) = δ*((21875/4096 *cβ + 13125/4096 *c3β - η*(21875/2048 *cβ + 13125/2048 *c3β))*sβ2*s2p5)
		    A(80) = δ*((-37071/16384 *cβ*c2β + cβ*(-7641/8192 + 567/32768 *c4β) - (10917/8192 *cβ _
		    + 2835/1024 *cβ*c2β)*c2 + (-10089/16284 *cβ + 135/8192 *cβ*c2β)*c4 + 513/8192 *cβ*c6 _
		    + 567/32768 *cβ*c8)*s2 - 81/8192 *cβ*c4β*s4 + 1053/65536 *cβ*c4β*s6  _ 
		    + (2565/32768 *c3β + 729/32768 *c5β)*s8 + (243/131072 *c3β + 1215/131072 *c5β)*s10 _
		    + η*((5967/8192 *cβ*c2β + cβ*(2457/4096 - 567/16384 *c4β) + (4005/4096 *cβ + 243/512 *cβ*c2β)*c2 _
		    + (6633/8192 *cβ - 5319/4096 *cβ*c2β)*c4 - 513/4096 *cβ*c6 - 567/16384 *cβ*c8)*s2 _
		    + 81/4096 *cβ*c4β*s4 - 1053/32768 *cβ*c4β*s6 - (2565/16384 *c3β + 729/16384 *c5β)*s8 _
		    - (243/65536 *c3β + 1215/65536 *c5β)*s10))
		    A(81) = δ*((-18603/8192 *cβ*c2β + cβ*(-20475/32768 + 567/32768 *c4β))*s2 + (2835/2048 *cβ*c2β _
		    + cβ*(5715/8192 + 81/8192 *c4β))*s4 + (135/16384 *cβ*c2β + cβ*(-20745/65536 + 1053/65536 *c4β))*s6 _
		    - (513/16384 *cβ + 2565/32768 *c3β + 729/32768 *c5β)*s8 _ 
		    + (567/65536 *cβ + 243/131072 *c3β + 1215/131072*c5β)*s10 + η*((5643/4096 *cβ*c2β + cβ*(3195/16384 - 567/16384 *c4β))*s6 _
		    + (513/8192 *cβ + 2565/16384 *c3β + 729/16384 *c5β)*s8 - (567/32768 *cβ + 243/65536 *c3β + 1215/65536 *c5β)*s10))
		    A(82) = δ*((319/24576 *cβ*c2β + cβ*(871/4096 + 1/49152 *c4β) + (933/4096 *cβ + 133/1536 *cβ*c2β)*c2 _
		    + (625/24576 *cβ + 211/4096 *cβ*c2β)*c4 - 11/12288 *cβ*c6 - 7/49152 *cβ*c8)*s2 - 1/12288 *cβ*c4β*s4 _
		    + 1/32768 *cβ*c4β*s6 - (45/16384 *c3β + 1/16384 *c5β)*s8  _ 
		    - (1/65536 *c3β + 5/65536 *c5β)*s10 + η*((257/12288 *cβ*c2β - cβ*(1493/6144 + 1/24576 *c4β) _
		    + (-1391/6144 + 11/768 *cβ*c2β)*c2 + (-49/12288 *cβ + 77/2048 *cβ*c2β)*c4 + 11/6144 *cβ*c6 _
		    + 7/24576 *cβ*c8)*s2 + 1/6144 *cβ*c4β*s4 - 1/16384 *cβ*c4β*s6 + (45/8192 *c3β + 1/8192 *c5β)*s8 + (1/32768 *c3β + 5/32768 *c5β)*s10))
		    A(83) = δ*((-157/12288 *cβ*c2β + cβ*(9827/49152 + 1/49152 *c4β))*s2 + (-133/3072 *cβ*c2β _
		    + cβ*(-1405/12288 + 1/12288 *c4β))*s4 + (211/8192 *cβ*c2β + cβ*(419/32768 + 1/32768 *c4β))*s6 _
		    + (11/24576 *cβ + 45/16384 *c3β + 1/16384 *c5β)*s8 + (-7/98304 *cβ - 1/65536 *c3β - 5/65536 *c5β)*s10  _ 
		    + η*((13/6144 *cβ*c2β + cβ*(-5923/24576 - 1/24576 *c4β))*s2 + (-11/1536 *cβ*c2β + cβ*(701/6144 - 1/6144 *c4β))*s4 _
		    + (77/4096 *cβ*c2β + cβ*(-35/16384 - 1/16384 *c4β))*s6 + (-11/12288 *cβ - 45/8192 *c3β - 1/8192 *c5β)*s8 + (7/49152 *cβ _
		    + 1/32768 *c3β + 5/32768 *c5β)*s10))
		    A(84) = δ*((-341/8192 *cβ + 1/8192 *cβ*c2β)*sβ2*s2 + (-3411/16384 *cβ + 7/16384 *cβ*c2β)*sβ2*s6 + (35/32768 *cβ _
		    + 21/32768 *c3β)*sβ2*s10 + η*((-43/4096 *cβ - 1/4096 *cβ*c2β)*sβ2*s2 + (-429/8192 *cβ + 7/8192 *cβ*c2β)*sβ2*s6 _
		    + (-35/16384 *cβ - 21/16384 *c3β)*sβ2*s10))
		    
		    //HP 3/2,SO
		    
		    A(85) = χsxDN*(2*cβ*c2p3*sβ - η*cβ*c2p3*sβ)
		    A(86) = χszDN*(η*c1p4*(-5/2 - 7/2 *c2β + (1/2 + 1/6 *c2β)*c2) + c1p4*(-3 - c2β + (5 + 5/3 *c2β)*c4)) _
		    + χsxDN*(c1p4*(7/3 *s2β - 10/3 *c2*s2β) - η*c1p4*(19/6 *s2β + 1/3 *c2*s2β))
		    A(87) = χsxDN*(η*(1/2 + 1/6 *c2β)*c1p5*s1 + (5 + 5/3 *c2β)*c1p5*s1)
		    A(88) = χsxDN*(η*(1/2 + 1/6 *c2β)*c1*s1p5 + (5 + 5/3 *c2β)*c1*s1p5)
		    A(89) = χsxDN*(η*c1p3*(-17/4 + 79/12 *c2β + (-1/4 + 7/12 *c2β)*c2)*s1 + c1p3*(3/2 - 13/6 *c2β _
		    + (-5/2 + 35/6 *c2β)*c2)*s1) + χszDN*(η*c1p3*(-7 *s2β + 2/3 *c2*s2β)*s1 + c1p3*(-2 *s2β + 20/3 *c2*s2β)*s1)
		    A(90) = χsxDN*(c1*(3/2 - 13/6 *c2β + (5/2 - 35/6 *c2β)*c2)*s1p3 + η*c1*(-17/4 + 79/12*c2β _
		    + (1/4 - 7/12 *c2β)*c2)*s1p3) + χszDN*(-c1 *(2*s2β + 20/3 *c2*s2β)*s1p3 - η*c1*(7*s2β + 2/3 *c2*s2β)*s1p3)
		    A(91) = χszDN*(η*(5/2 + 7/2 *c2β + (1/2 + 1/6 *c2β)*c2)*s1p4 + (3 + c2β + (5 + 5/3 *c2β)*c2)*s1p4) _
		    + χsxDN*(-(7/3 *s2β + 10/3 *c2*s2β)*s1p4 + η*(19/6 *s2β - 1/3 *c2*s2β)*s1p4)
		    A(92) = χszDN*(-3 + 3/2 *η)*c2*sβ2*s2p2
		    A(93) = χsxDN*(3/4 + 1/4 *c2β - η*(3/8 + 1/8 *c2β))*s2p3
		    A(94) = χsxDN*(10/3 + 1/3 *η)*cβ*c2*sβ*s2p2 + χszDN*(5 + 1/2 *η)*c2*sβ2*s2p2
		    A(95) = χszDN*(3/2 + 1/2 *c2β - η*(3/4 + 1/4 *c2β))*c2*s2p2 + χsxDN*(1/2 *η - 1)*c2*s2β*s2p2
		    A(96) = χsxDN*(-11/16 *c2β*s2 - 3/4 *s2p3 - 7/16 *c2β*s6 + η*(11/32 *c2β*s2 + 3/8 *s2p3 + 7/32 *c2β*s6)) _
		    + χszDN*(1/2 *s2β*s2 - 1/2 *s2β*s6 + η*(-1/4 *s2β*s2 + 1/4 *s2β*s6))
		    A(97) = χsyDN*((15/8 - 3/8 *c2β + (9/8 - 5/8 *c2β)*c4)*s2 + η*(-15/16 + 3/16 *c2β + (-9/16 + 5/16 *c2β)*c4)*s2)
		    A(98) = χsyDN*(η - 2)*cβ*c2*sβ*s2p2
		    A(99) = χsyDN*(3/4 + 1/4 *c2β - η*(3/8 + 1/8 *c2β))*s2p3
		    A(100) = χsyDN*(c1*(5/2 - 11/6 *c2β + (15/2 - 25/6 *c2β)*c2)*s1p3 + η*c1*(1/4 - 31/12 *c2β + (3/4 - 5/12 *c2β)*c2)*s1p3)
		    A(101) = χsyDN*(-(7/3 *s2β + 10/3 *c2*s2β)*s1p4 - η*(5/6 *s2β + 1/3 *c2*s2β)*s1p4)
		    A(102) = χsyDN*(5 + 5/3 *c2β + η*(1/2 + 1/6 *c2β))*c1*s1p5
		    // First time seeing a negative sign at the beginning.
		    A(103) = -χsyDN*(1/3 + 11/6 *η)*cβ*sβ*s2p2
		    A(104) = χsyDN*(η*c1p3*(1/4 - 31/12 *c2β + (-3/4 + 5/12 *c2β)*c2)*s1 + c1p3*(5/2 - 11/6 *c2β _
		    + (-15/2 + 25/6 *c2β)*c2)*s1)
		    A(105) = χsyDN*(c1p4*(7/3 *s2β - 10/3 *c2*s2β) + η*c1p4*(5/6 *s2β - 1/3 *c2*s2β))
		    A(106) = χsyDN*(η*(1/2 + 1/6 *c2β) + 5 + 5/3 *c2β)*c1p5*s1
		    A(107) = 2*δ*χaxDN*cβ*c2p3*sβ
		    A(108) = δ*(χazDN*c1p4*(-3 - c2β + (5 + 5/3 *c2β)*c2) + χaxDN*c1p4*(7/3 *s2β - 10/3 *c2*s2β))
		    A(109) = δ*χaxDN*(5 + 5/3 *c2β)*c1p5*s1
		    A(110) = δ*χaxDN*(5 + 5/3 *c2β)*c1*s1p5
		    A(111) = δ*(χaxDN*(3/2 - 13/6 *c2β + (-5/2 + 35/6 *c2β)*c2) + χazDN*(-2*s2β + 20/3 *c2*s2β))*c1p3*s1
		    A(112) = δ*(χaxDN*(3/2 - 13/6 *c2β + (5/2 - 35/6 *c2β)*c2) + χazDN*(-2*s2β - 20/3 *c2*s2β))*c1*s1p3
		    A(113) = δ*(χazDN*(3 + c2β + (5 + 5/3 *c2β)*c2)*s1p4 - χaxDN*(7/3 *s2β + 10/3 *c2*s2β)*s1p4)
		    A(114) = -3*δ*χazDN*c2*sβ2*s2p2
		    A(115) = δ*χaxDN*(3/4 + 1/4 *c2β)*s2p3
		    A(116) = δ*(10/3 *χaxDN*cβ*c2*sβ*s2p2 + 5*χazDN*c2*sβ2*s2p2)
		    A(117) = δ*χazDN*(3/2 + 1/2 *c2β)*c2*s2p2 - χaxDN*c2*s2β*s2p2
		    A(118) = δ*(χaxDN*(-11/16 *c2β*s2 - 3/4 *s2p3 - 7/16 *c2β*s6) + χazDN*(1/2 *s2β*s2 - 1/2 *s2β*s6))
		    A(119) = δ*(χayDN*(15/8 - 3/8 *c2β + (9/8 - 5/8 *c2β)*c4)*s2)
		    A(120) = -2*δ*χayDN*cβ*c2*sβ*s2p2
		    A(121) = δ*χayDN*(3/4 + 1/4 *c2β)*s2p3
		    A(122) = δ*χayDN*c1*(5/2 - 11/6 *c2β + (15/2 - 25/6 *c2β)*c2)*s1p3
		    A(123) = δ*χayDN*(-7/3 *s2β - 10/3 *c2*s2β)*s1p4
		    A(124) = δ*χayDN*(5 + 5/3 *c2β)*c1*s1p5
		    A(125) = -1/3 *δ*χayDN*cβ*sβ*s2p2
		    A(126) = δ*χayDN*c1p3*(5/2 - 11/6 *c2β + (-15/2 + 25/6 *c2β)*c2)*s1
		    A(127) = δ*χayDN*c1p4*(7/3 *s2β - 10/3 *c2*s2β)
		    A(128) = δ*χayDN*(5 + 5/3 *c2β)*c1p5*s1
		    
		    //Amplitude factors for H3X
		    
		    A(173) = 8*π*c1*sβ*s1p3
		    A(174) = -4*π*cβ*s1p4
		    A(175) = -8*π*c1p3*sβ*s1
		    A(176) = -4*π*cβ*c1p4
		    A(177) = δ*(c1p4*(-4375/384 *s2β - 4375/256 *s4β)*s1p6 + η*c1p4*(4375/192 *s2β + 4375/128 *s4β)*s1p6)
		    A(178) = δ*(625/96 *c2β + 625/32 *c4β - η*(625/48 *c2β + 625/16 *c4β))*c1p3*s1p7
		    A(179) = δ*(-625/256 *s2β + 5625/512 *s4β + η*(625/128 *s2β - 5625/256 *s4β))*c1p2*s1p8
		    A(180) = δ*(625/96 + 625/48 *c2β - η*(625/48 + 625/24 *c2β))*c1*sβ2*s1p9
		    A(181) = δ*(625/192 - 625/96 *η)*cβ*sβ3*s1p10
		    A(182) = δ*(η*c1p2*(-4923/512 *s2β + c2*(459/128 *s2β - 2079/256 *s4β) _
		    - 945/1024 *s4β + c4*(567/512 *s2β + 1701/1024 *s4β))*s1p4 + c1p2*(22203/1024 *s2β - c4*(567/1024 *s2β + 1701/2048 *s4β) _
		    + 945/2048 *s4β + c2*(-459/256 *s2β + 2079/512 *s4β))*s1p4)
		    A(183) = δ*(η*c1*(27/16 + 1233/128 *c2β + 27/128 *c4β + (27/8 + 27/16 *c2β + 27/16 *c4β)*c2 _
		    - (81/128 *c2β + 243/128 *c4β)*c4)*s1p5 + c1*(-27/32 - 4689/256 *c2β - 27/256 *c4β - (27/16 + 27/32 *c2β + 27/32 *c4β)*c2  _ 
		    + (81/256 *c2β + 243/256 *c4β)*c4)*s1p5)
		    A(184) = δ*(η*((4761/1024 - 1377/1024 *c2β)*s2β + (837/256 - 621/256 *c2β)*c2*s2β + (243/1024 - 2187/1024 *c2β)*c4*s2β)*s1p6 _
		    + ((-11673/2048 + 1377/2048 *c2β)*s2β + (-837/512 + 621/512 *c2β)*c2*s2β + (-243/2048 + 2187/2048 *c2β)*c4*s2β)*s1p6)
		    A(185) = δ*(η*c1*((81/32 - 27/16 *c2β)*sβ2 - (81/32 + 81/16 *c2β)*c2*sβ2)*s1p7 _
		    + c1*((-81/64 + 27/32 *c2β)*sβ2 + (81/64 + 81/32 *c2β)*c2*sβ2)*s1p7)
		    A(186) = δ*(81/64 - 81/32 *η)*cβ*c1p2*sβ3*s1p8
		    A(187) = δ*(683/16384 *cβ*sβ + (557/4096 - 11/12288 *c2β)*c4*s2β + (-1719/32768 + 91/32768 *c2β)*c6*s2β _
		    - 1/16384 *cβ*s3β + c2*(-10511/49152 *cβ*sβ + 173/49152 *cβ*s3β) + η*(85/8192 *cβ*sβ + (-679/6144 + 11/6144 *c2β)*c4*s2β  _ 
		    - (201/16384 + 91/16384 *c2β)*c6*s2β + 1/8192 *cβ*s3β + c2*(6031/24576 *cβ*sβ - 173/24576 *cβ*s3β) _
		    - c10*(7/49152 *s2β + 7/32768 *s4β) + c8*(-37/24576 *s2β + 91/16384 *s4β)) _
		    + c8*(37/49152 *s2β - 91/32768 *s4β) + c10*(7/98304 *s2β + 7/65536 *s4β))
		    A(188) = δ*(η*(19/512 *c4β*c3 + 9/512 *c4β*c5 + c1*(-11/16 - 35/128 *c2β + 79/1536 *c4β + (1/32 - 37/256 *c2β)*c2 _
		    + (1/32 + 3/128 *c2β)*c4 - 1/768 *c2β*c6) - 1/512 *c4β*c7)*s1p3 + (-19/1024 *c4β*c3 - 9/1024 *c4β*c5 _
		    + c1*(19/32 - 23/768 *c2β - 79/3072 *c4β - (1/64 + 347/512 *c2β)*c2 - (1/64 + 3/256 *c2β)*c4 + 1/1536 *c2β*c6) _
		    + 1/1024 *c4β*c7)*s1p3)
		    A(189) = δ*(c1p2*(-355/1024 *s2β - c2*(13/256 *s2β + 11/512 *s4β) + c4*(-1/1024 *s2β + 9/2048 *s4β) - 5/2048 *s4β)*s1p4 _
		    + η*c1p2*(-29/512 *s2β + c4*(1/512 *s2β - 9/1024 *s4β) + c2*(13/128 *s2β + 11/256 *s4β) + 5/1024 *s4β)*s1p4)
		    A(190) = δ*(η*c1p3*((7/48 + 1/24 *c2β)*sβ2 - (1/48 + 1/24 *c2β)*c2*sβ2)*s1p5 _
		    + c1p3*(-(7/96 + 1/48 *c2β)*sβ2 + (1/96 + 1/48 *c2β)*c2*sβ2)*s1p5)
		    A(191) = δ*(1/96 - 1/48 *η)*cβ*c1p4*sβ3*s1p6
		    A(192) = δ*((-77/256 + 1/256 *cβ)*sβ2*s4 + (5/512 + 7/512 *c2β)*sβ2*s8 _
		    + η*((45/128 - 1/128 *c2β)*sβ2*s4 - (5/256 - 7/256 *c2β)*sβ2*s8))
		    A(193) = δ*(135/64 + 189/64 *c2β - η*(135/32 + 189/32 *c2β))*c2*sβ2*s2p3
		    A(194) = δ*(-683/16384 *cβ*sβ + (-557/4096 + 11/12288 *c2β)*c4*s2β + (-1719/32768 + 91/32768 *c2β)*c6*s2β _
		    + 1/16384 *cβ*sβ + c2*(-10511/49152 *cβ*sβ + 173/49152 *cβ*s3β) + η*(-85/8192 *cβ*sβ + (679/6144 - 11/6144 *c2β)*c4*s2β  _ 
		    - (201/16384 + 91/16384 *c2β)*c6*s2β - 1/8192 *cβ*s3β + c2*(6031/24576 *cβ*sβ - 173/24576 *cβ*s3β) + c8*(37/24576 *s2β _
		    - 91/16384 *s4β) - c10*(7/49152 *s2β + 7/32768 *s4β)) + c10*(7/98304 *s2β + 7/65536 *s4β) + c8*(-37/49152 *s2β + 91/32768 *s4β))
		    A(195) = δ*(c1p3*(19/32 - 23/768 *c2β - 79/3072 *c4β + (1/64 + 347/512 *c2β)*c2 - (1/64 + 3/256 *c2β)*c4 - 1/1536 *c2β*c6)*s1 _
		    + 19/1024 *c4β*c1p3*s3 - 9/1024 *c4β*c1p3*s5 - 1/1024 *c4β*c1p3*s7 + η*(c1p3*(-11/16 - 35/128 *c2β + 79/1536 *c4β + (-1/32 + 37/256 *c2β)*c2 _ 
		    +(1/32 + 3/128 *c2β)*c4 + 1/768 *c2β*c6)*s1 - 19/512 *c4β*c1p3*s3 + 9/512 *c4β*c1p3*s5 + 1/512 *c4β*c1p3*s7))
		    A(196) = δ*(η*c1p4*(4923/512 *s2β + c4*(567/1024 *s2β + 1701/2048 *s4β) - 945/2048 *s4β + c2*(-459/256 *s2β + 2079/512 *s4β))*s1p2)
		    A(197) = δ*(η*c1p5*(27/16 + 1233/128 *c2β + 27/128 *c4β - (27/8 + 27/16 *c2β + 27/16 *c4β)*c2 - (81/128 *c2β + 243/128 *c4β)*c4)*s1 _
		    + c1p5*(-27/32 - 4689/256 *c2β - 27/256 *c4β + (27/16 + 27/32 *c2β + 27/32 *c4β)*c2 + (81/256 *c2β + 243/256 *c4β)*c4)*s1)
		    // The multiplication denominator, first time seeing that.
		    A(198) = δ*(c1p6*(11673/2048 *s2β + c4*(243/2048 *s2β - 2187/4096 *s4β) + c2*(-837/512 *s2β + 621/1024 *s4β) - 1377/4096 *s4β) _
		    + η*c1p6*(-4761/1024 *s2β + c2*(837/256 *s2β - 621/512 *s4β) + 1377/2048 *s4β + c4*(-243/1024 *s2β + 2187/2048*s4β)))
		    A(199) = δ*(c1p7*((-81/64 + 27/32 *c2β)*sβ2 - (81/64 + 81/32 *c2β)*c2*sβ2)*s1  _
		    + η*c1p7*((81/32 - 27/16 *c2β)*sβ2 + (81/32 + 81/16 *c2β)*c2*sβ2)*s1)
		    A(200) = δ*(81/32 *η - 81/64)*cβ*c1p8*sβ3*s1p2
		    A(201) = δ*(4375/384 *s2β + 4375/256 *s4β - η*(4375/192 *s2β + 4375/128 *s4β))*c1p6*s1p4
		    A(202) = δ*(625/96 *c2β + 625/32 *c4β - η*(625/48 *c2β + 625/16 *c4β))*c1p7*s1p3
		    A(203) = δ*(625/256 *s2β - 5625/512 *s4β + η*(-625/128 *s2β + 5625/256 *s4β))*c1p8*s1p2
		    A(204) = δ*(625/96 + 625/48 *c2β - η*(625/48 + 625/24 *c2β))*c1p9*sβ2*s1
		    A(205) = δ*(625/96 *η - 625/192)*cβ*c1p10*sβ3
		    
		    //HX 3/2, SO
		    
		    A(206) = χsyDN*(2*c2p3*sβ - η*c2p3*sβ)
		    A(207) = χsyDN*(η*c1p4*(-5/3 *sβ + 2/3 *c2*sβ) + c1p4*(-14/3 *sβ + 20/3 *c2*sβ))
		    A(208) = χsyDN*(-20/3 *cβ*c1p5*s1 - 2/3 *η*cβ*c1p5*s1)
		    A(209) = χsyDN*(η*c1p3*(7/3 *cβ + 1/3 *cβ*c2)*s1 + c1p3*(-2/3 *cβ + 10/3 *cβ*c2)*s1)
		    A(210) = χsyDN*(η*c1*(7/3 *cβ - 1/3 *cβ*c2)*s1p3 + c1*(-2/3 *cβ - 10/3 *cβ*c2)*s1p3)
		    // The bigg?
		    //A(211) = χsyDN*(η*(5/3 *sβ + 2/3 *c2*sβ)*s1p4 + bigg*(14/3 *sβ + 20/3 *c2*sβ)*s1p4)
		    A(211) = χsyDN*s1p4*sβ*(7+5/2*η + c2*(10 + η))
		    A(212) = χsyDN*(-20/3 *cβ*c1*s1p5 - 2/3 *η*cβ*c1*s1p5)
		    A(213) = χsyDN*(2*c2*sβ*s2p2 - η*c2*sβ*s2p2)
		    A(214) = χsyDN*(10/3 *c2*sβ*s2p2 + 1/3 *η*c2*sβ*s2p2)
		    A(215) = χsyDN*(-cβ*s2p3 + 1/2 *η*cβ*s2p3)
		    A(216) = χsyDN*(-5/4 *cβ*s2 - 1/4 *cβ*s6 + η*(5/8 *cβ*s2 + 1/8 *cβ*s6))
		    A(217) = (χsxDN*((-3/2 *cβ - 1/2 *cβ*c4)*s2 + η*(3/4 *cβ + 1/4 *cβ*c4)*s2) + χszDN*(-2*c4*sβ*s2 + η*c4*sβ*s2))
		    A(218) = (χszDN*(2*cβ*c2*s2p2 - η*cβ*c2*s2p2) + χsxDN*(-2*c2*sβ*s2p2 + η*c2*sβ*s2p2))
		    A(219) = χsxDN*(cβ*s2p3 - 1/2 *η*cβ*s2p3)
		    A(220) = (χsxDN*(c1*(-2/3 *cβ - 10/3 *cβ*c2)*s1p3 + η*c1*(-5/3 *cβ + 4*c3β - 1/3 *cβ*c2)*s1p3) _
		    + χszDN*(c1*(-4*sβ - 40/3 *c2*sβ)*s1p3 + η*c1*(-2*sβ - 4/3 *c2*sβ - 4*s3β)*s1p3))
		    A(221) = (χszDN*(η*(5*cβ + c3β + 2/3 *cβ*c2)*s1p4 + (4*cβ + 20/3 *cβ*c2)*s1p4) _
		    + χsxDN*((-14/3 *sβ - 20/3 *c2*sβ)*s1p4 + η*(10/3 *sβ - 2/3 *c2*sβ + s3β)*s1p4))
		    A(222) = χsxDN*(20/3 *cβ*c1*s1p5 + 2/3 *η*cβ*c1*s1p5)
		    A(223) = (-6*η*χszDN*cβ*sβ2*s2p2 + χsxDN*(1/3 *sβ*s2p2 + η*(-7/6 + 3*c2β)*sβ*s2p2))
		    A(224) = (χsxDN*(η*c1p3*(-5/3 *cβ + 4*c3β + 1/3 *cβ*c2)*s1 + c1p3*(-2/3 *cβ + 10/3 *cβ*c2)*s1) _
		    + χszDN*(c1p3*(-4*sβ + 40/3 *c2*sβ)*s1 + η*c1p3*(-2*sβ + 4/3 *c2*sβ - 4*s3β)*s1))
		    A(225) = (χszDN*(η*c1p4*(-5*cβ - c3β + 2/3 *cβ*c2) + c1p4*(-4*cβ + 20/3 *cβ*c2)) _
		    + χsxDN*(c1p4*(14/3 *sβ - 20/3 *c2*sβ) + η*c1p4*(-10/3 *sβ - 2/3 *c2*sβ - s3β)))
		    A(226) = χsxDN*(20/3 *cβ*c1p5*s1 + 2/3 *η*cβ*c1p5*s1)
		    A(227) = δ*(2*χayDN*c2p3*sβ)
		    A(228) = δ*(χayDN*c1p4*(-14/3 *sβ + 20/3 *c2*sβ))
		    A(229) = δ*(χayDN*c1p3*(-2/3 *cβ + 10/3 *cβ*c2)*s1)
		    A(230) = δ*(-20/3 *χayDN*cβ*c1p5*s1)
		    A(231) = δ*(χayDN*c1*(-2/3 *cβ - 10/3 *cβ*c2)*s1p3)
		    A(232) = δ*(χayDN*(14/3 *sβ + 20/3 *c2*sβ)*s1p4)
		    A(233) = δ*(-20/3 *χayDN*cβ*c1*s1p5)
		    A(234) = δ*(2*χayDN*c2*sβ*s2p2)
		    // Interesting + sign in sheet
		    A(235) = δ*(10/3 *χayDN*c2*sβ*s2p2)
		    A(236) = δ*(-χayDN*cβ*s1p3)
		    A(237) = δ*(χayDN*(-5/4 *cβ*s2 - 1/4 *cβ*s6))
		    A(238) = δ*(χaxDN*(-3/2 *cβ - 1/2 *cβ*c4)*s2 - 2*χazDN*c4*sβ*s2)
		    A(239) = δ*(2*χazDN*cβ*c2*s2p2 - 2*χaxDN*c2*sβ*s2p2)
		    A(240) = δ*(χaxDN*cβ*s2p3)
		    A(241) = δ*(χaxDN*c1*(-2/3 *cβ - 10/3 *cβ*c2)*s1p3 + χazDN*c1*(-4*sβ - 40/3 *c2*sβ)*s1p3)
		    A(242) = δ*(χazDN*(4*cβ + 20/3 *cβ*c2)*s1p4 + χaxDN*(-14/3 *sβ - 20/3 *c2*sβ)*s1p4)
		    A(243) = δ*(20/3 *χaxDN*cβ*c1*s1p5)
		    A(244) = δ*(1/3 *χaxDN*sβ*s2p2)
		    A(245) = δ*(χaxDN*c1p3*(-2/3 *cβ + 10/3 *cβ*c2)*s1 + χazDN*c1p3*(-4*sβ + 40/3 *c2*sβ)*s1)
		    A(246) = δ*(χazDN*c1p4*(-4*cβ + 20/3 *cβ*c2) + χaxDN*c1p4*(14/3 *sβ - 20/3 *c2*sβ))
		    A(247) = δ*(20/3 *χaxDN*cβ*c1p5*s1)
		    
		    
		    
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateWaveFactors()
		  // Calculate the received wave phase
		  
		  // Calculate basic angle multiples for the phase Ψ
		  CosApΨ(0,1) = Cos(ΨrDN)
		  SinApΨ(0,1) = Sin(ΨrDN)
		  CosApΨ(0,2) = CosApΨ(0,1)*CosApΨ(0,1) - SinApΨ(0,1)*SinApΨ(0,1)
		  SinApΨ(0,2)  = 2*CosApΨ(0,1)*SinApΨ(0,1)
		  CosApΨ(0,3) = CosApΨ(0,2)*CosApΨ(0,1) - SinApΨ(0,2)*SinApΨ(0,1)
		  SinApΨ(0,3)  = SinApΨ(0,2)*CosApΨ(0,1) + CosApΨ(0,2)*SinApΨ(0,1)
		  CosApΨ(0,4) = CosApΨ(0,3)*CosApΨ(0,1) - SinApΨ(0,3)*SinApΨ(0,1)
		  SinApΨ(0,4)  = SinApΨ(0,3)*CosApΨ(0,1) + CosApΨ(0,3)*SinApΨ(0,1)
		  CosApΨ(0,5) = CosApΨ(0,4)*CosApΨ(0,1) - SinApΨ(0,4)*SinApΨ(0,1)
		  SinApΨ(0,5)  = SinApΨ(0,4)*CosApΨ(0,1) + CosApΨ(0,4)*SinApΨ(0,1)
		  
		  // Calculate basic angle multiples for the phase α
		  CosApΨ(1,0) = Cos(αDN)
		  SinApΨ(1,0) = Sin(αDN)
		  CosApΨ(2,0) = CosApΨ(1,0)*CosApΨ(1,0) - SinApΨ(1,0)*SinApΨ(1,0)
		  SinApΨ(2,0)  = 2*CosApΨ(1,0)*SinApΨ(1,0)
		  CosApΨ(3,0) = CosApΨ(2,0)*CosApΨ(1,0) - SinApΨ(2,0)*SinApΨ(1,0)
		  SinApΨ(3,0)  = SinApΨ(2,0)*CosApΨ(1,0) + CosApΨ(2,0)*SinApΨ(1,0)
		  CosApΨ(4,0) = CosApΨ(3,0)*CosApΨ(1,0) - SinApΨ(3,0)*SinApΨ(1,0)
		  SinApΨ(4,0)  = SinApΨ(3,0)*CosApΨ(1,0) + CosApΨ(3,0)*SinApΨ(1,0)
		  CosApΨ(5,0) = CosApΨ(4,0)*CosApΨ(1,0) - SinApΨ(4,0)*SinApΨ(1,0)
		  SinApΨ(5,0)  = SinApΨ(4,0)*CosApΨ(1,0) + CosApΨ(4,0)*SinApΨ(1,0)
		  
		  // Calculate noise factors
		  // This is the value of the observed orbital frequency in Hz
		  Var fN As Double =  VDN*VDN*VDN/(2*Parameters.π*Parameters.GM)*Parameters.OneI1pZ
		  //  get the noise at various frequencies
		  // The following array allows us to to enhance or suppress values of harmonics at various frequencies
		  // to reflect how they may be better or more poorly received by the detector due to noise
		  // (Declaring this array as a class property reduces background memory management.)
		  Sn(1) = 1.0/Sqrt(Noise.GetNoise(fN))
		  Sn(2) = 1.0/Sqrt(Noise.GetNoise(2*fN))
		  Sn(3) = 1.0/Sqrt(Noise.GetNoise(3*fN))
		  Sn(4) = 1.0/Sqrt(Noise.GetNoise(4*fN))
		  Sn(5) = 1.0/Sqrt(Noise.GetNoise(5*fN))
		  
		  // Now basically calculate all possible combinations and weight according to the
		  // noise at a given frequency
		  For k As Integer = 1 to 5
		    Var sn As Double = Sn(k)
		    for j as Integer = 1 to 5
		      CosApΨ(j,k) = (CosApΨ(j,0)*CosApΨ(0,k) - SinApΨ(j,0)*SinApΨ(0,k))*sn
		      CosAmΨ(j,k) = (CosApΨ(j,0)*CosApΨ(0,k) + SinApΨ(j,0)*SinApΨ(0,k))*sn
		      SinApΨ(j,k)  = (SinApΨ(j,0)*CosApΨ(0,k) + CosApΨ(j,0)*SinApΨ(0,k))*sn
		      SinAmΨ(j,k)  = (SinApΨ(j,0)*CosApΨ(0,k) - CosApΨ(j,0)*SinApΨ(0,k))*sn
		    Next
		    CosApΨ(0,k) = CosApΨ(0,k)*sn
		    CosAmΨ(0,k) = CosApΨ(0,k)
		    SinApΨ(0,k) = SinApΨ(0,k)*sn
		    SinAmΨ(0,k) = -SinApΨ(0,k)
		  Next
		  
		  // Now calculate all wavy parts
		  // Factors for H0P
		  W(0) = CosApΨ(2,2)  // cos(2α + 2Ψ)
		  W(1) = CosApΨ(1,2)   // cos(α + 2Ψ)
		  W(2) = CosAmΨ(1,2)  // cos(α - 2Ψ)
		  W(3) =  CosAmΨ(2,2) // cos(2α - 2Ψ)
		  W(4) = CosApΨ(0,2)  // cos(2Ψ)
		  
		  DWDα(0) = -2.0*SinApΨ(2,2)  // derivative of cos(2α + 2Ψ) with respect to α
		  DWDα(1) = -SinApΨ(1,2)   // derivqtive of cos(α + 2Ψ)
		  DWDα(2) = -SinAmΨ(1,2)  // derivative of cos(α - 2Ψ)
		  DWDα(3) =  -2.0*SinAmΨ(2,2) // derivative of cos(2α - 2Ψ)
		  DWDα(4) = 0.0  // derivative of cos(2Ψ)
		  
		  DWDΨ(0) = -2.0*SinApΨ(2,2)  // derivative of cos(2α + 2Ψ) with respect to Ψ
		  DWDΨ(1) = -2.0*SinApΨ(1,2)   // derivqtive of cos(α + 2Ψ)
		  DWDΨ(2) = 2.0*SinAmΨ(1,2)  // derivative of cos(α - 2Ψ)
		  DWDΨ(3) =  2.0*SinAmΨ(2,2) // derivative of cos(2α - 2Ψ)
		  DWDΨ(4) = -2.0*SinAmΨ(0,2) // derivative of cos(2Ψ)
		  
		  if Parameters.PNOrder > 0 Then
		    
		    // Factors for H1P
		    W(5) = CosApΨ(3,3)  // cos(3α + 3Ψ)
		    W(6) = CosApΨ(1,1)  // cos(α + Ψ)
		    W(7) = CosAmΨ(1,1)   // cos(α - Ψ)
		    W(8) = CosApΨ(3,1)   // cos(3α + Ψ)
		    W(9) = CosApΨ(1,3)   // cos(α + 3Ψ)
		    W(10) = CosAmΨ(1,3)   // cos(α - 3Ψ)
		    W(11) = CosAmΨ(3,1)   // cos(3α - Ψ)
		    W(12) = CosAmΨ(3,3)  // cos(3α - 3Ψ)
		    W(13) = CosApΨ(0,3)   // cos(3Ψ)
		    W(14) = CosApΨ(2,1)   // cos(2α + Ψ)
		    W(15) = CosApΨ(2,3)   // cos(2α + 3Ψ)
		    W(16) = CosAmΨ(2,1)   // cos(2α  - Ψ)
		    W(17) = CosAmΨ(2,3)   // cos(2α - 3Ψ)
		    W(18) = CosApΨ(0,1)  // cos(Ψ)
		    
		    DWDα(5) = -3.0*SinApΨ(3,3) // derivative of cos(3α + 3Ψ) with respect to α
		    DWDα(6) = -1.0*SinApΨ(1,1)
		    DWDα(7) = -1.0*SinAmΨ(1,1)
		    DWDα(8) = -3.0*SinApΨ(3,1)
		    DWDα(9) = -1.0*SinApΨ(1,3)
		    DWDα(10) = -1.0*SinAmΨ(1,3)
		    DWDα(11) = -3.0*SinAmΨ(3,1)
		    DWDα(12) = -3.0*SinAmΨ(3,3)
		    DWDα(13) = 0.0
		    DWDα(14) = -2.0*SinApΨ(2,1)
		    DWDα(15) = -2.0*SinApΨ(2,3)
		    DWDα(16) = -2.0*SinAmΨ(2,1)
		    DWDα(17) = -2.0*SinAmΨ(2,3)
		    DWDα(18) = 0.0
		    
		    DWDΨ(5) = -3.0*SinApΨ(3,3) // derivative of cos(3α + 3Ψ) with respect to Ψ
		    DWDΨ(6) = -1.0*SinApΨ(1,1)
		    DWDΨ(7) = SinAmΨ(1,1)
		    DWDΨ(8) = -1.0*SinApΨ(3,1)
		    DWDΨ(9) = -3.0*SinApΨ(1,3)
		    DWDΨ(10) = 3.0*SinAmΨ(1,3)
		    DWDΨ(11) = SinAmΨ(3,1)
		    DWDΨ(12) = 3.0*SinAmΨ(3,3)
		    DWDΨ(13) = -3.0*SinApΨ(0,3)
		    DWDΨ(14) = -1.0*SinApΨ(2,1)
		    DWDΨ(15) = -3.0*SinApΨ(2,3)
		    DWDΨ(16) = SinAmΨ(2,1)
		    DWDΨ(17) = 3.0*SinAmΨ(2,3)
		    DWDΨ(18) = -1.0*SinApΨ(0,1)
		    
		  End If 
		  
		  if Parameters.PNOrder > 1 Then
		    
		    // Factors for H2P
		    W(19) =  CosApΨ(2,2)
		    W(20) =  CosApΨ(4,4)
		    W(21) =  CosApΨ(3,4)
		    W(22) =  CosApΨ(3,2)
		    W(23) =  CosApΨ(2,4)
		    W(24) =  CosApΨ(4,2)
		    W(25) =  CosApΨ(1,4)
		    W(26) =  CosAmΨ(1,2)
		    W(27) =  CosAmΨ(2,2)
		    W(28) =  CosAmΨ(1,4)
		    W(29) =  CosAmΨ(3,2)
		    W(30) =  CosAmΨ(2,4)
		    W(31) =  CosAmΨ(4,2)
		    W(32) =   CosAmΨ(3,4)
		    W(33) =   CosAmΨ(4,4)
		    W(34) =   CosApΨ(0,2)
		    W(35) =   CosApΨ(0,4)
		    W(36) =   CosApΨ(1,2)
		    
		    W(37) =   CosApΨ(1,1)
		    W(38) =   CosAmΨ(1,1)
		    W(39) =   SinAmΨ(1,1)
		    W(40) =   SinApΨ(0,1)
		    W(41) =   SinApΨ(1,1)
		    W(42) =   CosApΨ(1,1)
		    W(43) =   CosAmΨ(1,1)
		    W(44) =   SinAmΨ(1,1)
		    W(45) =   SinApΨ(0,1)
		    W(46) =   SinApΨ(1,1)
		    
		    
		    DWDα(19) =  -2.0*SinApΨ(2,2)
		    DWDα(20) =  -4.0*SinApΨ(4,4)
		    DWDα(21) =  -3.0*SinApΨ(3,4)
		    DWDα(22) =  -3.0*SinApΨ(3,2)
		    DWDα(23) =  -3.0*SinApΨ(2,4)
		    DWDα(24) =  -4.0*SinApΨ(4,2)
		    DWDα(25) =  -1.0*SinApΨ(1,4)
		    DWDα(26) =  -1.0*SinAmΨ(1,2)
		    DWDα(27) =  -2.0*SinAmΨ(2,2)
		    DWDα(28) =  -1.0*SinAmΨ(1,4)
		    DWDα(29) =  -3.0*SinAmΨ(3,2)
		    DWDα(30) =  -2.0*SinAmΨ(2,4)
		    DWDα(31) =  -4.0*SinAmΨ(4,2)
		    DWDα(32) =   -3.0*SinAmΨ(3,4)
		    DWDα(33) =   -4.0*SinAmΨ(4,4)
		    DWDα(34) =   0.0
		    DWDα(35) =   0.0
		    DWDα(36) =   -1.0*SinApΨ(1,2)
		    
		    DWDα(37) =   -1.0*SinApΨ(1,1)
		    DWDα(38) =   -1.0*SinAmΨ(1,1)
		    DWDα(39) =   CosAmΨ(1,1)
		    DWDα(40) =   0.0
		    DWDα(41) =   CosApΨ(1,1)
		    DWDα(42) =   -1.0*SinApΨ(1,1)
		    DWDα(43) =   -1.0*SinAmΨ(1,1)
		    DWDα(44) =   CosAmΨ(1,1)
		    DWDα(45) =   0
		    DWDα(46) =   CosApΨ(1,1)
		    
		    
		    DWDΨ(19) =  -2.0*SinApΨ(2,2)
		    DWDΨ(20) =  -4.0*SinApΨ(4,4)
		    DWDΨ(21) =  -4.0*SinApΨ(3,4)
		    DWDΨ(22) =  -2.0*SinApΨ(3,2)
		    DWDΨ(23) =  -4.0*SinApΨ(2,4)
		    DWDΨ(24) =  -2.0*SinApΨ(4,2)
		    DWDΨ(25) =  -4.0*SinApΨ(1,4)
		    DWDΨ(26) =  2.0*SinAmΨ(1,2)
		    DWDΨ(27) =  2.0*SinAmΨ(2,2)
		    DWDΨ(28) =  4.0*SinAmΨ(1,4)
		    DWDΨ(29) =  2.0*SinAmΨ(3,2)
		    DWDΨ(30) =  4.0*SinAmΨ(2,4)
		    DWDΨ(31) =  2.0*SinAmΨ(4,2)
		    DWDΨ(32) =   4.0*SinAmΨ(3,4)
		    DWDΨ(33) =   4.0*SinAmΨ(4,4)
		    DWDΨ(34) =   -2.0*SinApΨ(0,2)
		    DWDΨ(35) =   -2.0*SinApΨ(1,2)
		    DWDΨ(36) =   -2.0*SinApΨ(1,2)
		    
		    DWDΨ(37) =   -1.0*SinApΨ(1,1)
		    DWDΨ(38) =   SinAmΨ(1,1)
		    DWDΨ(39) =   -1.0*CosAmΨ(1,1)
		    DWDΨ(40) =   CosApΨ(0,1)
		    DWDΨ(41) =   CosApΨ(1,1)
		    DWDΨ(42) =   -1.0*SinApΨ(1,1)
		    DWDΨ(43) =   SinAmΨ(1,1)
		    DWDΨ(44) =   -1.0*CosAmΨ(1,1)
		    DWDΨ(45) =   CosApΨ(0,1)
		    DWDΨ(46) =   CosApΨ(1,1)
		    
		  end if 
		  
		  If Parameters.PNOrder > 2 Then
		    
		    // Factors for H3P
		    
		    W(47) =   CosApΨ(2,2)
		    W(48) =   CosApΨ(1,2)
		    W(49) =   CosAmΨ(1,2)
		    W(50) =   CosAmΨ(2,2)
		    W(51) =   CosApΨ(0,2)
		    W(52) =   CosApΨ(5,5)
		    W(53) =   CosApΨ(1,1)
		    W(54) =   CosApΨ(3,3)
		    W(55) =   CosApΨ(4,5)
		    W(56) =   CosApΨ(4,3)
		    W(57) =   CosApΨ(5,3)
		    W(58) =   CosAmΨ(1,1)
		    W(59) =   CosApΨ(3,1)
		    W(60) =   CosApΨ(3,5)
		    W(61) =   CosApΨ(1,3)
		    W(62) =   CosApΨ(2,5)
		    W(63) =   CosApΨ(4,1)
		    W(64) =   CosApΨ(5,1)
		    W(65) =   CosAmΨ(3,1)
		    W(66) =   CosApΨ(1,5)
		    W(67) =   CosAmΨ(1,3)
		    W(68) =   CosAmΨ(4,1)
		    W(69) =   CosAmΨ(5,1)
		    W(70) =   CosAmΨ(3,3)
		    W(71) =   CosAmΨ(1,5)
		    W(72) =   CosAmΨ(2,5)
		    W(73) =   CosAmΨ(4,3)
		    W(74) =   CosAmΨ(5,3)
		    W(75) =   CosAmΨ(3,5)
		    W(76) =   CosAmΨ(4,5)
		    W(77) =   CosAmΨ(5,5)
		    W(78) =   CosApΨ(0,3)
		    W(79) =   CosApΨ(0,5)
		    W(80) =   CosApΨ(2,3)
		    W(81) =   CosAmΨ(2,3)
		    W(82) =   CosApΨ(2,1)
		    W(83) =   CosAmΨ(2,1)
		    W(84) =   CosApΨ(0,1)
		    
		    W(85) =   CosApΨ(0,0)
		    W(86) =   CosApΨ(2,2)
		    W(87) =   CosApΨ(3,2)
		    W(88) =   CosAmΨ(3,2)
		    W(89) =   CosApΨ(1,2)
		    W(90) =   CosAmΨ(1,2)
		    W(91) =   CosAmΨ(2,2)
		    W(92) =   CosApΨ(0,0)
		    W(93) =   CosApΨ(3,0)
		    W(94) =   CosApΨ(0,2)
		    W(95) =   CosApΨ(2,0)
		    W(96) =   CosApΨ(1,0)
		    W(97) =   SinApΨ(1,0)
		    W(98) =   SinApΨ(2,0)
		    W(99) =   SinApΨ(3,0)
		    W(100) =   SinAmΨ(1,2)
		    W(101) =   SinAmΨ(2,2)
		    W(102) =   SinAmΨ(3,2)
		    W(103) =   SinApΨ(0,2)
		    W(104) =   SinApΨ(1,2)
		    W(105) =   SinApΨ(2,2)
		    W(106) =   SinApΨ(3,2)
		    W(107) =   CosApΨ(0,0)
		    W(108) =   CosApΨ(2,2)
		    W(109) =   CosApΨ(3,2)
		    W(110) =   CosAmΨ(3,2)
		    W(111) =   CosApΨ(1,2)
		    W(112) =   CosAmΨ(1,2)
		    W(113) =   CosAmΨ(2,2)
		    W(114) =   CosApΨ(0,0)
		    W(115) =   CosApΨ(3,0)
		    W(116) =   CosApΨ(0,2)
		    W(117) =   CosApΨ(2,0)
		    W(118) =   CosApΨ(1,0)
		    W(119) =   SinApΨ(1,0)
		    W(120) =   SinApΨ(2,0)
		    W(121) =   SinApΨ(3,0)
		    W(122) =   SinAmΨ(1,2)
		    W(123) =   SinAmΨ(2,2)
		    W(124) =   SinAmΨ(3,2)
		    W(125) =   SinApΨ(0,2)
		    W(126) =   SinApΨ(1,2)
		    W(127) =   SinApΨ(2,2)
		    W(128) =   SinApΨ(3,2)
		    
		    //DWDα
		    
		    DWDα(47) =   -2.0*SinApΨ(2,2)
		    DWDα(48) =   -SinApΨ(1,2)
		    DWDα(49) =   -SinAmΨ(1,2)
		    DWDα(50) =   -2.0*SinAmΨ(2,2)
		    DWDα(51) =   0.0
		    DWDα(52) =   -5.0*SinApΨ(5,5)
		    DWDα(53) =   -SinApΨ(1,1)
		    DWDα(54) =   -3.0*SinApΨ(3,3)
		    DWDα(55) =   -4.0*SinApΨ(4,5)
		    DWDα(56) =   -4.0*SinApΨ(4,3)
		    DWDα(57) =   -5.0*SinApΨ(5,3)
		    DWDα(58) =   -SinAmΨ(1,1)
		    DWDα(59) =   -3.0*SinApΨ(3,1)
		    DWDα(60) =   -3.0*SinApΨ(3,5)
		    DWDα(61) =   -SinApΨ(1,3)
		    DWDα(62) =   -2.0*SinApΨ(2,5)
		    DWDα(63) =   -4.0*SinApΨ(4,1)
		    DWDα(64) =   -5.0*SinApΨ(5,1)
		    DWDα(65) =   -3.0*SinAmΨ(3,1)
		    DWDα(66) =   -SinApΨ(1,5)
		    DWDα(67) =   -SinAmΨ(1,3)
		    DWDα(68) =   -4.0*SinAmΨ(4,1)
		    DWDα(69) =   -5.0*SinAmΨ(5,1)
		    DWDα(70) =   -3.0*SinAmΨ(3,3)
		    DWDα(71) =   -SinAmΨ(1,5)
		    DWDα(72) =   -2.0*SinAmΨ(2,5)
		    DWDα(73) =   -4.0*SinAmΨ(4,3)
		    DWDα(74) =   -5.0*SinAmΨ(5,3)
		    DWDα(75) =   -3.0*SinAmΨ(3,5)
		    DWDα(76) =   -4.0*SinAmΨ(4,5)
		    DWDα(77) =   -5.0*SinAmΨ(5,5)
		    DWDα(78) =   0.0
		    DWDα(79) =   0.0
		    DWDα(80) =   -2.0*SinApΨ(2,3)
		    DWDα(81) =   -2.0*SinAmΨ(2,3)
		    DWDα(82) =   -2.0*SinApΨ(2,1)
		    DWDα(83) =   -2.0*SinAmΨ(2,1)
		    DWDα(84) =   0.0
		    
		    DWDα(85) =   0.0
		    DWDα(86) =   -2.0*SinApΨ(2,2)
		    DWDα(87) =   -3.0*SinApΨ(3,2)
		    DWDα(88) =   -3.0*SinAmΨ(3,2)
		    DWDα(89) =   -SinApΨ(1,2)
		    DWDα(90) =   -SinAmΨ(1,2)
		    DWDα(91) =   -2.0*SinAmΨ(2,2)
		    DWDα(92) =   0.0
		    DWDα(93) =   -3.0*SinApΨ(3,0)
		    DWDα(94) =   0.0
		    DWDα(95) =   -2.0*SinApΨ(2,0)
		    DWDα(96) =   -SinApΨ(1,0)
		    DWDα(97) =   CosApΨ(1,0)
		    DWDα(98) =   2.0*CosApΨ(2,0)
		    DWDα(99) =   3.0*CosApΨ(3,0)
		    DWDα(100) =   CosAmΨ(1,2)
		    DWDα(101) =   2.0*CosAmΨ(2,2)
		    DWDα(102) =   3.0*CosAmΨ(3,2)
		    DWDα(103) =   0.0
		    DWDα(104) =   CosApΨ(1,2)
		    DWDα(105) =   2.0*CosApΨ(2,2)
		    DWDα(106) =   3.0*CosApΨ(3,2)
		    DWDα(107) =   0.0
		    DWDα(108) =   -2.0*SinApΨ(2,2)
		    DWDα(109) =   -3.0*SinApΨ(3,2)
		    DWDα(110) =   -3.0*SinAmΨ(3,2)
		    DWDα(111) =   -SinApΨ(1,2)
		    DWDα(112) =   -SinAmΨ(1,2)
		    DWDα(113) =   -2.0*SinAmΨ(2,2)
		    DWDα(114) =   0.0
		    DWDα(115) =   -3.0*SinApΨ(3,0)
		    DWDα(116) =   0.0
		    DWDα(117) =   -2.0*SinApΨ(2,0)
		    DWDα(118) =   -SinApΨ(1,0)
		    DWDα(119) =   CosApΨ(1,0)
		    DWDα(120) =   2.0*CosApΨ(2,0)
		    DWDα(121) =   3.0*CosApΨ(3,0)
		    DWDα(122) =   CosAmΨ(1,2)
		    DWDα(123) =   2.0*CosAmΨ(2,2)
		    DWDα(124) =   3.0*CosAmΨ(3,2)
		    DWDα(125) =   0.0
		    DWDα(126) =   CosApΨ(1,2)
		    DWDα(127) =   2.0*CosApΨ(2,2)
		    DWDα(128) =   3.0*CosApΨ(3,2)
		    
		    //DWDΨ
		    
		    DWDΨ(47) =   -2.0*SinApΨ(2,2)
		    DWDΨ(48) =   -2.0*SinApΨ(1,2)
		    DWDΨ(49) =   2.0*SinAmΨ(1,2)
		    DWDΨ(50) =   2.0*SinAmΨ(2,2)
		    DWDΨ(51) =   -2.0*SinApΨ(0,2)
		    DWDΨ(52) =   -5.0*SinApΨ(5,5)
		    DWDΨ(53) =   -SinApΨ(1,1)
		    DWDΨ(54) =   -3.0*SinApΨ(3,3)
		    DWDΨ(55) =   -5.0*SinApΨ(4,5)
		    DWDΨ(56) =   -3.0*SinApΨ(4,3)
		    DWDΨ(57) =   -3.0*SinApΨ(5,3)
		    DWDΨ(58) =   SinAmΨ(1,1)
		    DWDΨ(59) =   -SinApΨ(3,1)
		    DWDΨ(60) =   -5.0*SinApΨ(3,5)
		    DWDΨ(61) =   -3.0*SinApΨ(1,3)
		    DWDΨ(62) =   -5.0*SinApΨ(2,5)
		    DWDΨ(63) =   -SinApΨ(4,1)
		    DWDΨ(64) =   -SinApΨ(5,1)
		    DWDΨ(65) =   SinAmΨ(3,1)
		    DWDΨ(66) =   -5.0*SinApΨ(1,5)
		    DWDΨ(67) =   3.0*SinAmΨ(1,3)
		    DWDΨ(68) =   SinAmΨ(4,1)
		    DWDΨ(69) =   SinAmΨ(5,1)
		    DWDΨ(70) =   3.0*SinAmΨ(3,3)
		    DWDΨ(71) =   5.0*SinAmΨ(1,5)
		    DWDΨ(72) =   5.0*SinAmΨ(2,5)
		    DWDΨ(73) =   3.0*SinAmΨ(4,3)
		    DWDΨ(74) =   3.0*SinAmΨ(5,3)
		    DWDΨ(75) =   5.0*SinAmΨ(3,5)
		    DWDΨ(76) =   5.0*SinAmΨ(4,5)
		    DWDΨ(77) =   5.0*SinAmΨ(5,5)
		    DWDΨ(78) =   -3.0*SinApΨ(0,3)
		    DWDΨ(79) =   -5.0*SinApΨ(0,5)
		    DWDΨ(80) =   -3.0*SinApΨ(2,3)
		    DWDΨ(81) =   3.0*SinAmΨ(2,3)
		    DWDΨ(82) =   -SinApΨ(2,1)
		    DWDΨ(83) =   SinAmΨ(2,1)
		    DWDΨ(84) =   -SinApΨ(0,1)
		    
		    DWDΨ(85) =   0.0
		    DWDΨ(86) =   -2.0*SinApΨ(2,2)
		    DWDΨ(87) =   -2.0*SinApΨ(3,2)
		    DWDΨ(88) =   2.0*SinAmΨ(3,2)
		    DWDΨ(89) =   -2.0*SinApΨ(1,2)
		    DWDΨ(90) =   2.0*SinAmΨ(1,2)
		    DWDΨ(91) =   2.0*SinAmΨ(2,2)
		    DWDΨ(92) =   0.0
		    DWDΨ(93) =   0.0
		    DWDΨ(94) =   -2.0*SinApΨ(0,2)
		    DWDΨ(95) =   0.0
		    DWDΨ(96) =   0.0
		    DWDΨ(97) =   0.0
		    DWDΨ(98) =   0.0
		    DWDΨ(99) =   0.0
		    DWDΨ(100) =  -2.0*CosAmΨ(1,2)
		    DWDΨ(101) =   -2.0*CosAmΨ(2,2)
		    DWDΨ(102) =   -2.0*CosAmΨ(3,2)
		    DWDΨ(103) =   2.0*CosApΨ(0,2)
		    DWDΨ(104) =   2.0*CosApΨ(1,2)
		    DWDΨ(105) =   2.0*CosApΨ(2,2)
		    DWDΨ(106) =   2.0*CosApΨ(3,2)
		    DWDΨ(107) =   0.0
		    DWDΨ(108) =   -2.0*SinApΨ(2,2)
		    DWDΨ(109) =   -2.0*SinApΨ(3,2)
		    DWDΨ(110) =   2.0*SinAmΨ(3,2)
		    DWDΨ(111) =   -2.0*SinApΨ(1,2)
		    DWDΨ(112) =   2.0*SinAmΨ(1,2)
		    DWDΨ(113) =   2.0*SinAmΨ(2,2)
		    DWDΨ(114) =   0.0
		    DWDΨ(115) =   0.0
		    DWDΨ(116) =   -2.0*SinApΨ(0,2)
		    DWDΨ(117) =   0.0
		    DWDΨ(118) =   0.0
		    DWDΨ(119) =   0.0
		    DWDΨ(120) =   0.0
		    DWDΨ(121) =   0.0
		    DWDΨ(122) =   -2.0*CosAmΨ(1,2)
		    DWDΨ(123) =   -2.0*CosAmΨ(2,2)
		    DWDΨ(124) =   -2.0*CosAmΨ(3,2)
		    DWDΨ(125) =   2.0*CosApΨ(0,2)
		    DWDΨ(126) =   2.0*CosApΨ(1,2)
		    DWDΨ(127) =   2.0*CosApΨ(2,2)
		    DWDΨ(128) =   2.0*CosApΨ(3,2)
		    
		    
		  End if 
		  // Factors for H0X
		  W(129) = SinAmΨ(1,2)
		  W(130) = SinAmΨ(2,2)
		  W(131) = SinApΨ(1,2)
		  W(132) = SinApΨ(2,2)
		  
		  DWDα(129) = CosAmΨ(1,2)
		  DWDα(130) = 2.0*CosAmΨ(2,2)
		  DWDα(131) = CosApΨ(1,2)
		  DWDα(132) = 2.0*CosApΨ(2,2)
		  
		  DWDΨ(129) = -2.0*CosAmΨ(1,2)
		  DWDΨ(130) = -2.0*CosAmΨ(2,2)
		  DWDΨ(131) = 2.0*CosApΨ(1,2)
		  DWDΨ(132) = 2.0*CosApΨ(2,2)
		  
		  If Parameters.PNOrder > 0 Then
		    
		    // Factors for H1X
		    W(133) = SinAmΨ(1,3)  // sin(α  - 3Ψ)
		    W(134) = SinAmΨ(2,3)  // sin(2α - 3Ψ)
		    W(135) = SinAmΨ(3,3)  // sin(3α - 3Ψ)
		    W(136)= SinAmΨ(1,1)    // sin(α - Ψ)
		    W(137) = SinAmΨ(2,1)   // sin(2α - Ψ)
		    W(138) = SinAmΨ(3,1)   // sin(3α - Ψ)
		    W(139) = SinApΨ(0,1)   // sin(Ψ)
		    W(140) = SinApΨ(1,1)   // sin(α + Ψ)
		    W(141) = SinApΨ(2,1)   // sin(2α + Ψ)
		    W(142) = SinApΨ(3,1)  // sin(3α + Ψ)
		    W(143) = SinApΨ(1,3)  // sin(α + 3Ψ)
		    W(144) = SinApΨ(2,3) // sin(2α + 3Ψ)
		    W(145) = SinApΨ(3,3) // sin(3α + 3Ψ)
		    
		    DWDα(133) = CosAmΨ(1,3) // derivative of sin(α + 3Ψ) with respect to α
		    DWDα(134) = 2.0*CosAmΨ(2,3)
		    DWDα(135) = 3.0*CosAmΨ(3,3)
		    DWDα(136) = CosAmΨ(1,1)
		    DWDα(137) = 2.0*CosAmΨ(2,1)
		    DWDα(138) = 3.0*CosAmΨ(3,1)
		    DWDα(139) = 0.0
		    DWDα(140) = CosApΨ(1,1)
		    DWDα(141) = 2.0*CosApΨ(2,1)
		    DWDα(142) = 3.0*CosApΨ(3,1)
		    DWDα(143) = CosApΨ(1,3)
		    DWDα(144) = 2.0*CosApΨ(2,3)
		    DWDα(145) = 3.0*CosApΨ(3,3)
		    
		    DWDΨ(133) = -3.0*CosAmΨ(1,3) // derivative of sin(α + 3Ψ) with respect to α
		    DWDΨ(134) = 3.0*CosAmΨ(2,3)
		    DWDΨ(135) = -3.0*CosAmΨ(3,3)
		    DWDΨ(136) = -1.0*CosAmΨ(1,1)
		    DWDΨ(137) = -1.0*CosAmΨ(2,1)
		    DWDΨ(138) = -1.0*CosAmΨ(3,1)
		    DWDΨ(139) = CosApΨ(0,1)
		    DWDΨ(140) = CosApΨ(1,1)
		    DWDΨ(141) = CosApΨ(2,1)
		    DWDΨ(142) = CosApΨ(3,1)
		    DWDΨ(143) = 3.0*CosApΨ(1,3)
		    DWDΨ(144) = 3.0*CosApΨ(2,3)
		    DWDΨ(145) = 3.0*CosApΨ(3,3)
		    
		  End if 
		  
		  If Parameters.PNOrder > 1 Then
		    
		    // Factors for H2X
		    W(146) = SinAmΨ(1,4)
		    W(147) = SinAmΨ(2,4)
		    W(148) = SinAmΨ(3,4)
		    W(149) = SinAmΨ(4,4)
		    W(150) = SinAmΨ(1,2)
		    W(151) = SinAmΨ(2,2)
		    W(152) = SinAmΨ(3,2)
		    W(153) = SinAmΨ(4,2)
		    W(154) = SinApΨ(0,2)
		    W(155) = SinApΨ(1,2)
		    W(156) = SinApΨ(2,2)
		    W(157) = SinApΨ(3,2)
		    W(158) = SinApΨ(4,2)
		    W(159) = SinApΨ(1,4)
		    W(160) = SinApΨ(2,4)
		    W(161) = SinApΨ(3,4)
		    W(162) = SinApΨ(4,4)
		    
		    W(163) = CosApΨ(1,1)
		    W(164) = CosAmΨ(1,1)
		    W(165) = SinAmΨ(1,1)
		    W(166) = SinApΨ(0,1)
		    W(167) = SinApΨ(1,1)
		    W(168) = CosApΨ(1,1)
		    W(169) = CosAmΨ(1,1)
		    W(170) = SinAmΨ(1,1)
		    W(171) = SinApΨ(0,1)
		    W(172) = SinApΨ(1,1)
		    
		    
		    DWDα(146) = CosAmΨ(1,4)
		    DWDα(147) = 2.0*CosAmΨ(2,4)
		    DWDα(148) = 3.0*CosAmΨ(3,4)
		    DWDα(149) = 4.0*CosAmΨ(4,4)
		    DWDα(150) = CosAmΨ(1,2)
		    DWDα(151) = 2.0*CosAmΨ(2,2)
		    DWDα(152) = 3.0*CosAmΨ(3,2)
		    DWDα(153) = 4.0*CosAmΨ(4,2)
		    DWDα(154) = 0.0
		    DWDα(155) = CosApΨ(1,2)
		    DWDα(156) = 2.0*CosApΨ(2,2)
		    DWDα(157) = 3.0*CosApΨ(3,2)
		    DWDα(158) = 4.0*CosApΨ(4,2)
		    DWDα(159) = CosApΨ(1,4)
		    DWDα(160) = 2.0*CosApΨ(2,4)
		    DWDα(161) = 3.0*CosApΨ(3,4)
		    DWDα(162) = 4.0*CosApΨ(4,4)
		    
		    DWDα(163) = -SinApΨ(1,1)
		    DWDα(164) = -SinAmΨ(1,1)
		    DWDα(165) = CosAmΨ(1,1)
		    DWDα(166) = 0.0
		    DWDα(167) = CosApΨ(1,1)
		    DWDα(168) = -SinApΨ(1,1)
		    DWDα(169) = -SinAmΨ(1,1)
		    DWDα(170) = CosAmΨ(1,1)
		    DWDα(171) = 0.0
		    DWDα(172) = CosApΨ(1,1)
		    
		    
		    DWDΨ(146) = -4.0*CosAmΨ(1,4)
		    DWDΨ(147) = -4.0*CosAmΨ(2,4)
		    DWDΨ(148) = -4.0*CosAmΨ(3,4)
		    DWDΨ(149) = -4.0*CosAmΨ(4,4)
		    DWDΨ(150) = -2.0*CosAmΨ(1,2)
		    DWDΨ(151) = -2.0*CosAmΨ(2,2)
		    DWDΨ(152) = -2.0*CosAmΨ(3,2)
		    DWDΨ(153) = -2.0*CosAmΨ(4,2)
		    DWDΨ(154) = 2.0*CosApΨ(0,2)
		    DWDΨ(155) = 2.0*CosApΨ(1,2)
		    DWDΨ(156) = 2.0*CosApΨ(2,2)
		    DWDΨ(157) = 2.0*CosApΨ(3,2)
		    DWDΨ(158) = 2.0*CosApΨ(4,2)
		    DWDΨ(159) = 4.0*CosApΨ(1,4)
		    DWDΨ(160) = 4.0*CosApΨ(2,4)
		    DWDΨ(161) = 4.0*CosApΨ(3,4)
		    DWDΨ(162) = 4.0*CosApΨ(4,4)
		    
		    DWDΨ(163) = -SinApΨ(1,1)
		    DWDΨ(164) = SinAmΨ(1,1)
		    DWDΨ(165) = -1.0*CosAmΨ(1,1)
		    DWDΨ(166) = CosApΨ(0,1)
		    DWDΨ(167) = CosApΨ(1,1)
		    DWDΨ(168) = -SinApΨ(1,1)
		    DWDΨ(169) = SinAmΨ(1,1)
		    DWDΨ(170) = -1.0*CosAmΨ(1,1)
		    DWDΨ(171) = CosApΨ(0,1)
		    DWDΨ(172) = CosApΨ(1,1)
		    
		    
		  End if 
		  
		  If Parameters.PNOrder > 2 Then 
		    
		    // Factors for H3X
		    
		    W(173) = SinAmΨ(1,2)
		    W(174) = SinAmΨ(2,2)
		    W(175) = SinApΨ(1,2)
		    W(176) = SinApΨ(2,2)
		    W(177) = SinAmΨ(1,5)
		    W(178) = SinAmΨ(2,5)
		    W(179) = SinAmΨ(3,5)
		    W(180) = SinAmΨ(4,5)
		    W(181) = SinAmΨ(5,5)
		    W(182) = SinAmΨ(1,3)
		    W(183) = SinAmΨ(2,3)
		    W(184) = SinAmΨ(3,3)
		    W(185) = SinAmΨ(4,3)
		    W(186) = SinAmΨ(5,3)
		    W(187) = SinAmΨ(1,1)
		    W(188) = SinAmΨ(2,1)
		    W(189) = SinAmΨ(3,1)
		    W(190) = SinAmΨ(4,1)
		    W(191) = SinAmΨ(5,1)
		    W(192) = SinApΨ(0,1)
		    W(193) = SinApΨ(0,3)
		    W(194) = SinApΨ(1,1)
		    W(195) = SinApΨ(2,1)
		    W(196) = SinApΨ(1,3)
		    W(197) = SinApΨ(2,3)
		    W(198) = SinApΨ(3,3)
		    W(199) = SinApΨ(4,3)
		    W(200) = SinApΨ(5,3)
		    W(201) = SinApΨ(1,5)
		    W(202) = SinApΨ(2,5)
		    W(203) = SinApΨ(3,5)
		    W(204) = SinApΨ(4,5)
		    W(205) = SinApΨ(5,5)
		    
		    W(206) = CosApΨ(0,0)
		    W(207) = CosApΨ(2,2)
		    W(208) = CosApΨ(3,2)
		    W(209) = CosApΨ(1,2)
		    W(210) = CosAmΨ(1,2)
		    W(211) = CosAmΨ(2,2)
		    W(212) = CosAmΨ(3,2)
		    W(213) = CosApΨ(2,0)
		    W(214) = CosApΨ(0,2)
		    W(215) = CosApΨ(3,0)
		    W(216) = CosApΨ(1,0)
		    W(217) = SinApΨ(1,0)
		    W(218) = SinApΨ(2,0)
		    W(219) = SinApΨ(3,0)
		    W(220) = SinAmΨ(1,2)
		    W(221) = SinAmΨ(2,2)
		    W(222) = SinAmΨ(3,2)
		    W(223) = SinApΨ(0,2)
		    W(224) = SinApΨ(1,2)
		    W(225) = SinApΨ(2,2)
		    W(226) = SinApΨ(3,2)
		    W(227) = CosApΨ(0,0)
		    W(228) = CosApΨ(2,2)
		    W(229) = CosApΨ(1,2)
		    W(230) = CosApΨ(3,2)
		    W(231) = CosAmΨ(1,2)
		    W(232) = CosAmΨ(2,2)
		    W(233) = CosAmΨ(3,2)
		    W(234) = CosApΨ(2,0)
		    W(235) = CosApΨ(0,2)
		    W(236) = CosApΨ(3,0)
		    W(237) = CosApΨ(1,0)
		    W(239) = SinApΨ(2,0)
		    W(240) = SinApΨ(3,0)
		    W(241) = SinAmΨ(1,2)
		    W(242) = SinAmΨ(2,2)
		    W(243) = SinAmΨ(3,2)
		    W(244) = SinApΨ(0,2)
		    W(245) = SinApΨ(1,2)
		    W(246) = SinApΨ(2,2)
		    W(247) = SinApΨ(3,2)
		    
		    
		    //DWDα
		    
		    DWDα(173) = CosAmΨ(1,2)
		    DWDα(174) = 2.0*CosAmΨ(2,2)
		    DWDα(175) = CosApΨ(1,2)
		    DWDα(176) = 2.0*CosApΨ(2,2)
		    DWDα(177) = CosAmΨ(1,5)
		    DWDα(178) = 2.0*CosAmΨ(2,5)
		    DWDα(179) = 3.0*CosAmΨ(3,5)
		    DWDα(180) = 4.0*CosAmΨ(4,5)
		    DWDα(181) = 5.0*CosAmΨ(5,5)
		    DWDα(182) = CosAmΨ(1,3)
		    DWDα(183) = 2.0*CosAmΨ(2,3)
		    DWDα(184) = 3.0*CosAmΨ(3,3)
		    DWDα(185) = 4.0*CosAmΨ(4,3)
		    DWDα(186) = 5.0*CosAmΨ(5,3)
		    DWDα(187) = CosAmΨ(1,1)
		    DWDα(188) = 2.0*CosAmΨ(2,1)
		    DWDα(189) = 3.0*CosAmΨ(3,1)
		    DWDα(190) = 4.0*CosAmΨ(4,1)
		    DWDα(191) = 5.0*CosAmΨ(5,1)
		    DWDα(192) = 0.0
		    DWDα(193) = 0.0
		    DWDα(194) = CosApΨ(1,1)
		    DWDα(195) = 2.0*CosApΨ(2,1)
		    DWDα(196) = CosApΨ(1,3)
		    DWDα(197) = 2.0*CosApΨ(2,3)
		    DWDα(198) = 3.0*CosApΨ(3,3)
		    DWDα(199) = 3.0*CosApΨ(4,3)
		    DWDα(200) = 5.0*CosApΨ(5,3)
		    DWDα(201) = CosApΨ(1,5)
		    DWDα(202) = 2.0*CosApΨ(2,5)
		    DWDα(203) = 3.0*CosApΨ(3,5)
		    DWDα(204) = 4.0*CosApΨ(4,5)
		    DWDα(205) = 5.0*CosApΨ(5,5)
		    
		    DWDα(206) = 0.0
		    DWDα(207) = -2.0*SinApΨ(2,2)
		    DWDα(208) = -3.0*SinApΨ(3,2)
		    DWDα(209) = -SinApΨ(1,2)
		    DWDα(210) = -SinAmΨ(1,2)
		    DWDα(211) = -2.0*SinAmΨ(2,2)
		    DWDα(212) = -3.0*SinAmΨ(3,2)
		    DWDα(213) = -2.0*SinApΨ(2,0)
		    DWDα(214) = 0.0
		    DWDα(215) = -3.0*SinApΨ(3,0)
		    DWDα(216) = -SinApΨ(1,0)
		    DWDα(217) = CosApΨ(1,0)
		    DWDα(218) = 2.0*CosApΨ(2,0)
		    DWDα(219) = 3.0*CosApΨ(3,0)
		    DWDα(220) = CosAmΨ(1,2)
		    DWDα(221) = 3.0*CosAmΨ(2,2)
		    DWDα(222) = 3.0*CosAmΨ(3,2)
		    DWDα(223) = 0.0
		    DWDα(224) = CosApΨ(1,2)
		    DWDα(225) = 2.0*CosApΨ(2,2)
		    DWDα(226) = 3.0*CosApΨ(3,2)
		    DWDα(227) = 0.0
		    DWDα(228) = -2.0*SinApΨ(2,2)
		    DWDα(229) = -SinApΨ(1,2)
		    DWDα(230) = -3.0*SinApΨ(3,2)
		    DWDα(231) = -SinAmΨ(1,2)
		    DWDα(232) = -2.0*SinAmΨ(2,2)
		    DWDα(233) = -3.0*SinAmΨ(3,2)
		    DWDα(234) = -2.0*SinApΨ(2,0)
		    DWDα(235) = 0.0
		    DWDα(236) = -3.0*SinApΨ(3,0)
		    DWDα(237) = -SinApΨ(1,0)
		    DWDα(239) = 2.0*CosApΨ(2,0)
		    DWDα(240) = 3.0*CosApΨ(3,0)
		    DWDα(241) = CosAmΨ(1,2)
		    DWDα(242) = 2.0*CosAmΨ(2,2)
		    DWDα(243) = 3.0*CosAmΨ(3,2)
		    DWDα(244) = 0.0
		    DWDα(245) = CosApΨ(1,2)
		    DWDα(246) = 2.0*CosApΨ(2,2)
		    DWDα(247) = 3.0*CosApΨ(3,2)
		    
		    
		    //DWDΨ
		    
		    DWDΨ(173) = -2.0*CosAmΨ(1,2)
		    DWDΨ(174) = -2.0*CosAmΨ(2,2)
		    DWDΨ(175) = 2.0*CosApΨ(1,2)
		    DWDΨ(176) = 2.0*CosApΨ(2,2)
		    DWDΨ(177) = -5.0*CosAmΨ(1,5)
		    DWDΨ(178) = -5.0*CosAmΨ(2,5)
		    DWDΨ(179) = -5.0*CosAmΨ(3,5)
		    DWDΨ(180) = 05.0*CosAmΨ(4,5)
		    DWDΨ(181) = -5.0*CosAmΨ(5,5)
		    DWDΨ(182) = -3.0*CosAmΨ(1,3)
		    DWDΨ(183) = -3.0*CosAmΨ(2,3)
		    DWDΨ(184) = -3.0*CosAmΨ(3,3)
		    DWDΨ(185) = -3.0*CosAmΨ(4,3)
		    DWDΨ(186) = -3.0*CosAmΨ(5,3)
		    DWDΨ(187) = -1.0*CosAmΨ(1,1)
		    DWDΨ(188) = -1.0*CosAmΨ(2,1)
		    DWDΨ(189) = -1.0*CosAmΨ(3,1)
		    DWDΨ(190) = -1.0*CosAmΨ(4,1)
		    DWDΨ(191) = -1.0*CosAmΨ(5,1)
		    DWDΨ(192) = CosApΨ(0,1)
		    DWDΨ(193) = 3.0*CosApΨ(0,3)
		    DWDΨ(194) = CosApΨ(1,1)
		    DWDΨ(195) = CosApΨ(2,1)
		    DWDΨ(196) = 3.0*CosApΨ(1,3)
		    DWDΨ(197) = 3.0*CosApΨ(2,3)
		    DWDΨ(198) = 3.0*CosApΨ(3,3)
		    DWDΨ(199) = 3.0*CosApΨ(4,3)
		    DWDΨ(200) = 3.0*CosApΨ(5,3)
		    DWDΨ(201) = 5.0*CosApΨ(1,5)
		    DWDΨ(202) = 5.0*CosApΨ(2,5)
		    DWDΨ(203) = 5.0*CosApΨ(3,5)
		    DWDΨ(204) = 5.0*CosApΨ(4,5)
		    DWDΨ(205) = 5.0*CosApΨ(5,5)
		    
		    DWDΨ(206) = 0.0
		    DWDΨ(207) = -2.0*SinApΨ(2,2)
		    DWDΨ(208) = -2.0*SinApΨ(3,2)
		    DWDΨ(209) = -2.0*SinApΨ(1,2)
		    DWDΨ(210) = 2.0*SinAmΨ(1,2)
		    DWDΨ(211) = 2.0*SinAmΨ(2,2)
		    DWDΨ(212) = 2.0*SinAmΨ(3,2)
		    DWDΨ(213) = 0.0
		    DWDΨ(214) = -2.0*SinApΨ(0,2)
		    DWDΨ(215) = 0.0
		    DWDΨ(216) = 0.0
		    DWDΨ(217) = 0.0
		    DWDΨ(218) = 0.0
		    DWDΨ(219) = 0.0
		    DWDΨ(220) = -2.0*CosAmΨ(1,2)
		    DWDΨ(221) = -2.0*CosAmΨ(2,2)
		    DWDΨ(222) = -2.0*CosAmΨ(3,2)
		    DWDΨ(223) = 2.0*CosApΨ(0,2)
		    DWDΨ(224) = 2.0*CosApΨ(1,2)
		    DWDΨ(225) = 2.0*CosApΨ(2,2)
		    DWDΨ(226) = 2.0*CosApΨ(3,2)
		    DWDΨ(227) = 0
		    DWDΨ(228) = -2.0*SinApΨ(2,2)
		    DWDΨ(229) = -2.0*SinApΨ(1,2)
		    DWDΨ(230) = -2.0*SinApΨ(3,2)
		    DWDΨ(231) = 2.0*SinAmΨ(1,2)
		    DWDΨ(232) = 2.0*SinAmΨ(2,2)
		    DWDΨ(233) = 2.0*SinAmΨ(3,2)
		    DWDΨ(234) = 0.0
		    DWDΨ(235) = -2.0*SinApΨ(0,2)
		    DWDΨ(236) = 0.0
		    DWDΨ(237) = 0.0
		    DWDΨ(239) = 0.0
		    DWDΨ(240) = 0.0
		    DWDΨ(241) = -2.0*CosAmΨ(1,2)
		    DWDΨ(242) = -2.0*CosAmΨ(2,2)
		    DWDΨ(243) = -2.0*CosAmΨ(3,2)
		    DWDΨ(244) = 2.0*CosApΨ(0,2)
		    DWDΨ(245) = 2.0*CosApΨ(1,2)
		    DWDΨ(246) = 2.0*CosApΨ(2,2)
		    DWDΨ(247) = 2.0*CosApΨ(3,2)
		    
		    
		  End if 
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(P As CaseInfoClass)
		  // Initialize constants
		  Parameters = P
		  Cosβ = Cos(P.β)
		  Sinβ = Sin(P.β)
		  δ = P.δ
		  η = 0.25*(1.0 - δ*δ)
		  π = P.π
		  VeSinΘ = P.Ve*Sin(P.Θ)
		  VeCosΘ = P.Ve*Cos(P.Θ)
		  Δτr = P.ΔT/P.GM
		  Δτ = Δτr/(1.0 + P.Z)
		  SpinEvolver = New SpinEvolverClass(P)
		  
		  // Initialize the Noise class
		  Noise = New NoiseClass(Parameters.ΔT)
		  
		  // Set up the base case
		  'SourceEvolverBase = New SourceEvolverClass(P)
		  '// We need the following for calculating the z-derivative
		  'α0 = SourceEvolverBase.αN
		  'ι0 = SourceEvolverBase.ιN
		  'χax0 = SourceEvolverBase.χaXN
		  'χay0 = SourceEvolverBase.χaYN
		  'χaz0 = SourceEvolverBase.χaZN
		  'χsx0 = SourceEvolverBase.χsXN
		  'χsy0 = SourceEvolverBase.χsYN
		  'χsz0 = SourceEvolverBase.χsZN
		  '
		  '// Set up source evolvers where the value of δ is tweaked
		  'Var ε As Double = 1.0e-6
		  'SourceEvolverδMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.delta), -ε))
		  'SourceEvolverδPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.delta), ε))
		  'IDεForδ = 0.5/ε
		  '
		  '// Set up source evolvers where the value of v0 is adjusted
		  'ε = 1.0e-6
		  'SourceEvolverV0Minus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.v0), -ε))
		  'SourceEvolverV0Plus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.v0), ε))
		  'IDεForV0 = 0.5/ε
		  '
		  '// Set up source evolvers where the value of χ10x is adjusted
		  'ε = 1.0e-6
		  'SourceEvolverχ10xMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10x), -ε))
		  'SourceEvolverχ10xPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10x), ε))
		  'IDεForχ10x = 0.5/ε
		  '
		  '// Set up source evolvers where the value of χ10y is adjusted
		  'ε = 1.0e-6
		  'SourceEvolverχ10yMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10y), -ε))
		  'SourceEvolverχ10yPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10y), ε))
		  'IDεForχ10y = 0.5/ε
		  '
		  '// Set up source evolvers where the value of χ10z is adjusted
		  'ε = 1.0e-6
		  'SourceEvolverχ10zMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10z), -ε))
		  'SourceEvolverχ10zPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10z), ε))
		  'IDεForχ10z = 0.5/ε
		  '
		  '// Set up source evolvers where the value of χ20x is adjusted
		  'ε = 1.0e-6
		  'SourceEvolverχ20xMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20x), -ε))
		  'SourceEvolverχ20xPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20x), ε))
		  'IDεForχ20x = 0.5/ε
		  '
		  '// Set up source evolvers where the value of χ20y is adjusted
		  'ε = 1.0e-6
		  'SourceEvolverχ20yMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20y), -ε))
		  'SourceEvolverχ20yPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20y), ε))
		  'IDεForχ20y = 0.5/ε
		  '
		  '// Set up source evolvers where the value of χ20z is adjusted
		  'ε = 1.0e-6
		  'SourceEvolverχ20zMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20z), -ε))
		  'SourceEvolverχ20zPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20z), ε))
		  'IDεForχ20z = 0.5/ε
		  '
		  '// Calculate derivative of Z with respect to lnR
		  'DZDlnR = P.R*P.DZDR
		  '
		  '// Get the value of the detector time step (sampling interval)
		  'DτrD = P.ΔT/P.GM
		  '// do a trial step to get a value of DτIdeal.
		  'SourceBestDτr = 1.0e300 // Initialize this to be something huge
		  '// Note that DτIdeal is passed by reference, so each case has an opportunity to
		  '// tweak its value. This is necessary because the base case may have no spin,
		  '// while some side cases might have a spin that requires a certain step size.
		  '// A first argument of zero indicates a trial step here.
		  'SourceEvolverBase.DoStep(-1.0, DτrD, SourceBestDτr)
		  'If P.SolveFor(Integer(CaseInfoClass.Param.delta)) Then
		  'SourceEvolverδMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverδPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'End If
		  'If P.SolveFor(Integer(CaseInfoClass.Param.V0)) Then
		  'SourceEvolverV0Minus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverV0Plus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'End If
		  'If P.SolveForχ1 Then
		  'SourceEvolverχ10xMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverχ10xPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverχ10yMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverχ10yPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverχ10zMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverχ10zPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'End If
		  'If P.SolveForχ2 Then
		  'SourceEvolverχ20xMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverχ20xPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverχ20yMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverχ20yPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverχ20zMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'SourceEvolverχ20zPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		  'End If
		  'SourceBestDτr = SourceBestDτr*(1.0 + P.Z)  // get this step size in the solar system frame
		  '
		  '// Now set up the actual first time step
		  '// The ratio of the real future step will be some power of two of the detector sample step size.
		  '// Compute that power of two
		  'Var NewStepPower as Integer = Floor(Log(SourceBestDτr/DτrD)/Log(2))
		  'StepPowerF = NewStepPower // initalize the CurrentStepPower
		  'StepPowerP = NewStepPower
		  'DτrSF = DτrD*2^StepPowerF  // and initialize DτrSF (the time interval between the present and future source steps)
		  'DτrSP = 0.0 // This will indicate a first step
		  'SourceNow = 0
		  'SourcePast = 0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DidDetectorStepOK(StepNumber As Integer) As Boolean
		  // If we are within two detector steps of coalescence, bail out
		  If (StepNumber + 2)*Δτ > Parameters.τc Then
		    Return False
		  End If
		  
		  // Otherwise, check if we can get data from the spin evolver
		  Var spinData As SpinDataClass = SpinEvolver.GetSpinDataAtTime(StepNumber*Δτ)
		  If spinData = Nil Then Return False  // If the method returns nothing, coalescence must have happened
		  
		  // We have data, so
		  VDN = spinData.V // get the current speed
		  // If our speed is half that of light, our approximations are breaking down, so bail out
		  If VDN > 0.5 Then Return False
		  
		  // Otherwise, load the rest of the data coming from the spin evolver
		  ιDN = spinData.ι
		  αDN = spinData.α
		  χaxDN = spinData.χax
		  χayDN = spinData.χay
		  χazDN = spinData.χaz
		  χsxDN = spinData.χsx
		  χsyDN = spinData.χsy
		  χszDN = spinData.χsz
		  
		  // Calculate the wave phase
		  Var τrm As Double = (StepNumber - 0.5)*Δτr
		  ΨrDN = spinData.Ψ
		  // do the following instead of the above if we want the data in the orbiting LISA frame
		  // ΨrDN = ΨrDP + (1.0 + Parameters.Ve*Sin(Parameters.Θ)*Sin(Parameters.GMΩe*τrm - Parameters.Φ))*(spinData.Ψ - ΨP)
		  ΨrDP = ΨrDN
		  ΨP = spinData.Ψ
		  τrDN = StepNumber*Δτr
		  
		  // Calculate the wave
		  CalculateAmplitudes
		  CalculateWaveFactors
		  SumSourceH(W)
		  
		  // Write out useful information for plotting (if this is not a case from a file)
		  If Not Parameters.FromFile Then
		    Var t As Double = τrDN*Parameters.GM/Parameters.Year
		    Var myValues() As Double = Array(t, HP, HX)
		    Parameters.DataWriter.WriteDataToMemory(myValues)
		  End If
		  
		  // We have completed the detector step successfully
		  Return True
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SumSourceH(Wave() As Double, DoVDeriv As Boolean = False)
		  // These constants define static variables indicating the endpoints of certain polarizations
		  Static H0PLastIndex As Integer = 4
		  Static H1PLastIndex As integer = 18
		  Static H2PLastIndex As Integer = 46
		  Static H3PLastIndex As Integer = 128
		  Static H0XLastIndex As Integer = 132
		  Static H1XLastIndex As Integer = 145
		  Static H2XLastIndex As Integer = 172
		  Static H3XLastIndex As Integer = 247
		  
		  // First, do the plus polarization
		  Var jStart As Integer
		  Var sum As Double
		  For j As Integer = 0 To H0PLastIndex
		    sum = sum + A(j)*W(j)
		  Next
		  Var vpower As Double
		  If DoVDeriv Then
		    If DoVDeriv Then vpower = 2.0*VDN
		  Else
		    vpower = VDN*VDN
		  End If
		  HP = sum*vpower
		  
		  If Parameters.PNOrder > 0 Then
		    sum = 0.0
		    jStart = H0PLastIndex + 1
		    For j As Integer = jStart to H1PLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VDN
		    If DoVDeriv Then vpower = 1.5*vpower
		    HP = HP + sum*vpower
		  End If
		  
		  If Parameters.PNOrder > 1 Then
		    sum = 0.0
		    jStart = H1PLastIndex + 1
		    For j As Integer = jStart to H2PLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VDN
		    If DoVDeriv Then vpower = 1.33333333333333333*vpower
		    HP = HP + sum*vpower
		  End If
		  
		  If Parameters.PNOrder > 2 Then
		    jStart = H2PLastIndex + 1
		    sum = 0.0
		    For j As Integer = jStart to H3PLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VDN
		    If DoVDeriv Then vpower = 1.25*vpower
		    HP = HP + sum*vpower
		  End If
		  
		  // now assemble cross polarization
		  sum = 0.0
		  vpower = VDN*VDN
		  jStart = H3PLastIndex + 1
		  For j As Integer = jStart To H0PLastIndex
		    sum = sum + A(j)*Wave(j)
		  Next
		  If DoVDeriv Then
		    vpower = 2.0*VDN
		  Else
		    vpower = VDN*VDN
		  End If
		  HX = sum*vpower
		  
		  If Parameters.PNOrder > 0 Then
		    sum = 0.0
		    jStart = H0XLastIndex + 1
		    For j As Integer = jStart to H1PLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VDN
		    If DoVDeriv Then vpower = 1.5*vpower
		    HX = HX + sum*vpower
		  End If
		  
		  If Parameters.PNOrder > 1 Then
		    sum = 0.0
		    jStart = H1XLastIndex + 1
		    For j As Integer = jStart to H2PLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VDN
		    If DoVDeriv Then vpower = 1.3333333333333333*vpower
		    HX = HX + sum*vpower
		  End If
		  
		  If Parameters.PNOrder > 2 Then
		    jStart = H2XLastIndex + 1
		    sum = 0.0
		    For j As Integer = jStart to H3XLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VDN
		    If DoVDeriv Then vpower = 1.25*vpower
		    HX = HX + sum*vpower
		  End If
		  
		  // Calculate overall wave amplitude constant
		  Var h0 As Double = 0.5*(1.0 - Parameters.δ*Parameters.δ)*Parameters.GM/(Parameters.R)
		  HP = h0*HP
		  HX = h0*HX
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		A(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CosAmΨ(5,5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CosApΨ(5,5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Cosβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DetectorNow As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		DHDq(14) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DWDα(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DWDΨ(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DZDlnR As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΘDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΦDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		HP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		HX As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Noise As NoiseClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseInfoClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SinAmΨ(5,5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SinApΨ(5,5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sinβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sn(5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SpinEvolver As SpinEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VeCosΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VeSinΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		W(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ιDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		δ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Δτ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Δτr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		η As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		π As Double = 3.1415926535897
	#tag EndProperty

	#tag Property, Flags = &h0
		τrDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaxDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χayDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χazDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsxDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsyDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χszDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨP As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrDP As Double
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
			Name="Δτr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χayDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χazDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsxDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsyDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χszDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DZDlnR"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Cosβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sinβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="HP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="HX"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΘDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΦDN"
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
			Name="DetectorNow"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VeCosΘ"
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
			Name="π"
			Visible=false
			Group="Behavior"
			InitialValue="3.1415926535897"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaxDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ιDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Δτ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrDP"
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
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="τrDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
