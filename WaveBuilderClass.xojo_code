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
		  
		  // Assemble the base case situation
		  ΨrDP = ΨrDN // store the current received phase value as the past value before we update
		  ΨDP = ΨDN // and the same for the source phase
		  DΨrDΘDP = DΨrDΘDN  // and the same for the phase derivatives
		  DΨrDΦDP = DΨrDΦDN
		  GetDataAtDetectorStep(SourceEvolverBase) // get the data from the base case at the present step
		  // now we need to update the phase. The method just above will get ΨDN
		  // note that we have set up τrDN and τrDP in t DidDetectorStepOK method)he
		  Var orbitArg As Double = Parameters.GMΩe*0.5*(τrDN + τrDP) - Parameters.Φ
		  ΨrDN = ΨrDP + (1.0 + VeSinΘ*Sin(orbitArg))*(ΨDN - ΨDP)  // update the received phase to the present
		  DΨrDΘDN = DΨrDΘDP + VeCosΘ*Sin(orbitArg)*(ΨDN - ΨDP)  // update the derivative with respect to Θ
		  DΨrDΦDN = DΨrDΦDP - VeSinΘ*Cos(orbitArg)*(ΨDN - ΨDP)  // update the derivative with respect to Φ
		  // Now that we have the current value of the received phase, we can calculate the wave factors, amplitudes, and assemble the wave
		  CalculateWaveFactors
		  CalculateAmplitudes
		  SumSourceH(W)  // this will put the total plus and  cross polarizations into HP and HX
		  
		  // Store valuable variables for later use
		  Var hpBase As Double = HP
		  Var hxBase As Double = HX
		  Var hBase As Double = fp*HP + fx*HX
		  SumSourceH(DWDα)
		  Var dHDα As Double = fp*HP + fx*HX
		  SumSourceH(DWDΨ)
		  Var dHDΨr As Double = fp*HP + fx*HX
		  // This gets the derivative of the amplitude part of the wave with respect to V
		  SumSourceH(W, True)
		  Var dHDV As Double = fp*HP + fx*HX
		  
		  // Calculate the derivative with respect to M (this is the easy one!)
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.M)) Then
		    DHDq(Integer(CaseInfoClass.Param.M)) = hBase
		  Else
		    DHDq(Integer(CaseInfoClass.Param.M)) = 0.0
		  End If
		  
		  // Calculate the derivative with respect to ψ (this is the next easiest!)
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.psi)) Then
		    DHDq(Integer(CaseInfoClass.Param.psi)) = 2.0*(-fx*hpBase + fp*hxBase)
		  Else
		    DHDq(Integer(CaseInfoClass.Param.psi)) = 0.0
		  End If
		  
		  // in the case of λ0, DΨrDλ0 = 1, so the following is the correct total derivative.
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.lambda0)) Then
		    DHDq(Integer(CaseInfoClass.Param.lambda0)) = dHDΨr
		  Else
		    DHDq(Integer(CaseInfoClass.Param.lambda0)) = 0.0
		  End If
		  
		  // We can also use the above items to calculate the derivative with respect to Θ
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.theta)) Then
		    DHDq(Integer(CaseInfoClass.Param.theta)) = dfp1dΘ*hpBase + dfp2dΘ*hpBase + dfx1dΘ*hxBase + dfx2dΘ*hxBase _
		    + dHDΨr*DΨrDΘDN
		  Else
		    DHDq(Integer(CaseInfoClass.Param.theta)) = 0.0
		  End If
		  
		  // and the derivative with respect to Φ
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.phi)) Then
		    DHDq(Integer(CaseInfoClass.Param.phi)) = dfp1dΦ*hpBase + dfp2dΦ*hpBase + dfx1dΦ*hxBase + dfx2dΦ*hxBase _
		    + dHDΨr*DΨrDΦDN
		  Else
		    DHDq(Integer(CaseInfoClass.Param.phi)) = 0.0
		  End If
		  
		  // Now we will start calculating derivatives that involve derivatives of the wave amplitudes
		  
		  Var originalValue As Double
		  Var originalValue2 As Double
		  Var hPlus As Double
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.beta)) Then
		    // First, calculate the derivative with respect to β
		    originalValue = Cosβ // Store these values for safekeeping
		    originalValue2 = Sinβ
		    Cosβ = CosβPlus  // Reset their values to the plus tweaked versions
		    Sinβ = SinβPlus
		    CalculateAmplitudes  // calculate amplitudes using these tweaked values
		    SumSourceH(W)  // and calculate the waves with these amplitudes
		    hPlus= fp*HP + fx*HX  // save the results for later
		    Cosβ = CosβMinus  // now reset the values of Cosβ, Sinβ to the minus tweaked version
		    Sinβ = SinβMinus
		    CalculateAmplitudes // calculate the amplitudes using these tweaked values
		    SumSourceH(W) // and calculate the waves
		    Cosβ = originalValue  // restore the original values of of Cosβ, Sinβ, so that no harm is done
		    Sinβ = originalValue2
		    DHDq(Integer(CaseInfoClass.Param.beta)) = (hPlus - fp*HP - fx*HX)*IDεForβ  // This gives us the complete β-derivative
		  Else
		    DHDq(Integer(CaseInfoClass.Param.beta)) = 0.0
		  End If
		  
		  // Most of the remaining parameters require all or nearly all the following amplitude derivatives
		  // because varying the parameters have either implicit or explicit effects on the quantities that
		  // the wave amplitudes depend on.
		  
		  // Calculate amplitude derivative with respect to ι
		  Var ε As Double = 1.0e-5
		  originalValue = ιDN
		  ιDN = ιDN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  ιDN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  ιDN = originalValue
		  Var dHDι As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to δ
		  ε = 1.0e-5  // (one can reset this for individual cases if desired without affecting others)
		  originalValue = δ
		  originalValue2 = η
		  δ = δ + ε
		  η = 0.25*(1.0-δ*δ)
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  δ = originalValue - ε
		  η = 0.25*(1.0-δ*δ)
		  CalculateAmplitudes
		  SumSourceH(W)
		  δ = originalValue
		  η = originalValue2
		  Var dHDδ As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to χax
		  ε = 1.0e-5
		  originalValue = χaxDN
		  χaxDN = χaxDN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χaxDN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χaxDN = originalValue
		  Var dHDχax As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to χay
		  ε = 1.0e-5
		  originalValue = χayDN
		  χayDN = χayDN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χayDN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χayDN = originalValue
		  Var dHDχay As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to χaz
		  ε = 1.0e-5
		  originalValue = χazDN
		  χazDN = χazDN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χazDN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χazDN = originalValue
		  Var dHDχaz As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to χsx
		  ε = 1.0e-5
		  originalValue = χsxDN
		  χsxDN = χsxDN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χsxDN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χsxDN = originalValue
		  Var dHDχsx As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to χsy
		  ε = 1.0e-5
		  originalValue = χsyDN
		  χsyDN = χsyDN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χsyDN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χsyDN = originalValue
		  Var dHDχsy As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to χsz
		  ε = 1.0e-5
		  originalValue = χszDN
		  χszDN = χszDN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χszDN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χszDN = originalValue
		  Var dHDχsz As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // We need to define a bunch of variables to be used later
		  Var ιPlus As Double
		  Var αPlus As Double
		  Var ΨrPlus As Double
		  Var VPlus As Double
		  Var χaxPlus As Double
		  Var χayPlus As Double
		  Var χazPlus As Double
		  Var χsxPlus As Double
		  Var χsyPlus As Double
		  Var χszPlus As Double
		  // These variables will hold derivatives
		  Var dαDq As Double
		  Var dΨrDq As Double
		  Var dVDq As Double 
		  Var dιDq As Double
		  Var dχaxDq As Double
		  Var dχayDq As Double
		  Var dχazDq As Double
		  Var dχsxDq As Double
		  Var dχsyDq As Double
		  Var dχszDq As Double
		  
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.R)) Then
		    // Calculate the derivative with respect to q = lnR
		    dαDq = -(αDN - α0)*Parameters.OneOver1PlusZ*DZDlnR
		    dΨrDq = -(ΨrDN - Parameters.λ0)*Parameters.OneOver1PlusZ*DZDlnR
		    dVDq = -(VDN - Parameters.V0)*Parameters.OneOver1PlusZ*DZDlnR
		    dιDq = -(ιDN - ι0)*Parameters.OneOver1PlusZ*DZDlnR
		    dχaxDq = -(χaxDN - χax0)*Parameters.OneOver1PlusZ*DZDlnR
		    dχayDq = -(χayDN - χay0)*Parameters.OneOver1PlusZ*DZDlnR
		    dχazDq = -(χazDN - χaz0)*Parameters.OneOver1PlusZ*DZDlnR
		    dχsxDq = -(χsxDN - χsx0)*Parameters.OneOver1PlusZ*DZDlnR
		    dχsyDq = -(χsyDN - χsy0)*Parameters.OneOver1PlusZ*DZDlnR
		    dχszDq = -(χszDN - χsz0)*Parameters.OneOver1PlusZ*DZDlnR
		    
		    // Now, we put it all together (The first term is actually the derivative of h0 with respect to lnR).
		    DHDq(Integer(CaseInfoClass.Param.R))  = -hBase + dHDα*dαDq + dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq+ dHDχsz*dχszDq
		  Else
		    DHDq(Integer(CaseInfoClass.Param.R))  = 0.0
		  End If
		  
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.V0)) Then
		    // Calculate the derivative with respect to q = lnV0
		    GetDataAtDetectorStep(SourceEvolverV0Plus)
		    ιPlus = ιDN
		    αPlus = αDN
		    ΨrPlus = ΨrDN
		    VPlus = VDN
		    χaxPlus = χaxDN
		    χayPlus = χayDN
		    χazPlus = χazDN
		    χsxPlus = χsxDN
		    χsyPlus = χsyDN
		    χszPlus = χszDN
		    GetDataAtDetectorStep(SourceEvolverV0Minus)
		    dαDq = (αPlus - αDN)*IDεForV0
		    dΨrDq = (ΨrPlus - ΨrDN)*IDεForV0
		    dιDq = (ιPlus - ιDN)*IDεForV0
		    dVDq = (VPlus - VDN)*IDεForV0
		    dχaxDq = (χaxPlus - χaxDN)*IDεForV0
		    dχayDq = (χayPlus - χayDN)*IDεForV0
		    dχazDq = (χazPlus - χazDN)*IDεForV0
		    dχsxDq = (χsxPlus - χsxDN)*IDεForV0
		    dχsyDq = (χsyPlus - χsyDN)*IDεForV0
		    dχszDq = (χszPlus - χszDN)*IDεForV0
		    // Put it all together
		    DHDq(Integer(CaseInfoClass.Param.V0)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(CaseInfoClass.Param.V0)) = 0.0
		  End If
		  
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.delta)) Then
		    // Calculate the derivative with respect to q = δ
		    GetDataAtDetectorStep(SourceEvolverδPlus)
		    ιPlus = ιDN
		    αPlus = αDN
		    ΨrPlus = ΨrDN
		    VPlus = VDN
		    χaxPlus = χaxDN
		    χayPlus = χayDN
		    χazPlus = χazDN
		    χsxPlus = χsxDN
		    χsyPlus = χsyDN
		    χszPlus = χszDN
		    GetDataAtDetectorStep(SourceEvolverδMinus)
		    dαDq = (αPlus - αDN)*IDεForδ
		    dΨrDq = (ΨrPlus - ΨrDN)*IDεForδ
		    dVDq = (VPlus - VDN)*IDεForδ
		    dιDq = (ιPlus - ιDN)*IDεForδ
		    dχaxDq = (χaxPlus - χaxDN)*IDεForδ
		    dχayDq = (χayPlus - χayDN)*IDεForδ
		    dχazDq = (χazPlus - χazDN)*IDεForδ
		    dχsxDq = (χsxPlus - χsxDN)*IDεForδ
		    dχsyDq = (χsyPlus - χsyDN)*IDεForδ
		    dχszDq = (χszPlus - χszDN)*IDεForδ
		    // Put it all together
		    DHDq(Integer(CaseInfoClass.Param.delta)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq + dHDδ
		  Else
		    DHDq(Integer(CaseInfoClass.Param.delta)) = 0.0
		  End If
		  
		  '// Code to display values
		  'Var ιPlusStr as String = Format(ιPlus, "0.00000000000000e+00")
		  'Var αPlusStr as String = Format(αPlus, "0.00000000000000e+00")
		  'Var ΨrPlusStr as String = Format(ΨrPlus, "0.00000000000000e+00")
		  'Var VPlusStr as String = Format(VPlus, "0.00000000000000e+00")
		  'Var ιDNStr as String = Format(ιDN, "0.00000000000000e+00")
		  'Var αDNStr as String = Format(αDN, "0.00000000000000e+00")
		  'Var ΨrDNStr as String = Format(ΨrDN, "0.00000000000000e+00")
		  'Var VDNStr as String = Format(VDN, "0.00000000000000e+00")
		  'Var dHDιStr as String = Format(dHDι, "0.00000000000000e+00")
		  'Var dHDαStr as String = Format(dHDα, "0.00000000000000e+00")
		  'Var dHDΨrStr as String = Format(dHDΨr, "0.00000000000000e+00")
		  'Var dHDVStr as String = Format(dHDV, "0.00000000000000e+00")
		  'Var dHDM1Str as String = Format(DHDq(Integer(Item.M1)), "0.00000000000000e+00")
		  
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi10x)) Then
		    // Calculate the derivative with respect to q = χ10x
		    GetDataAtDetectorStep(SourceEvolverχ10xPlus)
		    ιPlus = ιDN
		    αPlus = αDN
		    ΨrPlus = ΨrDN
		    VPlus = VDN
		    χaxPlus = χaxDN
		    χayPlus = χayDN
		    χazPlus = χazDN
		    χsxPlus = χsxDN
		    χsyPlus = χsyDN
		    χszPlus = χszDN
		    GetDataAtDetectorStep(SourceEvolverχ10xMinus)
		    dαDq = (αPlus - αDN)*IDεForχ10x
		    dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ10x
		    dVDq = (VPlus - VDN)*IDεForχ10x
		    dιDq = (ιPlus - ιDN)*IDεForχ10x
		    dχaxDq = (χaxPlus - χaxDN)*IDεForχ10x
		    dχayDq = (χayPlus - χayDN)*IDεForχ10x
		    dχazDq = (χazPlus - χazDN)*IDεForχ10x
		    dχsxDq = (χsxPlus - χsxDN)*IDεForχ10x
		    dχsyDq = (χsyPlus - χsyDN)*IDεForχ10x
		    dχszDq = (χszPlus - χszDN)*IDεForχ10x
		    // Put it all together
		    DHDq(Integer(CaseInfoClass.Param.chi10x)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(CaseInfoClass.Param.chi10x)) = 0.0
		  End If
		  
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi10y)) Then
		    // Calculate the derivative with respect to q = χ10y
		    GetDataAtDetectorStep(SourceEvolverχ10yPlus)
		    ιPlus = ιDN
		    αPlus = αDN
		    ΨrPlus = ΨrDN
		    VPlus = VDN
		    χaxPlus = χaxDN
		    χayPlus = χayDN
		    χazPlus = χazDN
		    χsxPlus = χsxDN
		    χsyPlus = χsyDN
		    χszPlus = χszDN
		    GetDataAtDetectorStep(SourceEvolverχ10yMinus)
		    dαDq = (αPlus - αDN)*IDεForχ10y
		    dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ10y
		    dVDq = (VPlus - VDN)*IDεForχ10y
		    dιDq = (ιPlus - ιDN)*IDεForχ10y
		    dχaxDq = (χaxPlus - χaxDN)*IDεForχ10y
		    dχayDq = (χayPlus - χayDN)*IDεForχ10y
		    dχazDq = (χazPlus - χazDN)*IDεForχ10y
		    dχsxDq = (χsxPlus - χsxDN)*IDεForχ10y
		    dχsyDq = (χsyPlus - χsyDN)*IDεForχ10y
		    dχszDq = (χszPlus - χszDN)*IDεForχ10y
		    // Put it all together
		    DHDq(Integer(CaseInfoClass.Param.chi10y)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(CaseInfoClass.Param.chi10y)) = 0.0
		  End If
		  
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi10z)) Then
		    // Calculate the derivative with respect to q = χ10z
		    GetDataAtDetectorStep(SourceEvolverχ10zPlus)
		    ιPlus = ιDN
		    αPlus = αDN
		    ΨrPlus = ΨrDN
		    VPlus = VDN
		    χaxPlus = χaxDN
		    χayPlus = χayDN
		    χazPlus = χazDN
		    χsxPlus = χsxDN
		    χsyPlus = χsyDN
		    χszPlus = χszDN
		    GetDataAtDetectorStep(SourceEvolverχ10zMinus)
		    dαDq = (αPlus - αDN)*IDεForχ10z
		    dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ10z
		    dVDq = (VPlus - VDN)*IDεForχ10z
		    dιDq = (ιPlus - ιDN)*IDεForχ10z
		    dχaxDq = (χaxPlus - χaxDN)*IDεForχ10z
		    dχayDq = (χayPlus - χayDN)*IDεForχ10z
		    dχazDq = (χazPlus - χazDN)*IDεForχ10z
		    dχsxDq = (χsxPlus - χsxDN)*IDεForχ10z
		    dχsyDq = (χsyPlus - χsyDN)*IDεForχ10z
		    dχszDq = (χszPlus - χszDN)*IDεForχ10z
		    // Put it all together
		    DHDq(Integer(CaseInfoClass.Param.chi10z)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(CaseInfoClass.Param.chi10z)) = 0.0
		  End If
		  
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi20x)) Then
		    // Calculate the derivative with respect to q = χ20x
		    GetDataAtDetectorStep(SourceEvolverχ20xPlus)
		    ιPlus = ιDN
		    αPlus = αDN
		    ΨrPlus = ΨrDN
		    VPlus = VDN
		    χaxPlus = χaxDN
		    χayPlus = χayDN
		    χazPlus = χazDN
		    χsxPlus = χsxDN
		    χsyPlus = χsyDN
		    χszPlus = χszDN
		    GetDataAtDetectorStep(SourceEvolverχ20xMinus)
		    dαDq = (αPlus - αDN)*IDεForχ20x
		    dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ20x
		    dVDq = (VPlus - VDN)*IDεForχ20x
		    dιDq = (ιPlus - ιDN)*IDεForχ20x
		    dχaxDq = (χaxPlus - χaxDN)*IDεForχ20x
		    dχayDq = (χayPlus - χayDN)*IDεForχ20x
		    dχazDq = (χazPlus - χazDN)*IDεForχ20x
		    dχsxDq = (χsxPlus - χsxDN)*IDεForχ20x
		    dχsyDq = (χsyPlus - χsyDN)*IDεForχ20x
		    dχszDq = (χszPlus - χszDN)*IDεForχ20x
		    // Put it all together
		    DHDq(Integer(CaseInfoClass.Param.chi20x)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(CaseInfoClass.Param.chi20x)) = 0.0
		  End If
		  
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi20y)) Then
		    // Calculate the derivative with respect to q = χ20y
		    GetDataAtDetectorStep(SourceEvolverχ20yPlus)
		    ιPlus = ιDN
		    αPlus = αDN
		    ΨrPlus = ΨrDN
		    VPlus = VDN
		    χaxPlus = χaxDN
		    χayPlus = χayDN
		    χazPlus = χazDN
		    χsxPlus = χsxDN
		    χsyPlus = χsyDN
		    χszPlus = χszDN
		    GetDataAtDetectorStep(SourceEvolverχ20yMinus)
		    dαDq = (αPlus - αDN)*IDεForχ20y
		    dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ20y
		    dVDq = (VPlus - VDN)*IDεForχ20y
		    dιDq = (ιPlus - ιDN)*IDεForχ20y
		    dχaxDq = (χaxPlus - χaxDN)*IDεForχ20y
		    dχayDq = (χayPlus - χayDN)*IDεForχ20y
		    dχazDq = (χazPlus - χazDN)*IDεForχ20y
		    dχsxDq = (χsxPlus - χsxDN)*IDεForχ20y
		    dχsyDq = (χsyPlus - χsyDN)*IDεForχ20y
		    dχszDq = (χszPlus - χszDN)*IDεForχ20y
		    // Put it all together
		    DHDq(Integer(CaseInfoClass.Param.chi20y)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(CaseInfoClass.Param.chi20y)) = 0.0
		  End If
		  
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.chi20z)) Then
		    // Calculate the derivative with respect to q = χ20z
		    GetDataAtDetectorStep(SourceEvolverχ20zPlus)
		    ιPlus = ιDN
		    αPlus = αDN
		    ΨrPlus = ΨrDN
		    VPlus = VDN
		    χaxPlus = χaxDN
		    χayPlus = χayDN
		    χazPlus = χazDN
		    χsxPlus = χsxDN
		    χsyPlus = χsyDN
		    χszPlus = χszDN
		    GetDataAtDetectorStep(SourceEvolverχ20zMinus)
		    dαDq = (αPlus - αDN)*IDεForχ20z
		    dΨrDq = (ΨrPlus - ΨrDN)*IDεForχ20z
		    dVDq = (VPlus - VDN)*IDεForχ20z
		    dιDq = (ιPlus - ιDN)*IDεForχ20z
		    dχaxDq = (χaxPlus - χaxDN)*IDεForχ20z
		    dχayDq = (χayPlus - χayDN)*IDεForχ20z
		    dχazDq = (χazPlus - χazDN)*IDεForχ20z
		    dχsxDq = (χsxPlus - χsxDN)*IDεForχ20z
		    dχsyDq = (χsyPlus - χsyDN)*IDεForχ20z
		    dχszDq = (χszPlus - χszDN)*IDεForχ20z
		    // Put it all together
		    DHDq(Integer(CaseInfoClass.Param.chi20z)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(CaseInfoClass.Param.chi20z)) = 0.0
		  End If
		  
		  // If we are doing single cases, plot record.
		  // Make sure we have one item for every item in the CaseInfoClass.PlotItem enum list.
		  Var theValues() As Double
		  theValues.ResizeTo(Integer(CaseInfoClass.PlotItem.NItems) - 1)
		  theValues(Integer(CaseInfoClass.PlotItem.H)) = hBase
		  theValues(Integer(CaseInfoClass.PlotItem.HP)) = hpBase
		  theValues(Integer(CaseInfoClass.PlotItem.HX)) = hxBase
		  theValues(Integer(CaseInfoClass.PlotItem.V)) = VDN
		  theValues(Integer(CaseInfoClass.PlotItem.PsiR)) = ΨrDN
		  theValues(Integer(CaseInfoClass.PlotItem.Iota)) = ιDN
		  theValues(Integer(CaseInfoClass.PlotItem.Alpha)) = αDN
		  theValues(Integer(CaseInfoClass.PlotItem.ChiSx)) = χsxDN
		  theValues(Integer(CaseInfoClass.PlotItem.ChiSy)) = χsyDN
		  theValues(Integer(CaseInfoClass.PlotItem.ChiSz)) = χszDN
		  theValues(Integer(CaseInfoClass.PlotItem.ChiAx)) = χsxDN
		  theValues(Integer(CaseInfoClass.PlotItem.ChiAy)) = χsyDN
		  theValues(Integer(CaseInfoClass.PlotItem.ChiAz)) = χszDN
		  theValues(Integer(CaseInfoClass.PlotItem.dHdM)) = DHDq(Integer(CaseInfoClass.Param.M))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdDelta)) = DHDq(Integer(CaseInfoClass.Param.delta))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdV0)) = DHDq(Integer(CaseInfoClass.Param.V0))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdR)) = DHDq(Integer(CaseInfoClass.Param.R))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdBeta)) = DHDq(Integer(CaseInfoClass.Param.beta))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdPsi)) = DHDq(Integer(CaseInfoClass.Param.psi))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdLambda0)) = DHDq(Integer(CaseInfoClass.Param.lambda0))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdTheta)) = DHDq(Integer(CaseInfoClass.Param.theta))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdPhi)) = DHDq(Integer(CaseInfoClass.Param.phi))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdChi10x)) = DHDq(Integer(CaseInfoClass.Param.chi20x))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdChi10y)) = DHDq(Integer(CaseInfoClass.Param.chi20y))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdChi20z)) = DHDq(Integer(CaseInfoClass.Param.chi20z))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdChi20x)) = DHDq(Integer(CaseInfoClass.Param.chi20x))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdChi20y)) = DHDq(Integer(CaseInfoClass.Param.chi20y))
		  theValues(Integer(CaseInfoClass.PlotItem.dHdChi20z)) = DHDq(Integer(CaseInfoClass.Param.chi20z))
		  Parameters.PlotRecords.Add(New PlotRecord(theValues))
		  
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
		Sub CalculateDAdι()
		  // load the constant parameters
		  Var ιDN As Double  =self.ιDN 
		  Var β As Double  = self.β 
		  Var c2 As Double  = Trig.c2
		  Var s2 As Double = Trig.s2
		  Var c1 As Double = Trig.c1
		  Var s1   As Double = Trig.s1
		  Var c3   As Double = Trig.c3
		  Var s3  As Double  = Trig.s3
		  Var c4   As Double = Trig.c4
		  Var s4  As Double  = Trig.s4
		  Var c5  As Double  = Trig.c5
		  Var s5   As Double = Trig.s5
		  Var c6 As Double   = Trig.c6
		  Var s6   As Double = Trig.s6
		  Var c7   As Double = Trig.c7
		  Var s7 As Double   = Trig.s7
		  Var c8  As Double  = Trig.c8
		  Var s8  As Double  = Trig.s8
		  Var c9  As Double  = Trig.c9
		  Var s9   As Double = Trig.s9
		  Var c10  As Double  = Trig.c10
		  Var s10  As Double  = Trig.s10
		  Var c1p2  As Double  = Trig.c1p2
		  Var c1p3  As Double  = Trig.c1p3   
		  Var c1p4  As Double  =Trig. c1p4   
		  Var c1p5 As Double  = Trig.c1p5   
		  Var c1p6 As Double   = Trig.c1p6   
		  Var c1p7  As Double  = Trig.c1p7 
		  Var c1p8 As Double   = Trig.c1p8  
		  Var c1p9   As Double = Trig.c1p9  
		  Var c1p10  As Double  = Trig.c1p10
		  Var s1p2   As Double = Trig.s1p2   
		  Var s1p3  As Double  = Trig.s1p3   
		  Var s1p4As Double    = Trig.s1p4   
		  Var s1p5   As Double = Trig.s1p5  
		  Var s1p6 As Double   = Trig.s1p6   
		  Var s1p7 As Double   = Trig.s1p7   
		  Var s1p8  As Double  = Trig.s1p8   
		  Var s1p9 As Double   = Trig.s1p
		  Var s1p10 As Double  = Trig.s1p10
		  Var c2p2  As Double  = Trig.c2p2
		  Var c2p3   As Double = Trig.c2p3
		  Var c2p4 As Double  = Trig.c2p4
		  Var s2p2  As Double  = Trig.s2p2
		  Var s2p3  As Double  = Trig.s2p3
		  Var s2p4 As Double   = Trig.s2p4
		  Var s2p5 As Double   = Trig.s2p5
		  
		  // Define local β trig functions
		  Var cβ  As Double  = Trig.cβ
		  Var sβ  As Double  = Trig.sβ
		  Var c2β As Double   = Trig.c2β
		  Var s2β  As Double  = Trig.s2β
		  Var c3β As Double   = Trig.c3β
		  Var s3β  As Double  = Trig.s3β
		  Var c4β As Double   = Trig.c4β
		  Var s4β  As Double  = Trig.s4β
		  Var c5β As Double   = Trig.c5β
		  Var s5β As Double   = Trig.s5β
		  Var cβ2  As Double  = Trig.cβ2
		  Var cβ3 As Double   = Trig.cβ3
		  Var sβ2  As Double  = Trig.sβ2
		  Var sβ3  As Double  = Trig.sβ3
		  
		  //Load dAdι
		  dAdι(0) = c2β*c1p3*s1 + 3*c1p3*s1
		  dAdι(1) = 3*s2β* c1p2*s1p2 − s2β *c4
		  dAdι(2) = −s2β *s1p4 + 3*s2β *c1p2*s1p2
		  dAdι(3) = −c2β *c1*s1p3 − 3*c1*s1p3
		  dAdι(4) = −3*sβ2 *c2*s2
		  dAdι(5) = 27/32 *δ*c3β* c1p5*s1 + 135/32* δ*sβ* c1p5*s1
		  'dAdι(6) = 5/64* δ*c3β *c1*c2*s1 − 15/256* δ*c3β* c1*c4*s1 − 13/256 *δ*c3β *c1*s1 + 5/64 *δ*c3β* c1p2*s2 − 15/128* δ*c3β* c1p2*s4− 87/64* δ*sβ *c1*c2*s1 + 5/256 β*c1*c4*s1 + 175/256* δ*sβ *c1*s1 − 87/64* δ*sβ *c1p2*s2 + 5/128* δ*sβ *c1p2*s4
		  dAdι(7) = 5/64* δ*c3β* s1p2*s2 + 15/128 *δ*c3β *s1p2*s4 − 5/64 *δ*c3β *c1*c2*s1 − 15/256 *δ*c3β* c1*c4*s1 − 13/256 *δ*c3β *c1*s1− 87/64 *δ*sβ *s2
		  1s2 − 5/128* δ*sβ *s1p2*s4 + 87/64* δ*sβ *c1*c2*s1 + 5/256 *δ*sβ *c1*c4*s1 + 175/256 *δ*sβ *c1*s1
		  dAdι(8) = 1/16 *δ*c3β* c1p3*s1p3 − 1/32* δ*c3β *c1p5*s1 + 5/16* δ*sβ *c1p3*s1p3 − 5/32* δ*c1p5*sβ *s1
		  dAdι(9) = − 135/16* δ*c1p3*c3β *s1p3 + 135/32* δ*c1p5*c3β* s1 + 45/16 *δ*c1p3*sβ *s1p3 − 45/32* δ*c1p5*sβ *s1
		  dAdι(10) = 135/32* δ*c3β *c1*s1p5 − 135/16 *δ*c3β* c1p3*s1p3 − 45/32 *δ*c1*sβ *s1p5 + 45/16* δ*sβ* c1p3*s1p3
		  dAdι(11) = − 1/32* δ*c3β *c1*s1p5 + 1/16* δ*c3β *c1p3*s1p3 − 5/32* δ*sβ *c1*s1p5 + 5/16* δ*sβ* c1p3*s1p3
		  dAdι(12) = 27/16* δ*c2β *sβ *c1*s1p5 + 81/16 *δ*sβ *c1*s1p5
		  dAdι(13) = 135/16 *δ*cβ *sβ2 *c2*s2p2
		  dAdι(14) = 1/32* δ*c2β *cβ *s2p2 + 3/64* δ*c2β* cβ* s2*s4 − 1/32 *δ*c2β *cβ* c2p2 − 3/128* δ*c2β *cβ* c2*c4 − 1/128 *δ*c2β *cβ *c2− 85/256* δ*cβ *c2 − 11/32* δ*cβ *c4 − 3/256* δ*cβ *c6
		  dAdι(15) = − 27/32* δ*c2β *cβ* s2p2 − 27/64* δ*c2β *cβ* s2*s4 + 27/32* δ*c2β* cβ* c2p2 + 27/128* δ*c2β *cβ* c2*c4 + 81/128* δ*c2β *cβ *c2+ 45/256* δ*cβ* c2 + 9/32* δ*cβ* c4 + 27/256 *δ*cβ *c6
		  dAdι(16) = 1/256* δ*c2β *cβ* c2 + 1/32* δ*c2β *cβ* c4 + 9/256* δ*c2β *cβ *c6 − 85/256* δ*cβ *c2 + 11/32* δ*cβ *c4 − 3/256* δ*cβ *c6
		  dAdι(17) = 135/256* δ*c2β *cβ *c2 + 27/32* δ*c2β *cβ* c4 + 81/256* δ*c2β *c6 + 45/256* δ*cβ *c2 − 9/32* δ*cβ *c4 + 27/256 *δ*cβ *c6
		  dAdι(18) = 1/64* δ*cβ *sβ2 *c2 + 15/64 *δ*cβ *sβ2 *c6
		  dAdι(19) = −11*η*c2β* s1*c1p3*c2 − 1/2 *η*c2β *s1*c1p3*c4 + 26/3 *η*c2β *s1*c1p3 − 11/2 *η*c2β* s2*c1p4 − 1/2* η*c2β *s4*c1p4 + 11/3* c2β* s1*c1p3*c2+ 1/6 *c2β *s1*c1p3*c4 − 5*c2β *s1*c1p3 + 11/6 *c2β* s2*c1p4 + 1/6 *c2β* s4*c1p4 + 7/4* η*c4β *s1*c1p3*c2 − 7/8 *η*c4β *s1*c1p3*c4− 9/8* η*c4β *s1*c1p3 + 7/8 *η*c4β *s2*c1p4 − 7/8* η*c4β *s4*c1p4 − 7/12 *c4β *s1*c1p3*c2 + 7/24* c4β *s1*c1p3*c4 + 3/8 *c4β *s1*c1p3− 7/24* c4β *s2*c1p4 + 7/24 *c4β *s4*c1p4 + 5/4* η*s1*c1p3*c2 − 5/8 *η*s1*c1p3*c4 + 25/8* η*s1*c1p3 + 5/8* η*s2*c1p4 − 5/8 *η*s4*c1p4− 5/12* s1*c1p3*c2 + 5/24* s1*c1p3*c4 − 59/8* s1*c1p3 − 5/24 *s2*c1p4 + 5/24 *s4*c4
		  dAdι(20) = −8*η*c2β *sβ2* s1*c1p7 + 8/3* c2β* sβ2 *s1*c1p7 − 24*η*sβ2 *s1*c1p7 + 8*sβ2 *s1*c1p7
		  dAdι(21) = 112*η*cβ3 *sβ *s1p2*c1p6 − 16*η*cβ3 *sβ* c1p8 − 112/3 *cβ3* sβ* s1p2*c1p6 + 16/3 *cβ3 *sβ *c1p8
		  dAdι(22) = 5/4* η*c2β *s2β *s1p2*c1p4 − 1/4* η*c2β *s2β *c1p6 − 5/12 *c2β* s2β *s1p2*c1p4 + 1/12* c2β *s2β *c1p6−5*η*cβ2 *s2β *s1p2*c1p4*c2 − 2*η*cβ2* s2β *s1*s2*c1p5 + η*cβ2 *s2β* c1p6*c2 + 5/3* cβ2* s2β *s1p2*c1p4*c2+ 2/3 *cβ2 *s2β* s1*s2*c1p5 − 1/3 *cβ2 *s2β* c1p6*c2 − 25/4* η*s2β* s1p2*c1p4 + 5/4* η*s2β* c1p6 + 25/12* s2β* s1p2*c1p4 − 5/12* s2β* c1p6
		  dAdι(23) = −24*η*c2β* s1p3*c1p5 + 8*η*c2β *s1*c1p7 + 8*c2β *s1p3*c1p5 − 8/3 *c2β* s1*c1p7 − 42*η*c4β *s1p3*c1p5 + 14*η*c4β *s1*c1p7+14*c4β *s1p3*c1p5 − 14/3* c4β* s1*c1p7 − 30*η*s1p3*c1p5 + 10*η*s1*c1p7 + 10*s1p3*c1p5 − 10/3 *s1*c1p7
		  dAdι(24) = − 3/2 *η*c2β* sβ2* s1p3*c1p5 + 1/2 *η*c2β* sβ2 *s1*c1p7 + 1/2 *c2β *sβ2 *s1p3*c1p5 − 1/6 *c2β *sβ2* s1*c1p7− 9/2* η*sβ2 *s1p3*c1p5 + 3/2* η*sβ2* s1*c1p7 + 3/2 *sβ2* s1p3*c1p5 − 1/2* sβ2* s1*c1p7
		  dAdι(25) = −140*η*c2β *s2β* s1p4*c1p4 + 84*η*c2β *s2β* s1p2*c1p6 + 140/3 *c2β *s2β *s1p4*c1p4 − 28*c2β* s2β *s1p2*c1p6 + 20*η*s2β s1p4*c1p4−12*η*s2β* s1p2*c1p6 − 20/3 *s2β* s1p4*c1p4 + 4*s2β* s1p2*c1p6
		  dAdι(26) = − 21/16 *η*s4β *c3*s1p2*c1 + 7/16 *s4β* c3*s1p2*c1 − 31/8 *η*s2β *s1p4*c2 − 1/8 *η*s2β *s1p4*c4 − 8/3 *η*s2β *s1p4− 31/4 *η*s2β *s1p3*s2*c1 − 1/2 *η*s2β *s1p3*s4*c1 + 93/8 *η*s2β *s1p2*c1p2*c2 + 3/8 *η*s2β* s1p2*c1p2*c4 + 8*η*s2β *s1p2*c1p2+ 19/32* η*s4β *s1p4 + 35/32 *η*s4β* s1p3*s5 − 57/32* η*s4β *s1p2*c1p2 − 21/32* η*s4β *s1p2*c1*c5 + 31/24 *s2β* s1p4*c2 + 1/24 *s2β *s1p4*c4+3*s2β* s1p4 + 31/12 *s2β *s1p3*s2*c1 + 1/6 *s2β* s1p3*s4*c1 − 31/8* s2β* s1p2*c1p2*c2 − 1/8 *s2β *s1p2*c1p2*c4 − 9*s2β *s1p2*c1p2− 19/96 *s4β* s1p4 − 35/96 *s4β *s1p3*s5 + 19/32 *s4β* s1p2*c1p2 + 7/32 *s4β* s1p2*c1*c5
		  dAdι(27) =11/2 *η*c2β *s1p4*s2 − 1/2 *η*c2β* s1p4*s4 − 11*η*c2β *s1p3*c1*c2 + 1/2 *η*c2β *s1p3*c1*c4 − 26/3 *η*c2β *s1p3*c1− 11/6 *c2β* s1p4*s2 + 1/6 *c2β *s1p4*s4 + 11/3 *c2β *s1p3*c1*c2 − 1/6 *c2β *s1p3*c1*c4 + 5*c2β* s1p3*c1 − 7/8* η*c4β *s1p4*s2− 7/8 *η*c4β *s1p4*s4 + 7/4 *η*c4β *s1p3*c1*c2 + 7/8 *η*c4β *s1p3*c1*c4 + 9/8* η*c4β* s1p3*c1 + 7/24 *c4β *s1p4*s2+ 7/24 *c4β *s1p4*s4 − 7/12 *c4β *s1p3*c1*c2 − 7/24 *c4β* s1p3*c1*c4 − 3/8 *c4β *s1p3*c1 − 5/8* η*s1p4*s2 − 5/8 *η*s1p4*s4 + 5/4* η*s1p3*c1*c2+ 5/8* η*s1p3*c1*c4 − 25/8* η*s1p3*c1 + 5/24 *s1p4*s2 + 5/24 *s1p4*s4 − 5/12* s1p3*c1*c2 − 5/24* s1p3*c1*c4 + 59/8* s1p3*c1 
		  dAdι(28) = 84*η*c2β *s2β *s1p6*c1p2 − 140*η*c2β *s2β *s1p4*c1p4 − 28*c2β *s2β *s1p6*c1p2 + 140/3 *c2β* s2β* s1p4*c1p4−12*η*s2β* s1p6*c1p2 + 20*η*s2β *s1p4*c1p4 + 4*s2β *s1p6*c1p2 − 20/3 *s2β *s1p4*c4
		  dAdι(29) = − 1/4 *η*c2β *s2β* s1p6 + 5/4* η*c2β *s2β *s1p4*c1p2 + 1/12 *c2β *s2β* s1p6 − 5/12 *c2β *s2β* s1p4*c1p2−η*cβ2 *s2β* s1p6*c2 − 2*η*cβ2 *s2β* s1p5*s2*c1 + 5*η*cβ2* s2β* s1p4*c1p2*c2 + 1/3 *cβ2 *s2β* s1p6*c2 + 2/3 *cβ2 *s2β *s1p5*s2*c1− 5/3 *cβ2 *s2β *s1p4*c1p2*c2 + 5/4 *η*s2β *s1p6 − 25/4* η*s2β *s1p4*c1p2 − 1/12 *s2β *s1p6 + 25/12* s2β* s1p4*c1p2
		  dAdι(30) = −8*η*c2β *s1p7*c1 + 24*η*c2β* s1p5*c1p3 + 8/3 *c2β* s1p7*c1 − 8*c2β *s1p5*c1p3 − 14*η*c4β *s1p7*c1 + 42*η*c4β* s1p5*c1p3+ 14/3 *c4β* s1p7*c1 − 14*c4β *s1p5*c1p3 − 10*η*s1p7*c1 + 30*η*s1p5*c1p3 + 10/3 *s1p7*c1 − 10*s1p5*c3
		  dAdι(31) = − 1/2* η*c2β *sβ2 *s1p7*c1 + 3/2 *η*c2β* sβ2 *s1p5*c1p3 + 1/6 *c2β *sβ2 *s1p7*c1 − 1/2 *c2β *sβ2 *s1p5*c1p3− 3/2* η*sβ2 *s1p7*c1 + 9/2* η*sβ2 *s1p5*c1p3 + 1/2 *sβ2* s1p7*c1 − 3/2 *sβ2 *s1p5*c3
		  dAdι(32) = −16*η*cβ3 *sβ *s1p8 + 112*η*cβ3 *sβ *s1p6*c1p2 + 16/3 *cβ3 *sβ *s1p8 − 112/3 *cβ3 *sβ* s1p6*c1p2
		  dAdι(33) = 8*η*c2β *sβ2 *s1p7*c1 − 8/3 *c2β *sβ2 *s1p7*c1 + 24*η*sβ2* s1p7*c1 − 8*sβ2 *s1p7*c1
		  dAdι(34) = − 35/16 *η*c2β *sβ2* s2p2*s4 + 35/16* η*c2β *sβ2 *s2*c2*c4 + 25/16* η*c2β *sβ2 *s2*c2 + 35/48 *c2β* sβ2 *s2p2*s4− 35/48 *c2β *sβ2 *s2*c2*c4 − 25/48* c2β* sβ2 *s2*c2 − 25/16* η*sβ2 *s2p2*s4 + 25/16* η*sβ2 *s2*c2*c4 − 45/16* η*sβ2 *s2*c2+ 25/48 *sβ2* s2p2*s4 − 25/48 *sβ2 *s2*c2*c4 + 349/48* sβ2* s2*c2
		  dAdι(35) =35*η*c2β *sβ2* s2p3*c2 + 35/3 *c2β* sβ2* s2p3*c2 + 25*η*sβ2* s2p3*c2 − 25/3* sβ2* s2p3*c2 
		  dAdι(36) = − 93/8 *η*s2β* s1p2*c1p2*c2 + 3/8* η*s2β* s1p2*c1p2*c4 + 8*η*s2β *s1p2*c1p2 − 31/4 *η*s2β *s1*s2*c1p3 + 1/2 *η*s2β* s1*s4*c1p3+ 31/8 *η*s2β* c1p4*c2 − 1/8* η*s2β* c1p4*c4 − 8/3* η*s2β *c1p4 − 57/32 *η*s4β *s1p2*c1p2 − 21/32 *η*s4β *s1*s5*c1p2+ 21/16 *η*s4β *s1*c1p2*c3 + 21/8 *η*s4β *s3*c1p3*c3 + 19/32* η*s4β *c1p4 + 35/32 *η*s4β *c1p3*c5 + 31/8 *s2β *s1p2*c1p2*c2− 1/8 *s2β* s1p2*c1p2*c4 − 9*s2β *s1p2*c1p2 + 31/12 *s2β *s1*s2*c1p3 − 1/6 *s2β *s1*s4*c1p3 − 31/24 *s2β *c1p4*c2 + 1/24 *s2β *c1p4*c4+3*s2β* c1p4 + 19/32 *s4β* s1p2*c1p2 + 7/32* s4β *s1*s5*c1p2 − 7/16 *s4β *s1*c1p2*c3 − 7/16 *s4β *s3*c1p3 − 19/96 *s4β *c1p4 − 35/96 *s4β* c1p3*c5
		  dAdι(37) = −cβ *s1*c1*χax + sβ *s1*c1*χaz
		  dAdι(38) = 1/2* cβ *s2*χax − sβ *s1*c1*χaz
		  dAdι(39) = −cβ *s1*c1*χay
		  dAdι(40) = −sβ *c2*χay
		  dAdι(41) = cβ *s1*c1*χay
		  dAdι(42) = −cβ* δ*s1*c1*χsx + δ*sβ *s1*c1*χsz
		  dAdι(43) = 1/2* cβ* δ*s2*χsx − δ*sβ *s1*c1*χsz
		  dAdι(44) = −cβ* δ*s1*c1*χsy
		  dAdι(45) = −δ*sβ* c2*χsy
		  dAdι(46) = δ*cβ *s1*c1*χsy
		  dAdι(47) = 2*π*c2β *s1*c1p3 +6*π*s1*c1p3
		  dAdι(48) = 6*π*s2β* s1p2*c1p2 − 2*π*s2β* c1p4
		  dAdι(49) = −2*π*s2β *s1p4 + 6*π*s2β* s1p2*c1p2
		  dAdι(50) = −2*π*c2β *s1p3*c1 − 6*π*s1p3*c1
		  dAdι(51) = −6*π*sβ2* s2*c2
		  dAdι(52) = − 1953125/294912* δ*η*c2β^2 *sβ3 *s1*c1p9 − 1953125/49152 *δ*η*c2β* sβ3 *s1*c1p9 − 1953125/32768 *δ*η*sβ3 *s1*c9
		  dAdι(53) = − 7/32768* δ*η*c3β *c8*s1*c1 + 7/65536 *δ*c3β *c8*s1*c1 − 19/4096* δ*η*c3β *s1*c1*c2 + 47/8192* δ*η*c3β *s1*c1*c4+ 91/4096* δ*η*c3β* s1*c1*c6 + 331/32768* δ*η*c3β *s1*c1 − 19/4096 *δ*η*c3β *s2*c1p2 + 47/4096* δ*η*c3β *s4*c1p2 + 273/4096* δ*η*c3β *s6*c1p2− 1901/8192 *δ*c3β *s1*c1*c2 + 2833/16384 *δ*c3β *s1*c1*c4 − 91/8192* δ*c3β *s1*c1*c6 + 9653/65536* δ*c3β *s1*c1 − 1901/8192* δ*c3β* s2*c1p2+ 2833/8192 *δ*c3β* s4*c1p2 − 273/8192* δ*c3β *s6*c1p2 + 35/32768 *δ*η*c8*s5β* s1*c1 − 7/49152 *δ*η*c8*sβ *s1*c1 − 35/65536 *δ*c8*s5β* s1*c1+ 7/98304 *δ*c8*sβ *s1*c1 − 35/12288* δ*η*s5β *s1*c1*c2 + 21/8192* δ*η*s5β *s1*c1*c4 − 7/4096* δ*η*s5β *s1*c1*c6+ 155/98304* δ*η*s5β* s1*c1 − 35/12288* δ*η*s5β *s2*c1p2 + 21/4096* δ*η*s5β *s4*c1p2 − 21/4096* δ*η*s5β *s6*c1p2 − 1873/2048* δ*η*sβ *s1*c1*c2+ 59/6144* δ*η*sβ *s1*c1*c6 + 7449/16384 *δ*η*sβ *s1*c1 − 1873/2048 *δ*η*sβ *s2*c1p2 + 59/2048* δ*η*sβ *s6*c1p2 + 35/24576* δ*s5β *s1*c1*c2− 21/16384* δ*s5β *s1*c1*c4 + 7/8192 *δ*s5β *s1*c1*c6 − 155/196608* δ*s5β *s1*c1 + 35/24576* δ*s5β *s2*c1p2 − 21/8192* δ*s5β *s4*c1p2+ 21/8192* δ*s5β *s6*c1p2 + 10675/12288* δ*sβ *s1*c1*c2 − 1777/24576 *δ*sβ *s1*c1*c4 − 59/12288* δ*sβ *s1*c1*c6 − 43723/98304* δ*sβ* s1*c1+ 10675/12288* δ*sβ *s2*c1p2 − 1777/12288* δ*sβ *s4*c1p2 − 59/4096 *δ*sβ *s6*c2
		  dAdι(54) = − 19197/2048* δ*η*c3β *s1*c1p5*c2 − 9477/8192* δ*η*c3β *s1*c1p5*c4 + 73521/8192 *δ*η*c3β *s1*c1p5 − 6399/2048* δ*η*c3β* s2*c1p6− 3159/4096 *δ*η*c3β* s4*c1p6 + 19197/4096* δ*c3β *s1*c1p5*c2 + 9477/16384 *δ*c3β* s1*c1p5*c4 − 114993/16384 *δ*c3β *s1*c1p5+ 6399/4096 *δ*c3β *s2*c1p6 + 3159/8192 *δ*c3β *s4*c1p6 + 6561/2048* δ*η*s5β* s1*c1p5*c2 − 10935/8192* δ*η*s5β *s1*c1p5*c4− 17253/8192 *δ*η*s5β *s1*c1p5 + 2187/2048* δ*η*s5β *s2*c1p6 − 3645/4096 *δ*η*s5β *s4*c1p6 + 7209/1024* δ*η*sβ *s1*c1p5*c2− 5103/4096* δ*η*sβ* s1*c1p5*c4 + 14067/4096* δ*η*sβ *s1*c1p5 + 2403/1024* δ*η*sβ *s2*c1p6 − 1701/2048* δ*η*sβ *s4*c1p6− 6561/4096* δ*s5β *s1*c1p5*c2 + 10935/16384* δ*s5β *s1*c1p5*c4 + 17253/16384 *δ*s5β* s1*c1p5 − 2187/4096 *δ*s5β* s2*c1p6+ 3645/8192 *δ*s5β *s4*c1p6 − 7209/2048 *δ*sβ *s1*c1p5*c2 + 5103/8192 *δ*sβ *s1*c1p5*c4 − 117747/8192* δ*sβ* s1*c1p5− 2403/2048 *δ*sβ* s2*c1p6 + 1701/4096* δ*sβ *s4*c1p6
		  dAdι(55) = 9375/256 *δ*η*c3β *sβ2 *s1p2*c1p8 − 3125/768* δ*η*c3β *sβ2* c1p10 − 9375/512 *δ*c3β* sβ2 *s1p2*c1p8+ 3125/1536* δ*c3β* sβ2 *c1p10 + 35625/256* δ*η*cβ* sβ2 *s1p2*c1p8 − 11875/768* δ*η*cβ *sβ2* c1p10 − 35625/512 *δ*cβ *sβ2 *s1p2*c1p8+ 11875/1536* δ*cβ* sβ2 *c1p10
		  dAdι(56) = − 2835/256* δ*η*c2β *cβ *sβ2 *s1p2*c1p6*c2 + 1701/256* δ*η*c2β *cβ *sβ2 *s1p2*c1p6− 405/128* δ*η*c2β *cβ* sβ2* s1*s2*c1p7 + 405/256* δ*η*c2β* cβ *sβ2 *c1p8*c2 − 243/256 *δ*η*c2β *cβ *sβ2 *c1p8+ 2835/512 *δ*c2β* cβ* sβ2 *s1p2*c1p6*c2 − 1701/512* δ*c2β *cβ *sβ2* s1p2*c1p6 + 405/256 *δ*c2β* cβ *sβ2 *s1*s2*c1p7− 405/512 *δ*c2β *cβ *sβ2 *c1p8*c2 + 243/512 *δ*c2β* cβ* sβ2 *c1p8 − 3969/256* δ*η*cβ *sβ2 *s1p2*c1p6*c2 − 2457/256* δ*η*cβ *sβ2 *s1p2*c1p6− 567/128* δ*η*cβ *sβ2* s1*s2*c1p7 + 567/256* δ*η*cβ *sβ *c1p8*c2 + 351/256* δ*η*cβ *sβ *c1p8 + 3969/512* δ*cβ *sβ2 *s1p2*c1p6*c2+ 2457/512* δ*cβ* sβ2* s1p2*c1p6 + 567/256 *δ*cβ *sβ2 *s1*s2*c1p7 − 567/512 *δ*cβ *sβ2 *c1p8*c2 − 351/512* δ*cβ *sβ2* c1p8
		  dAdι(57) = − 81/32* δ*η*c2β *sβ3 *s1p3*c1p7 + 81/128* δ*η*c2β *sβ3 *s1*c1p9 + 81/64 *δ*c2β *sβ3 *s1p3*c1p7− 81/256* δ*c2β *sβ3 *s1*c1p9 − 243/32* δ*η*sβ3* s1p3*c1p7 + 243/128 *δ*η*sβ3 *s1*c1p9 + 243/64* δ*sβ3 *s1p3*c1p7 − 243/256* δ*sβ3* s1*c1p9
		  dAdι(58) = 7/65536* δ*c3β *c8*s1*c1 − 7/32768* η*c3β* c8*s1*c1 − 1901/8192 *δ*c3β *s1p2*s2 − 2833/8192 *δ*c3β *s1p2*s4− 273/8192 *δ*c3β* s1p2*s6 + 1901/8192 *δ*c3β *s1*c1*c2 + 2833/16384* δ*c3β *s1*c1*c4 + 91/8192* δ*c3β *s1*c1*c6 + 9653/65536 *δ*c3β *s1*c1− 19/4096 *η*c3β* s1p2*s2 − 47/4096 *η*c3β *s1p2*s4 + 273/4096 *η*c3β* s1p2*s6 + 19/4096 *η*c3β *s1*c1*c2 + 47/8192* η*c3β *s1*c1*c4− 91/4096* η*c3β *s1*c1*c6 + 331/32768* η*c3β *s1*c1 − 35/65536* δ*c8*s5β *s1*c1 + 7/98304* δ*c8*sβ *s1*c1+ 35/32768* η*c8*s5β* s1*c1 − 7/49152 *η*c8*sβ *s1*c1 + 35/24576 *δ*s5β *s1p2*s2 + 21/8192* δ*s5β *s1p2*s4 + 21/8192* δ*s5β *s1p2*s6− 35/24576* δ*s5β *s1*c1*c2 − 21/16384* δ*s5β *s1*c1*c4 − 7/8192*δ*s5β *s1*c1*c6 − 155/196608 *δ*s5β *s1*c1 + 10675/12288* δ*sβ *s1p2*s2+ 1103/12288 *δ*sβ *s1p2*s4 − 59/4096* δ*sβ *s1p2*s6 − 10675/12288* δ*sβ* s1*c1*c2 − 1103/24576 *δ*sβ *s1*c1*c4 + 59/12288* δ*sβ* s1*c1*c6− 43723/98304* δ*sβ* s1*c1 − 35/12288 *η*s5β *s1p2*s2 − 21/4096* η*s5β *s1p2*s4 − 21/4096* η*s5β *s1p2*s6 + 35/12288* η*s5β *s1*c1*c2+ 21/8192* η*s5β* s1*c1*c4 + 7/4096 *η*s5β *s1*c1*c6 + 155/98304 *η*s5β *s1*c1 − 1873/2048 *η*sβ *s1p2*s2 + 337/6144 *η*sβ *s1p2*s4+ 59/2048* η*sβ *s1p2*s6 + 1873/2048* η*sβ *s1*c1*c2 − 337/12288* η*sβ *s1*c1*c4 − 59/6144* η*sβ *s1*c1*c6 + 7449/16384 *η*sβ *s1*c1
		  dAdι(59) = − 151/512* δ*η*c3β *s1p3*c1p3*c2 − 13/2048 *δ*η*c3β *s1p3*c1p3*c4 + 57/2048 *δ*η*c3β *s1p3*c1p3 − 151/1024 *δ*η*c3β *s1p2*s2*c1p4− 13/2048 *δ*η*c3β *s1p2*s4*c1p4 + 151/1024 *δ*η*c3β *s1*c1p5*c2 + 13/4096 *δ*η*c3β *s1*c1p5*c4 − 57/4096* δ*η*c3β *s1*c1p5+ 151/1024* δ*c3β *s1p3*c1p3*c2 + 13/4096* δ*c3β *s1p3*c1p3*c4 − 825/4096 *δ*c3β *s1p3*c1p3 + 151/2048* δ*c3β *s1p2*s2*c1p4+ 13/4096* δ*c3β *s1p2*s4*c1p4 − 151/2048* δ*c3β* s1*c1p5*c2 − 13/8192* δ*c3β* s1*c1p5*c4 + 825/8192 *δ*c3β* s1*c1p5+ 3/512 *δ*η*s5β *s1p3*c1p3*c2 − 15/2048 *δ*η*s5β *s1p3*c1p3*c4 − 13/2048 *δ*η*s5β *s1p3*c1p3 + 3/1024 *δ*η*s5β *s1p2*s2*c1p4− 15/2048 *δ*η*s5β *s1p2*s4*c1p4 − 3/1024 *δ*η*s5β *s1*c1p5*c2 + 15/4096 *δ*η*s5β *s1*c1p5*c4 + 13/4096 *δ*η*s5β *s1*c1p5+ 27/256 *δ*η*sβ *s1p3*c1p3*c2 − 7/1024 *δ*η*sβ *s1p3*c1p3*c4 − 245/1024 *δ*η*sβ *s1p3*c1p3 + 27/512 *δ*η*sβ *s1p2*s2*c1p4− 7/1024 *δ*η*sβ *s1p2*s4*c1p4 − 27/512 *δ*η*sβ *s1*c1p5*c2 + 7/2048 *δ*η*sβ *s1*c1p5*c4 + 245/2048* δ*η*sβ* s1*c1p5− 3/1024 *δ*s5β *s1p3*c1p3*c2 + 15/4096 *δ*s5β* s1p3*c1p3*c4 + 13/4096 *δ*s5β *s1p3*c1p3 − 3/2048* δ*s5β *s1p2*s2*c1p4+ 15/4096* δ*s5β *s1p2*s4*c1p4 + 3/2048 *δ*s5β *s1*c1p5*c2 − 15/8192* δ*s5β *s1*c1p5*c4 − 13/8192 *δ*s5β *s1*c1p5− 27/512 *δ*sβ* s1p3*c1p3*c2 + 7/2048 *δ*sβ *s1p3*c1p3*c4 − 1675/2048 *δ*sβ *s1p3*c1p3 − 27/1024* δ*sβ *s1p2*s2*c1p4+ 7/2048* δ*sβ* s1p2*s4*c1p4 + 27/1024* δ*sβ* s1*c1p5*c2 − 7/4096* δ*sβ* s1*c1p5*c4 + 1675/4096* δ*sβ *s1*c1p5
		  dAdι(60) = − 8125/256 *δ*η*c3β *s1p3*c1p7 + 8125/1024* δ*η*c3β *s1*c1p9 + 8125/512 *δ*c3β* s1p3*c1p7 − 8125/2048 *δ*c3β *s1*c1p9− 9375/256 *δ*η*s5β *s1p3*c1p7 + 9375/1024 *δ*η*s5β *s1*c1p9 − 4375/128 *δ*η*sβ *s1p3*c1p7 + 4375/512* δ*η*sβ* s1*c1p9+ 9375/512* δ*s5β *s1p3*c1p7 − 9375/2048 *δ*s5β *s1*c1p9 + 4375/256 *δ*sβ *s1p3*c1p7 − 4375/1024* δ*sβ* s1*c1p9
		  dAdι(61) = 10017/512 *δ*η*c3β *s1p3*c1p3*c2 − 1701/2048 *δ*η*c3β *s1p3*c1p3*c4 − 45711/2048 *δ*η*c3β *s1p3*c1p3 + 10017/1024 *δ*η*c3β *s1p2*s2*c1p4− 1701/2048 *δ*η*c3β *s1p2*s4*c1p4 − 10017/1024 *δ*η*c3β *s1*c1p5*c2 + 1701/4096 *δ*η*c3β *s1*c1p5*c4 + 45711/4096 *δ*η*c3β* s1*c1p5− 10017/1024 *δ*c3β *s1p3*c1p3*c2 + 1701/4096 *δ*c3β *s1p3*c1p3*c4 + 149391/4096 *δ*c3β *s1p3*c1p3 − 10017/2048 *δ*c3β *s1p2*s2*c1p4+ 1701/4096 *δ*c3β *s1p2*s4*c1p4 + 10017/2048 *δ*c3β *s1*c1p5*c2 − 1701/8192 *δ*c3β *s1*c1p5*c4 − 149391/8192 *δ*c3β *s1*c1p5− 1701/512 *δ*η*s5β *s1p3*c1p3*c2 + 8505/2048* δ*η*s5β *s1p3*c1p3*c4 + 7371/2048 *δ*η*s5β *s1p3*c1p3 − 1701/1024 *δ*η*s5β *s1p2*s2*c1p4+ 8505/2048 *δ*η*s5β *s1p2*s4*c1p4 + 1701/1024 *δ*η*s5β* s1*c1p5*c2 − 8505/4096 *δ*η*s5β* s1*c1p5*c4 − 7371/4096 *δ*η*s5β *s1*c1p5+ 2187/256 *δ*η*sβ *s1p3*c1p3*c2 − 567/1024 *δ*η*sβ *s1p3*c1p3*c4 + 3195/1024 *δ*η*sβ *s1p3*c1p3 + 2187/512 *δ*η*sβ* s1p2*s2*c1p4− 567/1024 *δ*η*sβ *s1p2*s4*c1p4 − 2187/512 *δ*η*sβ* s1*c1p5*c2 + 567/2048* δ*η*sβ *s1*c1p5*c4 − 3195/2048 *δ*η*sβ* s1*c1p5+ 1701/1024* δ*s5β *s1p3*c1p3*c2 − 8505/4096 *δ*s5β *s1p3*c1p3*c4 − 7371/4096 *δ*s5β *s1p3*c1p3 + 1701/2048 *δ*s5β *s1p2*s2*c1p4− 8505/4096 *δ*s5β *s1p2*s4*c1p4 − 1701/2048 *δ*s5β* s1*c1p5*c2 + 8505/8192 *δ*s5β* s1*c1p5*c4 + 7371/8192 *δ*s5β *s1*c1p5− 2187/512 *δ*sβ *s1p3*c1p3*c2 + 567/2048* δ*sβ *s1p3*c1p3*c4 − 20475/2048 *δ*sβ *s1p3*c1p3 − 2187/1024 *δ*sβ *s1p2*s2*c1p4+ 567/2048* δ*sβ *s1p2*s4*c1p4 + 2187/1024* δ*sβ* s1*c1p5*c2 − 567/4096* δ*sβ *s1*c1p5*c4 + 20475/4096 *δ*sβ* s1*c1p5
		  dAdι(62) = 4375/256 *δ*η*c3β *s1p4*c1p6 − 1875/256* δ*η*c3β *s1p2*c1p8 − 4375/512 *δ*c3β *s1p4*c1p6 + 1875/512 *δ*c3β *s1p2*c1p8+ 21875/256 *δ*η*c5β *s1p4*c1p6 − 9375/256 *δ*η*c5β *s1p2*c1p8 − 21875/512 *δ*c5β *s1p4*c1p6 + 9375/512 *δ*c5β *s1p2*c1p8+ 30625/384* δ*η*cβ *s1p4*c1p6 − 4375/128* δ*η*cβ *s1p2*c1p8 − 30625/768 *δ*cβ *s1p4*c1p6 + 4375/256* δ*cβ* s1p2*c1p8
		  dAdι(63) = − 25/384 *δ*η*c2β *cβ *sβ2 *s1p4*c1p4*c2 + 5/384 *δ*η*c2β *cβ *sβ2* s1p4*c1p4 − 5/192* δ*η*c2β *cβ *sβ2* s1p3*s2*c1p5 + 5/128* δ*η*c2β *cβ *sβ2 *s1p2*c1p6*c2− 1/128 *δ*η*c2β *cβ *sβ2 *s1p2*c1p6 − 5/768* δ*c2β *cβ *sβ2 *s1p4*c1p4 + 1/256 *δ*c2β *cβ *sβ2 *s1p2*c1p6 + 25/768 *δ*c2β *cβ *s1p4*c1p4*c2+ 5/384 *δ*c2β *cβ *s1p3*s2*c1p5 − 5/256 *δ*c2β *cβ *s1p2*c1p6*c2 − 35/384* δ*η*cβ *sβ2 *s1p4*c1p4*c2 − 185/384* δ*η*cβ *sβ2 *s1p4*c1p4− 7/192* δ*η*cβ *sβ2 *s1p3*s2*c1p5 + 7/128 *δ*η*cβ *sβ2 *s1p2*c1p6*c2 + 37/128 *δ*η*cβ* sβ2 *s1p2*c1p6 + 35/768 *δ*cβ *sβ2 *s1p4*c1p4*c2 + 185/768 *δ*cβ *sβ2 *s1p4*c1p4+ 7/384 *δ*cβ *sβ2* s1p3*s2*c1p5 − 7/256 *δ*cβ *sβ2 *s1p2*c1p6*c2 − 37/256 *δ*cβ* sβ2 *s1p2*c1p6
		  dAdι(64) = 1/128 *δ*c2β* sβ3 *s1p5*c1p5 − 1/192 *δ*c2β *sβ3 *s1p3*c1p7 + 3/128* δ*sβ3 *s1p5*c1p5 − 1/64* δ*sβ3 *s1p3*c1p7
		  dAdι(65) = − 151/1024 *δ*η*c3β* s1p5*c1*c2 + 13/4096 *δ*η*c3β *s1p5*c1*c4 − 57/4096 *δ*η*c3β* s1p5*c1 − 151/1024 *δ*η*c3β *s1p4*s2*c1p2+ 13/2048 *δ*η*c3β *s1p4*s4*c1p2 + 151/512 *δ*η*c3β *s1p3*c1p3*c2 − 13/2048 *δ*η*c3β *s1p3*c1p3*c4 + 57/2048 *δ*η*c3β *s1p3*c1p3− 13/4096 *δ*c3β* s1p5*c1*c4 − 13/2048 *δ*c3β *s1p4*s4*c1p2 + 13/2048* δ*c3β *s1p3*c1p3*c4 + 151/2048 *δ*c3β *s1*c1*c2+ 825/8192 *δ*c3β *s1*c1 + 151/2048 *δ*c3β* s2*c1p2 + 3/1024 *δ*η*s5β *s1p5*c1*c2 + 15/4096 *δ*η*s5β *s1p5*c1*c4 + 13/4096 *δ*η*s5β *s1p5*c1+ 3/1024* δ*η*s5β *s1p4*s2*c1p2 + 15/2048 *δ*η*s5β *s1p4*s4*c1p2 − 3/512* δ*η*s5β* s1p3*c1p3*c2 − 15/2048 *δ*η*s5β *s1p3*c1p3*c4− 13/2048 *δ*η*s5β *s1p3*c1p3 + 27/512* δ*η*sβ *s1p5*c1*c2 + 7/2048 *δ*η*sβ *s1p5*c1*c4 + 245/2048 δ*η*sβ *s1p5*c1+ 27/512 *δ*η*sβ *s1p4*s2*c1p2 + 7/1024 *δ*η*sβ *s1p4*s4*c1p2 − 27/256 *δ*η*sβ *s1p3*c1p3*c2 − 7/1024 *δ*η*sβ *s1p3*c1p3*c4− 245/1024* δ*η*sβ *s1p3*c1p3 − 15/4096 *δ*s5β *s1p5*c1*c4 − 15/2048 *δ*s5β *s1p4*s4*c1p2 + 15/2048 *δ*s5β* s1p3*c1p3*c4− 3/2048* δ*s5β *s1*c1*c2 − 3/2048 *δ*s5β *s2*c1p2 − 7/4096 *δ*sβ* s1p5*c1*c4 − 7/2048 *δ*sβ *s1p4*s4*c1p2 + 7/2048 *δ*sβ *s1p3*c1p3*c4− 27/1024* δ*sβ *s1*c1*c2 + 1675/4096 *δ*sβ *s1*c1 − 27/1024 *δ*sβ* s2*c1p2
		  dAdι(66) = − 13125/512* δ*η*c3β *s1p5*c1p5 + 4375/256* δ*η*c3β *s1p3*c1p7 + 13125/1024 *δ*c3β *s1p5*c1p5 − 4375/512 *δ*c3β *s1p3*c1p7+ 65625/512* δ*η*s5β *s1p5*c1p5 − 21875/256 *δ*η*s5β* s1p3*c1p7 − 4375/256* δ*η*sβ *s1p5*c1p5 + 4375/384 *δ*η*sβ* s1p3*c1p7− 65625/1024 *δ*s5β *s1p5*c1p5 + 21875/512 *δ*s5β *s1p3*c1p7 + 4375/512 *δ*sβ* s1p5*c1p5 − 4375/768* δ*sβ *s1p3*c1p7
		  dAdι(67) = 10017/1024 *δ*η*c3β* s1p5*c1*c2 + 1701/4096 *δ*η*c3β* s1p5*c1*c4 + 45711/4096* δ*η*c3β *s1p5*c1 + 10017/1024* δ*η*c3β* s1p4*s2*c1p2+ 1701/2048* δ*η*c3β *s1p4*s4*c1p2 − 10017/512* δ*η*c3β* s1p3*c1p3*c2 − 1701/2048 *δ*η*c3β *s1p3*c1p3*c4 − 45711/2048 *δ*η*c3β* s1p3*c1p3− 10017/2048 *δ*c3β* s1p5*c1*c2 − 1701/8192 *δ*c3β *s1p5*c1*c4 − 149391/8192 *δ*c3β *s1p5*c1 − 10017/2048 *δ*c3β *s1p4*s2*c1p2− 1701/4096 *δ*c3β *s1p4*s4*c1p2 + 10017/1024* δ*c3β *s1p3*c1p3*c2 + 1701/4096 *δ*c3β *s1p3*c1p3*c4 + 149391/4096 *δ*c3β *s1p3*c1p3− 1701/1024* δ*η*s5β *s1p5*c1*c2 − 8505/4096 *δ*η*s5β *s1p5*c1*c4 − 7371/4096 *δ*η*s5β *s1p5*c1 − 1701/1024 *δ*η*s5β* s1p4*s2*c1p2− 8505/2048 *δ*η*s5β *s1p4*s4*c1p2 + 1701/512* δ*η*s5β *s1p3*c1p3*c2 + 8505/2048 *δ*η*s5β *s1p3*c1p3*c4 + 7371/2048* δ*η*s5β *s1p3*c1p3+ 2187/512* δ*η*sβ *s1p5*c1*c2 + 567/2048 *δ*η*sβ *s1p5*c1*c4 − 3195/2048 *δ*η*sβ *s1p5*c1 + 2187/512 *δ*η*sβ *s1p4*s2*c1p2+ 567/1024* δ*η*sβ *s1p4*s4*c1p2 − 2187/256* δ*η*sβ* s1p3*c1p3*c2 − 567/1024* δ*η*sβ *s1p3*c1p3*c4 + 3195/1024* δ*η*sβ *s1p3*c1p3+ 1701/2048* δ*s5β *s1p5*c1*c2 + 8505/8192 *δ*s5β *s1p5*c1*c4 + 7371/8192* δ*s5β *s1p5*c1 + 1701/2048* δ*s5β *s1p4*s2*c1p2+ 8505/4096* δ*s5β* s1p4*s4*c1p2 − 1701/1024* δ*s5β *s1p3*c1p3*c2 − 8505/4096 *δ*s5β* s1p3*c1p3*c4 − 7371/4096* δ*s5β *s1p3*c1p3− 2187/1024* δ*sβ *s1p5*c1*c2 − 567/4096 *δ*sβ *s1p5*c1*c4 + 20475/4096* δ*sβ *s1p5*c1 − 2187/1024* δ*sβ* s1p4*s2*c1p2− 567/2048* δ*sβ* s1p4*s4*c1p2 + 2187/512 *δ*sβ *s1p3*c1p3*c2 + 567/2048* δ*sβ *s1p3*c1p3*c4 − 20475/2048* δ*sβ *s1p3*c1p3
		  dAdι(68) = 5/128 *δ*η*c2β* cβ* sβ2 *s1p6*c1p2*c2 + 1/128 *δ*η*c2β *cβ *sβ2 *s1p6*c1p2 + 5/192 *δ*η*c2β* cβ *sβ2 *s1p5*s2*c1p3− 25/384 *δ*η*c2β* cβ *sβ2 *s1p4*c1p4*c2 − 5/384 *δ*η*c2β* cβ *sβ2* s1p4*c1p4 − 5/256 *δ*c2β* cβ* sβ2*s1p6*c1p2*c2− 1/256* δ*c2β *cβ *sβ2 *s1p6*c1p2 − 5/384 *δ*c2β *cβ *sβ2 *s1p5*s2*c1p3 + 25/768* δ*c2β *cβ *sβ2 *s1p4*c1p4*c2+ 5/768* δ*c2β *cβ *sβ2 *s1p4*c1p4 + 7/128 *δ*η*cβ *sβ2* s1p6*c1p2*c2 − 37/128 *δ*η*cβ *sβ2 *s1p6*c1p2 + 7/192 *δ*η*cβ *sβ2 *s1p5*s2*c1p3− 35/384 *δ*η*cβ *sβ2 *s1p4*c1p4*c2 + 185/384* δ*η*cβ *sβ2 *s1p4*c1p4 − 7/256 *δ*cβ *sβ2 *s1p6*c1p2*c2 + 37/256* δ*cβ *sβ2 *s1p6*c1p2− 7/384* δ*cβ *sβ2 *s1p5*s2*c1p3 + 35/768 *δ*cβ *sβ2 *s1p4*c1p4*c2 − 185/768* δ*cβ *sβ2 *s1p4*c1p4
		  dAdι(69) = 1/96* δ*η*c2β *sβ3* s1p7*c1p3 − 1/64 *δ*η*c2β *sβ3 *s1p5*c1p5 + 1/128 *δ*c2β *sβ3 *s1p5*c1 + 1/32 *δ*η*sβ3 *s1p7*c1p3 − 3/64* δ*η*sβ3 *s1p5*c1p5− 1/64* δ*sβ3* s1p7*c1p3 + 3/128 *δ*sβ3 *s1p5*c5
		  dAdι(70) = − 1053/256 *δ*η*c2β* sβ* s1p6*s2 + 1701/512* δ*η*c2β *sβ *s1p6*s4 + 3159/256 *δ*η*c2β* sβ* s1p5*c1*c2− 5103/1024 *δ*η*c2β* sβ *s1p5*c1*c4 + 14067/1024 *δ*η*c2β* sβ* s1p5*c1 + 1053/512 *δ*c2β *sβ *s1p6*s2 − 1701/1024 *δ*c2β *sβ *s1p6*s4− 3159/512 *δ*c2β* sβ* s1p5*c1*c2 + 5103/2048 *δ*c2β *sβ *s1p5*c1*c4 − 24435/2048 *δ*c2β* sβ *s1p5*c1 + 2187/1024 *δ*η*c4β *sβ *s1p6*s2+ 3645/2048 *δ*η*c4β *sβ *s1p6*s4 − 6561/1024 *δ*η*c4β* sβ* s1p5*c1*c2 − 10935/4096* δ*η*c4β* sβ* s1p5*c1*c4 − 17253/4096 *δ*η*c4β *sβ *s1p5*c1− 2187/2048* δ*c4β *sβ *s1p6*s2 − 3645/4096 *δ*c4β *sβ *s1p6*s4 + 6561/2048* δ*c4β *sβ *s1p5*c1*c2 + 10935/8192 *δ*c4β *sβ *s1p5*c1*c4+ 17253/8192 *δ*c4β *sβ *s1p5*c1 + 297/1024 *δ*η*sβ *s1p6*s2 + 5103/2048 *δ*η*sβ *s1p6*s4 − 891/1024* δ*η*sβ *s1p5*c1*c2− 15309/4096 *δ*η*sβ *s1p5*c1*c4 + 42201/4096 *δ*η*sβ *s1p5*c1 − 297/2048* δ*sβ2 *s1p6*s2*c4 − 297/1024* δ*sβ2 *s1p6*s4*c2+ 891/2048 *δ*sβ2 *s1p5*c1*c2*c4 − 5103/4096* δ*sβ *s1p6*s4 + 15309/8192 *δ*sβ *s1p5*c1*c4 − 166617/8192* δ*sβ* s1p5*c1
		  dAdι(71) = 4375/256 *δ*η*c3β *s1p7*c1p3 − 13125/512 *δ*η*c3β* s1p5*c1p5 − 4375/512* δ*c3β *s1p7*c1p3 + 13125/1024 *δ*c3β *s1p5*c1p5− 21875/256 *δ*η*s5β* s1p7*c1p3 + 65625/512 *δ*η*s5β *s1p5*c1p5 + 4375/384 *δ*η*sβ *s1p7*c1p3 − 4375/256 *δ*η*sβ *s1p5*c1p5− 21875/512 *δ*s5β *s1p7*c1p3 + 65625/1024 *δ*s5β *s1p5*c1p5 − 4375/768 *δ*sβ *s1p7*c1p3 + 4375/512 *δ*sβ *s1p5*c1p5
		  dAdι(72) = 1875/256* δ*η*c3β* s1p8*c1p2 − 4375/256* δ*η*c3β *s1p6*c1p4 − 187/512* δ*c3β *s1p8*c1p2 + 4375/512 *δ*c3β *s1p6*c1p4+ 9375/256 *δ*η*c5β *s1p8*c1p2 − 21875/256 *δ*η*c5β *s1p6*c1p4 − 9375/512* δ*c5β *s1p8*c1p2 + 21875/512 *δ*c5β *s1p6*c1p4+ 4375/128 *δ*η*cβ *s1p8*c1p2 − 30625/384 *δ*η*cβ *s1p6*c1p4 − 4375/256 *δ*cβ* s1p8*c1p2 + 30625/768 *δ*cβ *s1p6*c1p4
		  dAdι(73) = 405/256 *δ*η*c2β *cβ *sβ2 *s1p8*c2 + 243/256 *δ*η*c2β* cβ* sβ2 *s1p8 + 405/128 *δ*η*c2β *cβ *sβ2 *s1p7*s2*c1− 2835/256 *δ*η*c2β *cβ *sβ2* s1p6*c1p2*c2 − 1701/256* δ*η*c2β* cβ *sβ2 *s1p6*c1p2 − 405/512 *δ*c2β *cβ *sβ2 *s1p8*c2− 243/512 *δ*c2β *cβ *sβ2 *s1p8 − 405/256 *δ*c2β* cβ *sβ2 *s1p7*s2*c1 + 2835/512 *δ*c2β *cβ *sβ2 *s1p6*c1p2*c2+ 1701/512 *δ*c2β *cβ *sβ2 *s1p6*c1p2 + 567/256 *δ*η*cβ *sβ2 *s1p8*c2 − 351/256 *δ*η*cβ *sβ2 *s1p8 + 567/128 *δ*η*cβ *sβ2 *s1p7*s2*c1− 3969/256 *δ*η*cβ *sβ2 *s1p6*c1p2*c2 + 2457/256 *δ*η*cβ *sβ2 *s1p6*c1p2 − 567/512 *δ*cβ *sβ2 *s1p8*c2 + 351/512 *δ*cβ *sβ2 *s1p8− 567/256 *δ*cβ *sβ2 *s1p7*s2*c1 + 3969/512 *δ*cβ *sβ2 *s1p6*c1p2*c2 − 2457/512* δ*cβ *sβ2 *s1p6*c1p2
		  dAdι(74) = 81/128 *δ*η*c2β *sβ3 *s1p9*c1 − 81/32 *δ*η*c2β *sβ3 *s1p7*c1p3 + 81/256 *δ*c2β *sβ3 *s1p9*c1 − 81/64 *δ*c2β *sβ3 *s1p7*c1p3+ 243/128 *δ*η*sβ3 *s1p9*c1 − 243/32 *δ*η*sβ3 *s1p7*c1p3 − 243/256* δ*sβ3 *s1p9*c1 + 243/64 *δ*sβ3 *s1p7*c1p3
		  dAdι(75) = 8125/1024 *δ*η*c3β *s1p9*c1 − 8125/256 *δ*η*c3β *s1p7*c1p3 − 8125/2048 *δ*c3β *s1p9*c1 + 8125/512 *δ*c3β *s1p7*c1p3+ 9375/1024 *δ*η*s5β *s1p9*c1 − 9375/256 *δ*η*s5β *s1p7*c1p3 + 4375/512 *δ*η*sβ *s1p9*c1 − 4375/128 *δ*η*sβ *s1p7*c1p3− 9375/2048 *δ*s5β *s1p9*c1 + 9375/512 *δ*s5β *s1p7*c1p3 − 4375/1024 *δ*sβ* s1p9*c1 + 4375/256 *δ*sβ *s1p7*c1p3
		  dAdι(76) = 3125/768 *δ*η*c3β *sβ2 *s1p10 − 9375/256 *δ*η*c3β *sβ2* s1p8*c1p2 − 3125/1536* δ*c3β *sβ2 *s1p10 + 9375/512 *δ*c3β* sβ2 *s1p8*c1p2+ 11875/768 *δ*η*cβ *sβ2 *s1p10 − 35625/256* δ*η*cβ* sβ2 *s1p8*c1p2 − 11875/1536 *δ*cβ* sβ2* s1p10 + 35625/512 *δ*cβ *sβ2 *s1p8*c1p2
		  dAdι(77) = − 3125/384 *δ*η*c2β *sβ3* s1p9*c1 + 3125/768 *δ*c2β* sβ3* s1p9*c1 − 3125/128* δ*η*sβ3 *s1p9*c1 + 3125/256* δ*sβ3* s1p9*c1
		  dAdι(78) = 5103/1024 *δ*η*c2β* cβ *sβ2 *s2p3*s4 − 15309/2048 *δ*η*c2β *cβ* sβ2 *s2p2*c2*c4 − 11907/2048 *δ*η*c2β *cβ *sβ2 *s2p2*c2− 5103/2048 *δ*c2β *cβ *sβ2* s2p3*s4 + 15309/4096 *δ*c2β *cβ *sβ2* s2p2*c2*c4 + 11907/4096 *δ*c2β *cβ *sβ2 *s2p2*c2+ 1701/1024 *δ*η*cβ *sβ2 *s2p3*s4 − 5103/2048 *δ*η*cβ *sβ2 *s2p2*c2*c4 + 30591/2048 *δ*η*cβ *sβ2* s2p2*c2− 1701/2048 *δ*cβ *sβ2 *s2p3*s4 + 5103/4096 *δ*cβ* sβ2* s2p2*c2*c4 − 134271/4096 *δ*cβ* sβ2* s2p2*c2
		  dAdι(79) = − 65625/2048 *δ*η*c3β *sβ2 *s24*c2 + 65625/4096 *δ*c3β *sβ2*s2p4*c2 − 109375/2048 *δ*η*cβ *sβ2* s2p4*c2 + 109375/4096* δ*cβ* s2β *s2p4*c2
		  dAdι(80) = − 243/512 *δ*η*c2β *cβ *s2p2 + 5319/2048 *δ*η*c2β* cβ *s2*s4 + 243/512 *δ*η*c2β* cβ *c2p2 − 5319/4096 *δ*η*c2β* cβ* c2*c4+ 5967/8192 *δ*η*c2β* cβ* c2 + 2835/1024 *δ*c2β* cβ* s2p2 − 135/4096 *δ*c2β *cβ *s2*s4 − 2835/1024 *δ*c2β* cβ* c2p2+ 135/8192 *δ*c2β *cβ* c2*c4 − 37071/16384 *δ*c2β *cβ *c2 + 2565/4096 *δ*η*c3β* s8 − 1215/65536 *δ*η*c3β *c10− 2565/8192 *δ*c3β* s8 + 1215/131072 *δ*c3β* c10 − 567/16384 *δ*η*c4β *cβ *c2 + 81/2048* δ*η*c4β* cβ *c4− 3159/32768 *δ*η*c4β *cβ* c6 + 567/32768 *δ*c4β *cβ* c2 − 81/4096 *δ*c4β *cβ* c4 + 3159/65536* δ*c4β *cβ* c6+ 729/4096 *δ*η*c5β* s8 − 6075/65536 *δ*η*c5β* c10 − 729/8192 *δ*c5β* s8 + 6075/131072 *δ*c5β *c10− 567/16384 *δ*η*c8*cβ *c2 + 567/32768 *δ*c8*cβ* c2 − 4005/4096 *δ*η*cβ *s2p2 − 6633/4096 *δ*η*cβ *s2*s4+ 1539/4096 *δ*η*cβ *s2*s6 + 4005/4096 *δ*η*cβ* c2p2 + 6633/8192* δ*η*cβ* c2*c4 − 513/4096 *δ*η*cβ *c2*c6+ 2457/4096 *δ*η*cβ *c2 + 10917/8192 *δ*cβ *s2p2 + 10089/8192 *δ*cβ *s2*s4 − 1539/8192 *δ*cβ *s2*s6− 10917/8192* δ*cβ *c2p2 − 10089/16384 *δ*cβ *c2*c4 + 513/8192 *δ*cβ *c2*c6 − 7641/8192 *δ*cβ* c2
		  dAdι(81) = 16929/4096 *δ*η*c2β *cβ* c6 − 18603/8192 *δ*c2β *cβ *c2 + 2835/1024 *δ*c2β *cβ* c4 + 405/16384 *δ*c2β *cβ* c6− 2565/4096 *δ*η*c3β *s8 − 1215/65536 *δ*η*c3β *c10 + 2565/8192 *δ*c3β* s8 + 1215/131072 *δ*c3β *c10 − 1701/16384 *δ*η*c4β* cβ* c6+ 567/32768 *δ*c4β *cβ* c2 + 81/4096 *δ*c4β *cβ *c4 + 3159/65536 *δ*c4β *cβ *c6 − 729/4096 *δ*η*c5β *s8 − 6075/65536* δ*η*c5β* c10+ 729/8192* δ*c5β* s8 + 6075/131072* δ*c5β* c10 − 513/2048 *δ*η*cβ *s8 + 9585/16384 *δ*η*cβ *c6 − 2835/32768 *δ*η*cβ *c10+ 513/4096 *δ*cβ* s8 − 20475/32768* δ*cβ* c2 + 5715/4096 *δ*cβ *c4 − 62235/65536 *δ*cβ *c6 + 2835/65536* δ*cβ* c10
		  dAdι(82) = − 11/768 *δ*η*c2β *cβ *s2p2 − 77/1024 *δ*η*c2β *cβ *s2*s4 + 11/768 *δ*η*c2β *cβ *c2p2 + 77/2048 *δ*η*c2β *cβ *c2*c4+ 257/12288* δ*η*c2β* cβ* c2 − 133/1536 *δ*c2β* cβ* s2p2 − 211/2048* δ*c2β *cβ *s2*s4 + 133/1536 *δ*c2β *cβ *c2p2+ 211/4096* δ*c2β *cβ *c2*c4 + 319/24576* δ*c2β *cβ *c2 − 45/2048 *δ*η*c3β* s8 + 5/32768 *δ*η*c3β* c10 + 45/4096* δ*c3β *s8− 5/65536* δ*c3β *c10 − 1/24576 *δ*η*c4β* cβ *c2 + 1/3072 *δ*η*c4β *cβ *c4 − 3/16384 *δ*η*c4β *cβ *c6+ 1/49152* δ*c4β* cβ* c2 − 1/6144 *δ*c4β* cβ* c4 + 3/32768* δ*c4β *cβ *c6 − 1/2048 *δ*η*c5β* s8 + 25/32768 *δ*η*c5β *c10+ 1/4096* δ*c5β *s8 − 25/65536 *δ*c5β* c10 + 7/24576 *δ*η*c8*cβ *c2 − 7/49152 *δ*c8*cβ *c2+ 49/6144 *δ*η*cβ *s2*s4 − 11/2048 *δ*η*cβ *s2*s6 − 49/12288 *δ*η*cβ *c2*c4 + 11/6144 *δ*η*cβ *c2*c6− 1493/6144 *δ*η*cβ* c2 − 933/4096 *δ*cβ *s2p2 − 625/12288 *δ*cβ *s2*s4 + 11/4096 *δ*cβ *s2*s6+ 933/4096 *δ*cβ *c2p2 + 625/24576 *δ*cβ* c2*c4 − 11/12288 *δ*cβ *c2*c6 + 871/4096 *δ*cβ *c2 + 1391/6144 *δ*η*s2p2− 1391/6144 *δ*η*c2p2
		  dAdι(83) = 13/6144 *δ*η*c2β *cβ* c2 − 11/768 *δ*η*c2β *cβ* c4 + 231/4096* δ*η*c2β *cβ *c6 − 157/12288* δ*c2β *cβ* c2− 133/1536 *δ*c2β *cβ *c4 + 633/8192 *δ*c2β *cβ *c6 + 45/2048 *δ*η*c3β *s8 + 5/32768 *δ*η*c3β *c10 − 45/4096 *δ*c3β *s8− 5/65536 *δ*c3β *c10 − 1/24576 *δ*η*c4β *cβ *c2 − 1/3072 *δ*η*c4β *cβ* c4 − 3/16384 *δ*η*c4β *cβ *c6+ 1/49152* δ*c4β *cβ *c2 + 1/6144* δ*c4β *cβ* c4 + 3/32768 *δ*c4β *cβ *c6 + 1/2048 *δ*η*c5β *s8+ 25/32768* δ*η*c5β *c10 − 1/4096* δ*c5β *s8 − 25/65536 *δ*c5β *c10 + 11/3072 *δ*η*cβ *s8− 5923/24576 *δ*η*cβ* c2 + 701/3072 *δ*η*cβ *c4 − 105/16384* δ*η*cβ* c6 + 35/49152 *δ*η*cβ* c10− 11/6144 *δ*cβ* s8 + 9827/49152 *δ*cβ *c2 − 1405/6144 *δ*cβ *c4 + 1257/32768 *δ*cβ *c6 − 35/98304* δ*cβ *c10
		  dAdι(84) = − 1/4096 *δ*η*c2β *cβ *sβ2 *c2 + 21/8192 *δ*η*c2β *cβ *sβ2* c6 + 1/8192 *δ*c2β *cβ *sβ2 *c2 + 21/16384 *δ*c2β *cβ *sβ2 *c6- 105/16384 *δ*η*c3β *sβ2 *c10 + 105/32768 *δ*c3β *sβ2 *c10 − 43/4096 *δ*η*cβ *sβ2 *c2 − 1287/8192 *δ*η*cβ *sβ2 *c6− 175/16384 *δ*η*cβ* sβ2 *c10 − 341/8192 *δ*cβ* sβ2 *c2 − 10233/16384* δ*cβ* sβ2 *c6 + 175/32768 *δ*cβ *sβ2 *c10
		  dAdι(85) = 3*η*cβ* sβ* c2p2*s2*χsx − 6*η*cβ* sβ* c2p2*s2*χsx
		  dAdι(86) = − 1/3* η*c2β* s1*c1p3*c2*χsz + 7*η*c2β *s1*c1p3*χsz − 1/6 *η*c2β *s2*c1p4*χsz − 10/3 *c2β *s1*c1p3*c4*χsz + 2*c2β *s1*c1p3*χsz− 10/3 *c2β *s4*c1p4*χsz + 2/3 *η*s2β *s1*c1p3*c2*χsx + 19/3 *η*s2β *s1*c1p3*χsx + 1/3 *η*s2β *s2*c1p4*χsx+ 20/3 *s2β *s1*c1p3*c2*χsx − 14/3 *s2β *s1*c1p3*χsx + 10/3 *s2β* s2*c1p4*χsx − η*s1*c1p3*c2*χsz + 5*η*s1*c1p3*χsz− 1/2 *η*s2*c1p4*χsz − 10*s1*c1p3*c4*χsz + 6*s1*c1p3*χsz − 10*s4*c1p4*χsz
		  dAdι(87) = − 5/12 *η*c2β *s1p2*c1p4*χsx + 1/12 *η*c2β* c1p6*χsx − 25/6 *c2β *s1p2*c1p4*χsx + 5/6* c2β* c1p6*χsx − 5/4* η*s1p2*c1p4*χsx+ 1/4* η*c1p6*χsx − 25/2 *s1p2*c1p4*χsx + 5/2 *c1p6*χsx
		  dAdι(88) = − 1/12 *η*c2β *s1p6*χsx + 5/12 *η*c2β *s1p4*c1p2*χsx − 5/6 *c2β *s1p6*χsx + 25/6 *c2β *s1p4*c1p2*χsx − 1/4 *η*s1p6*χsx+ 5/4 *η*s1p4*c1p2*χsx − 5/2 *s1p6*χsx + 25/2 *s1p4*c1p2*χsx
		  dAdι(89) = − 7/8 *η*c2β *s1p2*c1p2*c2*χsx − 79/8 *η*c2β *s1p2*c1p2*χsx − 7/12 *η*c2β *s1*s2*c1p3*χsx + 7/24 *η*c2β *c1p4*c2*χsx+ 79/24 *η*c2β* c1p4*χsx − 35/4 *c2β *s1p2*c1p2*c2*χsx + 13/4 *c2β *s1p2*c1p2*χsx − 35/6 *c2β *s1*s2*c1p3*χsx+ 35/12 *c2β *c1p4*c2*χsx − 13/12 *c2β *c1p4*χsx + 3/8* η*s1p2*c1p2*c2*χsx + 51/8* η*s1p2*c1p2*χsx + 1/4* η*s1*s2*c1p3*χsx− 1/8 *η*c1p4*c2*χsx − 17/8 *η*c1p4*χsx + 15/4 *s1p2*c1p2*c2*χsx − 9/4 *s1p2*c1p2*χsx + 5/2 *s1*s2*c1p3*χsx− 5/4 *c1p4*c2*χsx + 3/4 *c1p4*χsx − η*s2β *s1p2*c1p2*c2*χsz + 21/2 *η*s2β *s1p2*c1p2*χsz − 2/3 *η*s2β *s1*s2*c1p3*χsz+ 1/3 *η*s2β *c1p4*c2*χsz − 7/2 *η*s2β* c1p4*χsz − 10*s2β *s1p2*c1p2*c2*χsz + 3*s2β *s1p2*c1p2*χsz − 20/3 *s2β *s1*s2*c1p3*χsz+ 10/3 *s2β *c1p4*c2*χsz − s2β *c1p4*χsz
		  dAdι(90) = − 7/8 *δ*η*c2β *s1p2*c1p2*c2*χsx − 79/8 *δ*η*c2β *s1p2*c1p2*χsx − 7/12 *δ*η*c2β *s1*s2*c1p3*χsx + 7/24 *δ*η*c2β *c1p4*c2*χsx+ 79/24 *δ*η*c2β *c1p4*χsx − 35/4 *δ*c2β *s1p2*c1p2*c2*χsx + 13/4* δ*c2β *s1p2*c1p2*χsx − 35/6 *δ*c2β *s1*s2*c1p3*χsx+ 35/12 *δ*c2β *c1p4*c2*χsx − 13/12 *δ*c2β *c1p4*χsx + 3/8* δ*η*s1p2*c1p2*c2*χsx + 51/8* δ*η*s1p2*c1p2*χsx + 1/4 *δ*η*s1*s2*c1p3*χsx− 1/8 *δ*η*c1p4*c2*χsx − 17/8* δ*η*c1p4*χsx + 15/4 *δ*s1p2*c1p2*c2*χsx − 9/4 *δ*s1p2*c1p2*χsx + 5/2* δ*s1*s2*c1p3*χsx− 5/4 *δ*c1p4*c2*χsx + 3/4 *δ*c1p4*χsx − δ*η*s2β *s1p2*c1p2*c2*χsz + 21/2 *δ*η*s2β *s1p2*c1p2*χsz − 2/3* δ*η*s2β *s1*s2*c1p3*χsz+ 1/3 *δ*η*s2β *c1p4*c2*χsz − 7/2 *δ*η*s2β *c1p4*χsz − 10*δ*s2β *s1p2*c1p2*c2*χsz + 3*δ*s2β *s1p2*c1p2*χsz − 20/3 *δ*s2β *s1*s2*c1p3*χsz+ 10/3* δ*s2β *c1p4*c2*χsz − δ*s2β *c1p4*χsz
		  dAdι(91) = − 1/6 *η*c2β *s1p4*s2*χsz + 1/3 *η*c2β *s1p3*c1*c2*χsz + 7/3* η*c2β* s1p3*c1*χsz − 5/3 *c2β *s1p4*s2*χsz+ 10/3 *c2β *s1p3*c1*c2*χsz + 2/3 *c2β *s1p3*c1*χsz + 1/3 *η*s2β *s1p4*s2*χsx − 2/3 *η*s2β *s1p3*c1*c2*χsx + 19/3 *η*s2β *s1p3*c1*χsx+ 10/3 *s2β *s1p4*s2*χsx − 20/3 *s2β *s1p3*c1*c2*χsx − 14/3 *s2β *s1p3*c1*χsx − 1/2* η*s1p4*s2*χsz + η*s1p3*c1*c2*χsz+5*η*s1p3*c1*χsz − 5*s1p4*s2*χsz + 10*s1p3*c1*c2*χsz + 6*s1p3*c1*χsz
		  dAdι(92) = − 3/2* η*sβ2 *s2p3*χsz + 3*η*sβ2 *s2*c2p2*χsz + 3*sβ2 *s2p3*χsz − 6*sβ2 *s2*c2p2*χsz
		  dAdι(93) = − 3/8 *η*c2β *s2p2*c2*χsx + 3/4 *c2β* s2p2*c2*χsx − 9/8* η*s2p2*c2*χsx + 9/4 *s2p2*c2*χsx
		  dAdι(94) = − 1/3 *η*cβ *sβ *s2p3*χsx + 2/3 *η*cβ *sβ *s2*c2p2*χsx − 10/3 *cβ *sβ *s2p3*χsx + 20/3 *cβ *sβ *s2*c2p2*χsx− 1/2* η*sβ2 *s2p3*χsz + η*sβ2 *s2*c2p2*χsz − 5*sβ2 *s2p3*χsz + 10*sβ2 *s2*c2p2*χsz
		  dAdι(95) = 1/4 *η*c2β* s2p3*χsz − 1/2 *η*c2β* s2*c2p2*χsz − 1/2 *c2β* s2p3*χsz + c2β* s2*c2p2*χsz − 1/2 *η*s2β *s2p3*χsx+η*s2β *s2*c2p2*χsx + s2β *s2p3*χsx − 2*s2β *s2*c2p2*χsx + 3/4 *η*s2p3*χsz − 3/2 *η*s2*c2p2*χsz − 3/2* s2p3*χsz + 3*s2*c2p2*χsz
		  dAdι(96) = 11/32 *η*c2β *c2*χsx + 21/32 *η*c2β *c6*χsx − 11/16 *c2β* c2*χsx − 21/16 *c2β* c6*χsx + 9/8* η*s2p2*c2*χsx− 9/4 *s2p2*c2*χsx − 1/4* η*s2β *c2*χsz + 3/4* η*s2β *c6*χsz + 1/2 *s2β* c2*χsz − 3/2 *s2β* c6*χsz
		  dAdι(97) = − 5/8 *η*c2β *s2*s4*χsy + 5/16 *η*c2β* c2*c4*χsy + 3/16 *η*c2β *c2*χsy + 5/4 *c2β *s2*s4*χsy − 5/8* c2β *c2*c4*χsy− 3/8 *c2β* c2*χsy + 9/8 *η*s2*s4*χsy − 9/16* η*c2*c4*χsy − 15/16* η*c2*χsy − 9/4* s2*s4*χsy + 9/8 *c2*c4*χsy + 15/8 *c2*χsy
		  dAdι(98) = −η*cβ* sβ* s2p3*χsy + 2*η*cβ *sβ *s2*c2p2*χsy + 2*cβ *sβ *s2p3*χsy − 4*cβ* sβ *s2*c2p2*χsy
		  dAdι(99) = − 3/8 *η*c2β* s2p2*c2*χsy + 3/4 *c2β* s2p2*c2*χsy − 9/8* η*s2p2*c2*χsy + 9/4 *s2p2*c2*χsy
		  dAdι(100) = 5/24 *η*c2β *s1p4*c2*χsy + 31/24* η*c2β *s1p4*χsy + 5/12 *η*c2β *s1p3*s2*c1*χsy − 5/8* η*c2β *s1p2*c1p2*c2*χsy− 31/8 *η*c2β *s1p2*c1p2*χsy + 25/12 *c2β *s1p4*c2*χsy − 55/24 *c2β *s1p4*χsy + 25/6 *c2β* s1p3*s2*c1*χsy − 25/4 *c2β *s1p2*c1p2*c2*χsy+ 55/8 *c2β *s1p2*c1p2*χsy − 3/8* η*s1p4*c2*χsy − 1/8* η*s1p4*χsy − 3/4* η*s1p3*s2*c1*χsy + 9/8 *η*s1p2*c1p2*c2*χsy+ 3/8* η*s1p2*c1p2*χsy − 15/4 *s1p4*c2*χsy − 15/2 *s1p3*s2*c1*χsy + 45/4 *s1p2*c1p2*c2*χsy
		  dAdι(101) = 1/3 *η*s2β* s1p4*s2*χsy − 2/3 *η*s2β *s1p3*c1*c2*χsy − 5/3* η*s2β* s1p3*c1*χsy − 14/3 *s2β *s1p3*c1*χsy+ 20/3 *s2β *s1*c1p3*c2*χsy + 10/3 *s2β* s2*c1p4*χsy
		  dAdι(102) = − 1/12 *η*c2β *s1p6*χsy + 5/12 *η*c2β *s1p4*c1p2*χsy − 5/6 *c2β *s1p6*χsy + 25/6 *c2β *s1p4*c1p2*χsy − 1/4* η*s1p6*χsy+ 5/4* η*s1p4*c1p2*χsy − 5/2 *s1p6*χsy + 25/2 *s1p4*c1p2*χsy
		  dAdι(103) = − 11/3 *η*cβ *sβ *s2*c2*χsy − 2/3 *cβ *sβ *s2*c2*χsy
		  dAdι(104) = − 5/8 *η*c2β *s1p2*c1p2*c2*χsy + 31/8 *η*c2β *s1p2*c1p2*χsy − 5/12 *η*c2β* s1*s2*c1p3*χsy + 5/24 *η*c2β *c1p4*c2*χsy− 31/24 *η*c2β* c1p4*χsy − 25/4* c2β *s1p2*c1p2*c2*χsy + 11/4 *c2β *s1p2*c1p2*χsy − 25/6 *c2β *s1*s2*c1p3*χsy+ 25/12 *c2β *c1p4*c2*χsy − 11/12 *c2β *c1p4*χsy + 9/8 *η*s1p2*c1p2*c2*χsy − 3/8 *η*s1p2*c1p2*χsy + 3/4 *η*s1*s2*c1p3*χsy− 3/8 *η*c1p4*c2*χsy + 1/8 *η*c1p4*χsy + 45/4 *s1p2*c1p2*c2*χsy − 15/4 *s1p2*c1p2*χsy + 15/2 *s1*s2*c1p3*χsy − 15/4 *c1p4*c2*χsy + 5/4* c1p4*χsy
		  dAdι(105) = 2/3*η*s2β*s1*c1p3*c2*χsyDN − 5/3*η*s2β*s1*c1p3*χsyDN + 1/3*η*s2β*s2*c1p4*χsyDN + 20/3*s2βs1*c1p3 c2*χsyDN − 14/3*s2β*s1*c1p3*χsyDN + 10/3*s2β*s2*c1p4*χsyDN
		  dAdι(106) = 5/12*η*c2β*s1p2*c1p4*χsyDN + 1/12*η*c2β*c1p6*χsyDN − 25/6*c2β*s1p2*c1p4*χsyDN + 5/6*c2β*c1p6*χsyDN − 5/4*η*s1p2*c1p4*χsyDN + 1/4*η*c1p6*χsyDN − 25/2*s1p2*c1p4*χsyDN + 5/2*c1p6*χsyDN
		  dAdι(107) = − 6*δ*η*cβ*sβ*s2*c22*χaxDN
		  dAdι(108) = − 10/3*δ*η*c2β*s1*c1p3*c2*χazDN + 2 3*δ*η*c2β*s1*c1p3*χazDN − 5 3*δ*η*c2β*s2*c1p4*χazDN + 20/3*δ*s2β*s1*c1p3* c2*χaxDN − 14/3*δ*s2β*s1*c1p3*χaxDN + 10 3*δ*s2β*s2*c1p4*χaxDN − 10*δ*s1*c1p3*c2*χazDN + 6*δ*s1*c1p3*χazDN − 5*δ*s2*c1p4*χazDN
		  dAdι(109) = − 25/6*δ*η*c2β*s1p2*c1p4*χaxDN + 5/6*δ*η*c2β*c1p6*χaxDN − 25/2*δ*s1p2*c1p4*χaxDN + 5/2*δ*c1p6*χaxDN
		  dAdι(110) = − 5/6*δ*η*c2β*s1p6*χaxDN + 25/6*δ*η*c2β*s1p4*c1p2*χaxDN − 5/2*δ*s1p6*χaxDN + 25/2*δ*s1p4*c1p2*χaxDN
		  dAdι(111) = − 35/4*δ*η*c2β*s1p2*c1p2*c2*χaxDN + 13/4*δ*η*c2β*s1p2*c1p2*χaxDN − 35/6*δ*η*c2β*s1*s2c 3 1*χaxDN + 35/12*δ*η*c2β*c1p4*c2*χaxDN − 13/12*δ*η*c2β*c1p4*χaxDN + 15/4*δ*s1p2*c1p2*c2*χaxDN − 9/4*δ*s1p2*c1p2*χaxDN + 5/2*δ*s1*s2*c1p3*χaxDN − 5/4*δ*c1p4*c2*χaxDN + 3/4*δ*c1p4*χaxDN − 10*δ*s2β*s1p2*c1p2*c2*χazDN + 3*δ*s2β*s1p2*c1p2*χazDN − 20/3*δ*s2β*s1*s2*c1p3*χazDN + 10/3*δ*s2β*c1p4*c2*χazDN −*δ*s2β*c1p4*χazDN
		  dAdι(112) = 35/12*δ*η*c2β*s1p4*c2*χaxDN + 13/12*δ*η*c2β*s1p4*χaxDN + 35/6*δ*η*c2β*s1p3*s2*c1*χaxDN − 35/4*δ*η*c2β*s1p2*c1p2*c2*χaxDN − 13/4*δ*η*c2β*s1p2*c1p2*χaxDN − 5/4*δ*η*s1p4*c2*χaxDN − 3/4*δ*η*s1p4*χaxDN − 5/2*δ*η*s1p3*s2*c1*χaxDN + 15/4*δ*η*s1p2*c1p2*c2*χaxDN + 9/4*δ*η*s1p2*c1p2*χaxDN + 10/3*δ*η*s2β*s1p4*c2*χazDN + δ*η*s2β*s1p4*χazDN + 20/3*δ*η*s2β*s1p3*s2*c1*χazDN −10*δ*η*s2β*s1p2*c1p2*c2*χazDN − 3*δ*η*s2β*s1p2*c1p2*χazDN
		  dAdι(113) = − 5/3*δ*η*c2β*s1p4*s2*χazDN + 10/3*δ*η*c2β*s1p3*c1*c2*χazDN + 2/3*δ*η*c2β*s1p3*c1*χazDN + 10/3*δ*η*s2β*s1p4*s2*χaxDN − 20/3*δ*η*s2β*s1p3*c1*c2*χaxDN − 14/3*δ*η*s2β*s1p3*c1*χaxDN − 5*δ*η*s1p4*s2*χazDN + 10*δ*η*s1p3*c1*c2*χazDN + 6*δ*η*s1p3*c1*χazDN
		  dAdι(114) = 3*δ*η*sβ2*s2p3*χazDN − 6*δ*η*sβ2*s2*c22*χazDN
		  dAdι(115) = 3/4*δ*η*c2β*s22*c2*χaxDN + 9/4*δ*η*s22*c2*χaxDN
		  dAdι(116) = − 10/3*δ*η*cβ*sβ*s2p3*χaxDN + 20/3*δ*η*cβ*sβ*s2*c22*χaxDN − 5*δ*η*sβ2*s2p3*χazDN + 10*δ*η*sβ2*s2*c22*χazDN
		  dAdι(117) = − 1/2*δ*η*c2β*s2p3*χazDN + δ*η*c2β*s2*c22*χazDN + s2β*s2p3*χaxDN − 2*s2β*s2*c22*χaxDN − 3/2*δ*η*s2p3*χazDN + 3*δ*η*s2*c22*χazDN
		  dAdι(118) = − 11/16*δ*η*c2β*c2*χaxDN − 21/16*δ*η*c2β*c6*χaxDN − 9/4*δ*η*s22*c2*χaxDN + 1/2*δ*η*s2β*c2*χazDN − 3/2*δ*η*s2β*c6*χazDN
		  dAdι(119) = 5/4*δ*η*c2β*s2*s4*χayDN − 5/8*δ*η*c2β*c2*c4*χayDN − 3/8*δ*η*c2β*c2*χayDN − 9/4*δ*η*s2*s4*χayDN + 9/8*δ*η*c2*c4*χayDN + 15 8*δ*η*c2*χayDN
		  dAdι(120) = 2*δ*η*cβ*sβ*s2p3*χayDN − 4*δ*η*cβ*sβ*s2*c22*χayDN
		  dAdι(121) = 3/4*δ*η*c2β*s22*c2*χayDN + 9/4*δ*η*s22*c2*χayDN
		  dAdι(122) = 25/12*δ*η*c2β*s1p4*c2*χayDN + 11/12*δ*η*c2β*s1p4*χayDN + 25/6*δ*η*c2β*s1p3*s2*c1*χayDN − 25/4*δ*η*c2β*s1p2*c1p2*c2*χayDN − 11/4*δ*η*c2β*s1p2*c1p2*χayDN − 15/4*δ*η*s1p4*c2*χayDN − 5/4*δ*η*s1p4*χayDN − 15/2*δ*η*s1p3*s2*c1*χayDN + 45/4*δ*η*s1p2*c1p2*c2*χayDN + 15/4*δ*η*s1p2*c1p2*χayDN
		  dAdι(123) = 10/3*δ*η*s2β*s1p4*s2*χayDN − 20/3*δ*η*s2β*s1p3*c1c2*χayDN − 14/3*δ*η*s2β*s1p3*c1*χayDN 
		  dAdι(124) = − 5/6*δ*η*c2β*s1p6*χayDN  + 25/6*δ*η*c2β*s1p4*c1p2*χayDN  − 5/2*δ*η*s1p6*χayDN  + 25/2*δ*η*s1p4*c1p2*χayDN 
		  dAdι(125) = − 2/3*δ*η*cβ*sβ*s2*c2*χayDN 
		  dAdι(126) = − 25/4*δ*η*c2β*s1p2*c1p2*c2*χayDN  + 11/4*δ*η*c2β*s1p2*c1p2*χayDN  − 25/6*δ*η*c2βs1s2*c1p3*χayDN  + 25/12*δ*η*c2β*c1p4*c2*χayDN  − 11/12*δ*η*c2β*c1p4*χayDN  + 45/4*δ*η*s1p2*c1p2*c2*χayDN  − 15/4*δ*η*s1p2*c1p2*χayDN  + 15/2*δ*η*s1s2*c1p3*χayDN  − 15/4*δ*η*c1p4*c2*χayDN  + 5/4*δ*η*c1p4*χayDN 
		  dAdι(127) = 20/3*δ*η*s2β*s1*c1p3*c2*χayDN − 14/3*δ*η*s2β*s1*c1p3*χayDN + 10/3*δ*η*s2β*s2*c1p4*χayDN
		  dAdι(128) = − 25/6*δ*η*c2β*s1p2*c1p4*χayDN + 5/6*δ*η*c2β*c1p6*χayDN − 25/2*δ*η*s1p2*c1p4*χayDN + 5/2*δ*η*c1p6*χayDN
		  dAdι(129) = − 2*sβ*s1p4 + 6*sβ*s1p2*c1p2
		  dAdι(130) = − 4*cβ*s1p3*c1
		  dAdι(131) = 1/6*sβ*s1p2*c1p2 − 2*sβ*c1p4
		  dAdι(132) = 4*cβ*s1*c1p3
		  dAdι(133) = 45/8*δ*s2β*s1p5*c1 − 45/4*δs2β*s1p3*c1p3
		  dAdι(134) = − 9/4*δ*c2β*s1p6 + 45/4*δ*c2β*s1p4*c1p2
		  dAdι(135) = 27/8*δ*s2β*s1p5*c1
		  dAdι(136) = − 43/128*δ*cβ*sβ*s2 + 23/64*δ*s2β*s4 − 15/256*δ*s2β*s6
		  dAdι(137) = − 1/8*δ*c2β*s1p4*c2 + 1/8*δ*c2β*s1p4 − 1/4*δ*c2β*s1p3*s2*c1 + 3/8*δ*c2β*s1p2*c1p2*c2 − 3/8*δ*c2β*s1p2*c1p2 + 1/2*δ*s1p4 − 3/2*δ*s1p2*c1p2
		  dAdι(138) = − 1/8*δ*s2β*s1p5*c1 + 1/4*δ*s2β*s1p3*c1p3
		  dAdι(139) = δ*s2β*c4
		  dAdι(140) = − 43/128*δ*cβ*sβ*s2 − 23/64*δ*s2β*s4 − 15/256*δ*s2β*s6
		  dAdι(141) = − 3/8*δ*c2β*s1p2*c1p2*c2 + 3/8*δ*c2β*s1p2*c1p2− 1/4*δ*c2β*s1*s2*c1p3 + 1/8*δ*c2β*c1p4*c2 − 1/8*δ*c2β*c1p4 + 3/2*δ*s1p2*c1p2 − 1/2*δ*c1p4
		  dAdι(142) = 1/4*δ*s2β*s1p3*c1p3 − 1/8*δ*s2β*s1*c1p5
		  dAdι(143) = − 45/4*δ*s2β*s1p3*c1p3 + 45/8*δ*s2β*s1*c1p5
		  dAdι(144) = − 45/4*δ*c2β*s1p2*c1p4 + 9/4*δ*c2β*c1p6
		  dAdι(145) = 27/8*δ*s2β*s1*c1p5
		  dAdι(146) = 42*η*c3β*s1p6 *c1p2 − 70*η*c3β*s1p4*c1p4 − 14*c3β*s1p6 *c1p2 + 70/3*c3β*s1p4*c1p4 + 18*η*sβ*s1p6 *c1p2 −30*η*sβs 4 1 c 4 1 − 6*sβ*s1p6 *c1p2 + 10*sβ*s1p4*c1p4
		  dAdι(147) = − 56/3*η*c3β*s1p7*c1 + 56*η*c3β*s1p5*c1p3 − 8/3*η*cβ*s1p7*c1 + 8*η*cβ*s1p5*c1p3
		  dAdι(148) = − 6*η*c3β*s1p8 + 42*η*c3β*s1p6*c1p2 + 2*c3β*s1p8 − 14*c3β*s1p6*c1p2 + 2*η*sβ*s1p8 − 14*η*sβ*s1p6*c1p2 − 2/3*sβ*s1p8 + 14/3*sβ*s1p6*c1p2
		  dAdι(149) = 32*η*cβ*sβ*s1p7*c1 − 32/3*cβ*sβ*s1p7*c1
		  dAdι(150) = − 19/8*η*c3β*s1p4*c2 + 7/16*η*c3β*s1p4*c4 − 9/16*η*c3β*s1p4*− 19/4*η*c3β*s1p3*s2c1 + 7/4*η*c3β*s1p3*s4c1 + 57/8*η*c3β*s1p2*c1p2*c2 − 21/16*η*c3β*s1p2*c1p2*c4 + 27/16*η*c3β*s1p2*c1p2*+ 19/24*c3β*s1p4*c2 − 7/48*c3β*s1p4*c4 + 3/16*c3β*s1p4*+ 19 12*c3β*s1p3*s2c1 − 7/12*c3β*s1p3*s4c1 − 19/8*c3β*s1p2*c1p2*c2 + 7/16*c3β*s1p2*c1p2*c4 − 9/16*c3β*s1p2*c1p2*+ 9/8*η*sβ*s1p4*c2 + 3/16*η*sβ*s1p4*c4 − 103/48*η*sβ*s1p4*+ 9/4*η*sβ*s1p3*s2c1 + 3/4*η*sβ*s1p3*s4c1 − 27/8*η*sβ*s1p2*c1p2*c2 − 9/16*η*sβ*s1p2*c1p2*c4 + 103/16*η*sβ*s1p2*c1p2*− 3/8*sβ*s1p4*c2 − 1/16*sβ*s1p4*c4 + 79/16*sβ*s1p4*− 3/4*sβ*s1p3*s2c1 − 1/4*sβ*s1p3*s4c1 + 9/8*sβ*s1p2*c1p2*c2 + 3/16*sβ*s1p2*c1p2*c4 − 237/16*sβ*s1p2*c 2 1
		  dAdι(151) = 1/2*η*c3β*s1p4*s2 − 7/4*η*c3β*s1p4*s4 − η*c3β*s1p3*c1*c2 + 7/4*η*c3β*s1p3*c1*c4 − 3/4*η*c3β*s1p3*c1 − 1/6*c3β*s1p4*s2 + 7/12*c3β*s1p4*s4 + 1/3*c3β*s1p3*c1*c2 − 7/12*c3β*s1p3*c1*c4 + 1/4*c3β*s1p3*c1 + 7/2*η*cβ*s1p4*s2 − 1/4*η*cβ*s1p4*s4 − 7*η*cβ*s1p3*c1*c2 + 1/4*η*cβ*s1p3*c1*c4 − 119/12*η*cβ*s1p3*c1 − 7/6*cβ*s1p4*s2 + 1/12*cβ*s1p4*s4 + 7/3*cβ*s1p3*c1*c2 − 1/12*cβ*s1p3*c1*c4 + 47/4*cβ*s1p3*c1
		  dAdι(152) = − 3/2*η*c2β*sβ*s1p6*c2 − 3*η*c2β*sβ*s1p5 s2c1 + 15/2*η*c2β*sβ*s1p4*c1p2*c2 + 1/2*c2β*sβ*s1p6*c2 +c2β*sβ*s1p5 s2c1 − 5/2*c2β*sβ*s1p4*c1p2*c2 − 1/2*η*sβ*s1p6*c2 + 2*η*sβ*s1p6 − η*sβ*s1p5*s2*c1 + 5/2*η*sβ*s1p4*c1p2*c2 −10*η*sβ*s1p4*c1p2 + 1/6*sβ*s1p6*c2 − 2/3*sβ*s1p6 + 1/3*sβ*s1p5 s2c1 − 5/6*sβ*s1p4*c1p2*c2 + 10/3*sβ*s1p4*c1p2
		  dAdι(153) = −2*η*cβ*sβ2*s1p7*c1 + 6*η*cβ*sβ2*s1p5*c1p3 + 2/3*cβ*sβ2*s1p7*c1 − 2*cβ*sβ2*s1p5*c1p3
		  dAdι(154) = − 15/2*η*cβ*sβ2*s2p3 + 15*η*cβ*sβ2*s2*c22 + 5/2*cβ*sβ2*s2p3 − 5*cβ*sβ2*s2*c22
		  dAdι(155) = − 57/8*η*c3β*s1p2*c1p2*c2 − 21/16*η*c3β*s1p2*c1p2*c4 + 27/16*η*c3β*s1p2*c1p2 − 19/4*η*c3βs1s2*c1p3 − 7/4*η*c3βs1s4*c1p3 + 19/8*η*c3β*c1p4*c2 + 7/16*η*c3β*c1p4*c4 − 9/16*η*c3β*c1p4 + 19/8*c3β*s1p2*c1p2*c2 + 7/16*c3β*s1p2*c1p2*c4 − 9/16*c3β*s1p2*c1p2 + 19/12*c3βs1s2*c1p3 + 7/12*c3βs1s4*c1p3 − 19/24*c3β*c1p4*c2 − 7/48*c3β*c1p4*c4 + 3/16*c3β*c1p4 + 27/8*η*sβ*s1p2*c1p2*c2 − 9/16*η*sβ*s1p2*c1p2*c4 + 103/16*η*sβ*s1p2*c1p2 + 9/4*η*sβs1s2*c1p3 − 3 4*η*sβs1s4*c1p3 − 9/8*η*sβ*c1p4*c2 + 3/16*η*sβ*c1p4*c4 − 103/48*η*sβ*c1p4 − 9/8*sβ*s1p2*c1p2*c2 + 3/16*sβ*s1p2*c1p2*c4 − 237/16*sβ*s1p2*c1p2 − 3/4*sβs1s2*c1p3 + 1/4*sβs1s4*c1p3 + 3/8*sβ*c1p4*c2 − 1/16*sβ*c1p4*c4 + 79/16*sβ*c1p4
		  dAdι(156) = − η*c3β*s1*c1p3*c2 − 7/4*η*c3β*s1*c1p3*c4 + 3/4*η*c3β*s1*c1p3 − 1/2*η*c3β*s2*c1p4 − 7/4*η*c3β*s4*c1p4 + 1/3*c3β*s1*c1p3*c2 + 7/12*c3β*s1*c1p3*c4 − 1/4*c3β*s1*c1p3 + 1/6*c3β*s2*c1p4 + 7/12*c3β*s4*c1p4 − 7*η*cβ*s1*c1p3*c2 − 1 4*η*cβ*s1*c1p3*c4 + 119/12*η*cβ*s1*c1p3 − 7/2*η*cβ*s2*c1p4 − 1/4*η*cβ*s4*c1p4 + 7/3*cβ*s1*c1p3*c2 + 1/12*cβ*s1*c1p3*c4 − 47/4*cβ*s1*c1p3 + 7/6*cβ*s2*c1p4 + 1/12*cβ*s4*c1p4
		  dAdι(157) = − 15/2*η*c2β*sβ*s1p2*c1p4*c2 − 3*η*c2β*sβ*s1*s2*c1p5+ 3/2*η*c2β*sβ*c1p6*c2 + 5/2 c2β*sβ*s1p2*c1p4*c2 +c2β*sβ*s1*s2*c1p5− 1/2 c2β*sβ*c1p6*c2 − 5/2*η*sβ*s1p2*c1p4*c2 − 10*η*sβ*s1p2*c1p4− η*sβ*s1*s2*c1p5+ 1/2*η*sβ*c1p6*c2 +2*η*sβ*c1p6+ 5/6*sβ*s1p2*c1p4*c2 + 10/3*sβ*s1p2*c1p4+ 1/3*sβ*s1*s2*c1p5− 1/6*sβ*c1p6*c2 − 2/3*sβ*c1p6
		  dAdι(158) = −6*η*cβ*sβ2*s1p3*c1p5 + 2*η*cβ*sβ2*s1*c1p7 + 2*cβ*sβ2*s1p3*c1p5 − 2/3*cβ*sβ2*s1*c1p7
		  dAdι(159) = − 70*η*c3β*s1p4*c1p4 + 42*η*c3β*s1p2*c1p6 + 70/3*c3β*s1p4*c1p4 − 14*c3β*s1p2*c1p6 − 30*η*sβ*s1p4*c1p4 + 18*η*sβ*s1p2*c1p6 +10*sβ*s1p4*c1p4 − 6*sβ*s1p2*c1p6
		  dAdι(160) = − 84*η*c3β*s1p3*c1p5 + 28*η*c3β*s1*c1p7 + 28*c3β*s1p3*c1p5 − 28/3*c3βs1*c1p7 − 12*η*cβ*s1p3*c1p5 + 4*η*cβ*s1*c1p7 +4*cβ*s1p3*c1p5 − 4/3*cβ*s1*c1p7
		  dAdι(161) = 84*η*c2β*sβ*s1p2*c1p6 − 12*η*c2β*sβ*c1p8 − 28*c2β*sβ*s1p2*c1p6 + 4*c2β*sβ*c1p8 + 28*η*sβ*s1p2*c1p6 − 4*η*sβ*c1p8 − 28/3*sβ*s1p2*c1p6 + 4/3*sβ*c1p8
		  dAdι(162) = − 32*η*cβ*sβ2*s1*c1p7 + 32/3*cβ*sβ2*s1*c1p7
		  dAdι(163) = − 1/2*s2*χayDN
		  dAdι(164) = s1*c1*χayDN
		  dAdι(165) =  1/2*δ*cβ2*s2**χaxDN - 1/2*δ*cβ*sβ*s2*χazDN
		  dAdι(166) = − sβ2*c2*χazDN + cβ*sβ*c2*χaxDN
		  dAdι(167) = - 1/2*δ*cβ2*s2**χaxDN + 1/2*δ*cβ*sβ*s2*χazDN
		  dAdι(168) = − 1/2*δ*s2*χsyDN
		  dAdι(169) = δ*s1*c1*χsyDN
		  dAdι(170) = 1/2*δ*cβ2*s2**χsxDN - 1/2*δ*cβ*sβ*s2*χszDN
		  dAdι(171) = − δ*sβ2*c2*χszDN + δ*cβ*sβ*c2*χsxDN
		  dAdι(172) =  − 1/2*δ*cβ2*s2**χsxDN + 1/2*δ*cβ*sβ*s2*χszDN
		  dAdι(173) =  − 4*π*sβ*s1p4 + 12*π*sβ*s1p2*c2
		  dAdι(174) = -8*π*cβ*s1p3*c1
		  dAdι(175) = 12*π*sβ* s1p2*c1p2 − 4*π*sβ*c1p4
		  dAdι(176) = 8*π*cβ*s1*c1p3
		  dAdι(177) = − 4375/96*δ*η*s2β*s1p7*c1p3+ 4375/64*δ*η*s2β*s1p5 *c1p5− 4375/64*δ*η*s4β*s1p7*c1p3+ 13125/128*δ*η*s4β*s1p5 *c1p5+ 4375/192*δ*s2β*s1p7*c1p3− 4375/128*δ*s2β*s1p5 *c1p5+ 4375/128*δ*s4β*s1p7*c1p3− 13125/256*δ*s4β*s1p5 *c1p5
		  dAdι(178) = 625/32*δ*η*c2β*s1p8 *c1p2 − 4375/96*δ*η*c2β*s1p6 *c1p4 − 625/64*δ*c2β*s1p8 *c1p2 + 4375/192*δ*c2β*s1p6 *c1p4 + 1875/32*δ*η*c4β*s1p8 *c1p2 − 4375/32*δ*η*c4β*s1p6 *c1p4 − 1875/64*δ*c4β*s1p8 *c1p2 + 4375/64*δ*c4β*s1p6 *c1p4
		  dAdι(179) = − 625/128*δ*η*s2β*s1p9*c1 + 625/32*δ*η*s2β*s1p7*c1p3 + 5625/256*δ*η*s4β*s1p9*c1 − 5625/64*δ*η*s4β*s1p7*c1p3 + 625/256*δ*s2β*s1p9*c1 − 625/64*δ*s2β*s1p7*c1p3 − 5625/512*δ*s4β*s1p9*c1 + 5625/128*δ*s4β*s1p7*c1p3
		  dAdι(180) = 625/48*δ*η*c2β*sβ2*s101 − 1875 16*δ*η*c2β*sβ2*s1p8*c1p2 − 625/96*δ*c2β*sβ2*s101 + 1875/32*δ*c2β*sβ2*s1p8*c1p2 + 625/96*δ*η*sβ2*s101 − 1875/32*δ*η*sβ2*s1p8*c1p2 − 625 192*δ*sβ2*s101 + 1875/64*δ*sβ2*s1p8*c1p2
		  dAdι(181) = 3125/96*δ*η*cβ*sβ^3*s1p9*c1 + 3125/192*δ*cβ*sβ^3*s1p9*c1
		  dAdι(182) = − 459/128*δ*η*s2β*s1p5*c1*c2 − 567/512*δ*η*s2β*s1p5*c1*c4 + 4923/512*δ*η*s2β*s1p5*c1 − 459/128*δ*η*s2β*s1p4*s2*c1p2− 567/256*δ*η*s2β*s1p4*s4*c1p2+ 459/64*δ*η*s2β*s1p3*c1p3*c2 + 567/256*δ*η*s2β*s1p3*c1p3*c4 − 4923/256*δ*η*s2β*s1p3*c1p3 + 2079/256*δ*η*s4β*s1p5*c1*c2 − 1701/1024*δ*η*s4β*s1p5*c1*c4 + 945/1024*δ*η*s4β*s1p5*c1 + 2079/256*δ*η*s4β*s1p4*s2*c1p2− 1701/512*δ*η*s4β*s1p4*s4*c1p2− 2079/128*δ*η*s4β*s1p3*c1p3*c2 + 1701/512*δ*η*s4β*s1p3*c1p3*c4 − 945/512*δ*η*s4β*s1p3*c1p3*+ 459/256*δ*s2β*s1p5*c1*c2 + 567/1024*δ*s2β*s1p5*c1*c4 − 22203/1024*δ*s2β*s1p5*c1 + 459/256*δ*s2β*s1p4*s2*c1p2+ 567/512*δ*s2β*s1p4*s4*c1p2− 459/128*δ*s2β*s1p3*c1p3*c2 − 567/512*δ*s2β*s1p3*c1p3*c4 + 22203/512*δ*s2β*s1p3*c1p3 − 2079/512*δ*s4β*s1p5*c1*c2 + 1701/2048*δ*s4β*s1p5*c1*c4 − 945/2048*δ*s4β*s1p5*c1 − 2079/512*δ*s4β*s1p4*s2*c1p2+ 1701/1024*δ*s4β*s1p4*s4*c1p2+ 2079/256*δ*s4β*s1p3*c1p3*c2 − 1701/1024*δ*s4β*s1p3*c1p3*c4 + 945/1024*δ*s4β*s1p3*c1p3
		  dAdι(183) = − 27/32*δ*η*c2β*s1p6*c2 + 81/256*δ*η*c2β*s1p6*c4 − 1233/256*δ*η*c2β*s1p6 − 27/16*δ*η*c2β*s1p6*s2*c1 + 81/64*δ*η*c2β*s1p6*s4*c1 + 135/32*δ*η*c2β*s1p4*c1p2*c2 − 405/256*δ*η*c2β*s1p4*c1p2*c4 + 6165/256*δ*η*c2β*s1p4*c1p2*+ 27/64*δ*c2β*s1p6*c2 − 81/512*δ*c2β*s1p6*c4 + 4689/512*δ*c2β*s1p6*+ 27/32*δ*c2β*s1p6*s2*c1 − 81/128*δ*c2β*s1p6*s4*c1 − 135/64*δ*c2β*s1p4*c1p2*c2 + 405/512*δ*c2β*s1p4*c1p2*c4 − 23445/512*δ*c2β*s1p4*c1p2*+ 243/256*δ*η*c4β*s1p6*c4 − 27/256*δ*η*c4β*s1p6 + 243/64*δ*η*c4β*s1p6*s4*c1 − 1215/256*δ*η*c4β*s1p4*c1p2*c4 + 135/256*δ*η*c4β*s1p4*c1p2*+ 27/64*δ*c4β*s1p6*c2 − 243/512*δ*c4β*s1p6*c4 + 27/512*δ*c4β*s1p6*+ 27/32*δ*c4β*s1p6*s2*c1 − 243/128*δ*c4β*s1p6*s4*c1 − 135/64*δ*c4β*s1p4*c1p2*c2 + 1215/512*δ*c4β*s1p4*c1p2*c4 − 135/512*δ*c4β*s1p4*c1p2 − 27/16*δ*η*s1p6*c2 − 27/32*δ*η*s6 1 − 27/8*δ*η*s5 1*s2*c1 + 135/16*δ*η*s1p4*c1p2*c2 + 135/32*δ*η*s1p4*c1p2*− 27/32*δ*η*c4β*s1p6*c2 − 27/16*δ*η*c4β*s1p6*s2*c1 + 135/32*δ*η*c4β*s1p4*c1p2*c2 + 27/32*δ*s1p6*c2 + 27 64*δ*s1p6 + 27/16*δ*s1p5*s2*c1 − 135/32*δ*s1p4*c1p2*c2 − 135/64*δ*s1p4*c1p2
		  dAdι(184) = 621/256*δ*η*c2βs2β*s1p6*s2 + 2187/512*δ*η*c2βs2β*s1p6*s4 − 1863/256*δ*η*c2βs2β*s1p5*c1c2 − 6561/1024*δ*η*c2βs2β*s1p5*c1c4 − 4131/1024*δ*η*c2βs2β*s1p5*c1 − 621/512*δ*c2βs2β*s1p6*s2 − 2187/1024*δ*c2βs2β*s1p6*s4 + 1863/512*δ*c2βs2β*s1p5*c1c2 + 6561/2048*δ*c2βs2β*s1p5*c1c4 + 4131/2048*δ*c2βs2β*s1p5*c1 − 837/256*δ*η*s2β*s1p6*s2 − 243/512*δ*η*s2β*s1p6*s4 + 2511/256*δ*η*s2β*s1p5*c1c2 + 729/1024*δ*η*s2β*s1p5*c1c4 + 14283/1024*δ*η*s2β*s1p5*c1 + 837/512*δ*s2β*s1p6*s2 + 243/1024*δ*s2β*s1p6*s4 − 2511/512*δ*s2β*s1p5*c1c2 − 729/2048*δ*s2β*s1p5*c1c4 − 35019/2048*δ*s2β*s1p5*c1
		  dAdι(185) = 81/32*δ*η*c2β*sβ2*s1p8*c2 + 27/32*δ*η*c2β*sβ2*s1p8+ 81/16*δ*η*c2β*sβ2*s1p7*s2*c1 − 567/32*δ*η*c2β*sβ2*s1p6*c1p2 c2 − 189/32*δ*η*c2β*sβ2*s1p6*c1p2 − 81/64*δ*c2β*sβ2*s1p8*c2 − 27/64*δ*c2β**sβ2*s1p8− 81/32*δ*c2β*sβ2*s1p7*s2*c1 + 567/64*δ*c2β*sβ2*s1p6*c1p2*c2 + 189/64*δ*c2β*sβ2*s1p6*c1p2 + 81/64*δ*η*sβ2*s1p8*c2 − 81/64*δ*η*sβ2*s1p8 + 81/32*δ*η*sβ2*s1p7*c2*s2*c1 − 567/64*δ*η*sβ2*s1p6*c1p2*c2 + 567/64*δ*η*s*sβ2*s1p6*c1p2 − 81/128*δ**sβ2*s1p8*c2 + 81/128*δ*sβ2*s1p8 − 81/64*δ*sβ2*s1p7*c2*s2*c1 + 567/128*δ*sβ2*s1p6*c1p2*c2 − 567/128*δ*s*sβ2*s1p6*c1p2
		  dAdι(186) = 81/32*δ*η*cβ*sβ^3*s1p9*c1 − 81/8*δ*η*cβ*sβ^3*s1p7*c1p3 − 81/64*δ*cβ*sβ^3*s1p9*c1 + 81/16*δ*cβ*sβ^3*s1p7*c1p3
		  dAdι(187) = − 11/3072*δ*η*c2β*s2β*s4 + 273/16384*δ*η*c2β*s2β*s6 + 11/6144*δ*c2β*s2β*s4 − 273/32768*δ*c2β*s2β*s6 + 173/24576*δ*η*c3β*cβ*s2 − 173/49152*δ*c3β*cβ*s2 − 6031/24576*δ*η*cβ*sβ*s2 + 10511/49152*δ*cβ*sβ*s2 + 679/3072*δ*η*s2β*s4 + 603/16384*δ*η*s2β*s6 + 35/49152*δ*η*s2β*s10 + 35/32768*δ*η*s4β*s10 − 557/2048*δ*s2β*s4 + 5157/32768*δ*s2β*s6 − 35/98304*δ*s2β*s10 − 35/65536*δ*s4β*s10
		  dAdι(188) = 37/512*δ*η*c2β*s1p4*c2 − 3/256*δ*η*c2β*s1p4*c4 + 1/1536*δ*η*c2β*s1p4*c6 + 35/256*δ*η*c2β*s1p4 + 37/256*δ*η*c2β*s1p3*s2c1 − 3/64*δ*η*c2β*s1p3*s4*c1 + 1/256*δ*η*c2β*s1p3*s6c1 − 111/512*δ*η*c2β*s1p2*c1p2*c2 + 9 256*δ*η*c2β*s1p2*c1p2*c4 − 1/512*δ*η*c2β*s1p2*c1p2*c6 − 105 256*δ*η*c2β*s1p2*c1p2*+ 347 1024*δ*c2β*s1p4*c2 + 3/512*δ*c2β*s1p4*c4 − 1/3072*δ*c2β*s1p4*c6 + 23/1536*δ*c2β*s1p4 + 347/512*δ*c2β*s1p3*s2c1 + 3/128*δ*c2β*s1p3*s4*c1 − 1/512*δ*c2β*s1p3*s6c1 − 1041/1024*δ*c2β*s1p2*c1p2*c2 − 9/512*δ*c2β*s1p2*c1p2*c4 + 1/1024*δ*c2β*s1p2*c1p2*c6 − 23/512*δ*c2β*s1p2*c1p2 + 57/1024*δ*η*c3c4β*s1p2*c1 − 57/2048*δ*c3c4β*s1p2*c1 − 79/3072*δ*η*c4β*s1p4 − 45 1024*δ*η*c4β*s1p3*s5 + 7/1024*δ*η*c4β*s1p3*s7 + 79/1024*δ*η*c4β*s1p2*c1p2 + 27/1024*δ*η*c4β*s1p2*c1c5 − 3/1024*δ*η*c4β*s1p2*c1c7 + 79/6144*δ*c4β*s1p4 + 45/2048*δ*c4β*s1p3*s5 − 7/2048*δ*c4β*s1p3*s7 − 79/2048*δ*c4β*s1p2*c1p2 − 27/2048*δ*c4β*s1p2*c1c5 + 3/2048*δ*c4β*s1p2*c1c7 − 1/64*δ*η*s1p4*c2 − 1/64*δ*η*s1p4*c4 + 11/32*δ*η*s1p4 − 1/32*δ*η*s1p3*s2c1 − 1/16*δ*η*s3 1 s4*c1 + 3 64*δ*η*s1p2*c1p2*c2 + 3/64*δ*η*s1p2*c1p2*c4 − 33/32*δ*η*s1p2*c1p2 + 1/128*δ*s1p4*c2 + 1/128*δ*s1p4*c4 − 19/64*δ*s1p4 + 1/64*δ*s1p3*s2c1 + 1/32*δ*s1p3*s4*c1 − 3/128*δ*s1p2*c1p2*c2 − 3/128*δ*s1p2*c1p2*c4 + 57/64*δ*s1p2*c1p2
		  dAdι(189) = − 13/128*δ*η*s2β*s1p5*c1c2 − 1/512*δ*η*s2β*s1p5*c1c4 + 29/512*δ*η*s2β*s1p5*c1 − 13/128*δ*η*s2β*s1p4*s2*c1p2 − 1/256*δ*η*s2β*s1p4*s4*c1p2 + 13/64*δ*η*s2β*s1p5*c1p3*c2 + 1/256*δ*η*s2β*s1p5*c1p3*c4 − 29/256*δ*η*s2β*s1p5*c1p3*− 11/256*δ*η*s4β*s1p5*c1c2 + 9/1024*δ*η*s4β*s1p5*c1c4 − 5/1024*δ*η*s4β*s1p5*c1 − 11/256*δ*η*s4β*s1p4*s2*c1p2 + 9/512*δ*η*s4β*s1p4*s4*c1p2 + 11/128*δ*η*s4β*s1p5*c1p3*c2 − 9/512*δ*η*s4β*s1p5*c1p3*c4 + 5/512*δ*η*s4β*s1p5*c1p3*+ 13/256*δ*s2β*s1p5*c1c2 + 1/1024*δ*s2β*s1p5*c1c4 + 355/1024*δ*s2β*s1p5*c1 + 13/256*δ*s2β*s1p4*s2*c1p2 + 1/512*δ*s2β*s1p4*s4*c1p2 − 13/128*δ*s2β*s1p5*c1p3*c2 − 1/512*δ*s2β*s1p5*c1p3*c4 − 355/512*δ*s2β*s1p5*c1p3*+ 11/512*δ*s4β*s1p5*c1c2 − 9/2048*δ*s4β*s1p5*c1c4 + 5/2048*δ*s4β*s1p5*c1 + 11/512*δ*s4β*s1p4*s2*c1p2 − 9/1024*δ*s4β*s1p4*s4*c1p2 − 11/256*δ*s4β*s1p5*c1p3*c2 + 9/1024*δ*s4β*s1p5*c1p3*c4 − 5/1024*δ*s4β*s1p5*c1p3
		  dAdι(190) = 1/16*δ*η*c2β*sβ2*s1p6*c1p2*c2 − 1/16*δ*η*c2β*sβ2*s1p6*c1p2 + 1/24*δ*η*c2β*sβ2*s1p5*s2*c1p3 − 5/48*δ*η*c2β*sβ2*s1p4*c1p4*c2 + 5/48*δ*η*c2β*sβ2*s1p4*c1p4 − 1/32*δ*c2β*sβ2*s1p6*c1p2*c2 + 1/32*δ*c2β*sβ2*s1p6*c1p2 − 1/48*δ*c2β*sβ2*s1p5*s2*c1p3 + 5/96*δ*c2β*sβ2*s1p4*c1p4*c2 − 5/96*δ*c2β*sβ2*s1p4*c1p4 + 1/32*δ*η*sβ2*s1p6*c1p2*c2 − 7/32*δ*η*sβ2*s1p6*c1p2 + 1/48*δ*η*sβ2*s1p5*s2*c1p3 − 5/96*δ*η*sβ2*s1p4*c1p4*c2 + 35/96*δ*η*sβ2*s1p4*c1p4 − 1/64*δ*sβ2*s1p6*c1p2*c2 + 7/64*δ*sβ2*s1p6*c1p2 − 1/96*δ*sβ2*s1p5*s2*c1p3 + 5 192*δ*sβ2*s1p4*c1p4*c2 − 35/192*δ*sβ2*s1p4*c1p4
		  dAdι(191) = 1/24*δ*η*cβ*sβ^3*s1p7*c1p3 − 1/16*δ*η*cβ*sβ^3*s1p5*c1p5 − 1/48*δ*cβ*sβ^3*s1p7*c1p3 + 1/32*δ*cβ*sβ^3*s1p5*c1p5
		  dAdι(192) = − 7/64*δ*η*c2β*sβ2*s8 − 1/64*δ*η*c2β*sβ2*c4 − 7/128*δ*c2β*sβ2*s8 + 1/128*δ*cβ*sβ2*c4 + 5/64*δ*η*sβ2*s8 + 45/64*δ*η*sβ2*c4 − 5/128*δ*sβ2*s8 − 77/128*δ*sβ2*c4
		  dAdι(193) = 189/32*δ*η*c2β*sβ2*s2p4 − 567/32*δ*η*c2β*sβ2*s22*c22 − 189/64*δ*c2β*sβ2*s2p4 + 567/64*δ*c2β*sβ2*s22*c22 + 135/32*δ*η**sβ2*s2p4 − 405/32*δ*η*sβ2*s22*c22 − 135/64*δ*sβ2*s2p4 + 405/64*δ*sβ2*s22*c22
		  dAdι(194) = 11/3072*δ*η*c2β*s2β*s4 + 273/16384*δ*η*c2β*s2β*s6 − 11/6144*δ*c2β*s2β*s4 − 273/32768*δ*c2β*s2β*s6 + 173/24576*δ*η*c3β*cβ*s2 − 173/49152*δ*c3β*cβ*s2 − 6031/24576*δ*η*cβ*sβ*s2 + 10511/49152*δ*cβ*sβ*s2 − 679/3072*δ*η*s2β*s4 + 603/16384*δ*η*s2β*s6 + 35/49152*δ*η*s2β*s10 + 35/32768*δ*η*s4β*s10 + 557/2048*δ*s2β*s4 + 5157/32768*δ*s2β*s6 − 35/98304*δ*s2β*s10 − 35/65536*δ*s4β*s10
		  dAdι(195) = − 111/512*δ*η*c2β*s1p2*c1p2*c2 − 9/256*δ*η*c2β*s1p2*c1p2*c4 − 1/512*δ*η*c2β*s1p2*c1p2*c6 + 105/256*δ*η*c2β*s1p2*c1p2 − 37/256*δ*η*c2β*s1*s2*c1p3 − 3/64*δ*η*c2β*s1*s4*c1p3 − 1/256*δ*η*c2β*s1*s6*c1p3 + 37/512*δ*η*c2β*c1p4*c2 + 3/256*δ*η*c2β*c1p4*c4 + 1/1536*δ*η*c2β*c1p4*c6 − 35/256*δ*η*c2β*c1p4*− 1041/1024*δ*c2β*s1p2*c1p2*c2 + 9/512*δ*c2β*s1p2*c1p2*c4 + 1/1024*δ*c2β*s1p2*c1p2*c6 + 23/512*δ*c2β*s1p2*c1p2*− 347/512*δ*c2β*s1*s2*c1p3+ 3/128*δ*c2β*s1*s4*c1p3+ 1/512*δ*c2β*s1*s6*c1p3+ 347/1024*δ*c2β*c1p4*c2 − 3/512*δ*c2β*c1p4*c4 − 1/3072*δ*c2β*c1p4*c6 − 23/1536*δ*c2β*c1p4− 79/1024*δ*η*c4β*s1p2*c1p2− 27/1024*δ*η*c4β*s1*s5*c1p2 − 3/1024*δ*η*c4β*s1*s7*c1p2 + 57/1024*δ*η*c4β*s1*c1p2*c3 + 57/1024*δ*η*c4β*s3*c1p3 + 79/3072*δ*η*c4β*c1p4+ 45/1024*δ*η*c4β*c1p3*c5 + 7/1024*δ*η*c4β*c1p3*c7 + 79/2048*δ*c4β*s1p2*c1p2+ 27/2048*δ*c4β*s1*s5*c1p2 + 3/2048*δ*c4β*s1*s7*c1p2 − 57/2048*δ*c4β*s1*c1p2*c3 − 57/2048*δ*c4β*s3*c1p3− 79/6144*δ*c4β*c1p4− 45/2048*δ*c4β*c1p3*c5 − 7/2048*δ*c4β*c1p3*c7 + 3/64*δ*η*s1p2*c1p2*c2 − 3/64*δ*η*s1p2*c1p2*c4 + 33/32*δ*η*s1p2*c1p2*+ 1/32*δ*η*s1*s2*c1p3− 1/16*δ*η*s1*s4*c1p3− 1/64*δ*η*c1p4*c2 + 1/64*δ*η*c1p4*c4 − 11/32*δ*η*c1p4− 3/128*δ*s1p2*c1p2*c2 + 3/128*δ*s1p2*c1p2*c4 − 57/64*δ*s1p2*c1p2− 1/64*δ*s1*s2*c1p3+ 1/32*δ*s1*s4*c1p3+ 1/128*δ*c1p4*c2 − 1/128*δ*c1p4*c4 + 19/64*δ*c1p4
		  dAdι(196) = 459/128*δ*η*s2β*s1p3*c1p3*c2 − 567/512*δ*η*s2β*s1p3*c1p3*c4 − 4923/256*δ*η*s2β*s1p3*c1p3 + 459/256*δ*η*s2β*s1p2*s2*c1p4 − 567/512*δ*η*s2β*s1p2*s4*c1p4 − 459/256*δ*η*s2β*s1*c1p5*c2 + 567/1024*δ*η*s2β*s1*c1p5*c4 + 4923/512*δ*η*s2β*s1*c1p5 − 2079/256*δ*η*s4β*s1p3*c1p3*c2 − 1701/1024*δ*η*s4β*s1p3*c1p3*c4 + 945/1024*δ*η*s4β*s1p3*c1p3 − 2079/512*δ*η*s4β*s1p2*s2*c1p4 − 1701/1024*δ*η*s4β*s1p2*s4*c1p4 + 2079/512*δ*η*s4β*s1*c1p5*c2 + 1701/2048*δ*η*s4β*s1*c1p5*c4 − 945/2048*δ*η*s4β*s1*c1p5
		  dAdι(197) = 135/32*δ*η*c2β*c1p2*c1p5*c2 + 405/256*δ*η*c2β*c1p2*c1p5*c4 − 6165/256*δ*η*c2β*c1p2*c1p5*+ 27/16*δ*η*c2β*s1*s2*c1p5 + 81/64*δ*η*c2β*s1*s4*c1p5 − 27/32*δ*η*c2β*c1p6*c2 − 81/256*δ*η*c2β*c1p6*c4 + 1233/256*δ*η*c2β*c1p6*− 135/64*δ*c2β*c1p2*c1p5*c2 − 405/512*δ*c2β*c1p2*c1p5*c4 + 23445/512*δ*c2β*c1p2*c1p5*− 27/32*δ*c2β*s1*s2*c1p5 − 81/128*δ*c2β*s1*s4*c1p5 + 27/64*δ*c2β*c1p6*c2 + 81/512*δ*c2β*c1p6*c4 − 4689/512*δ*c2β*c1p6*+ 135/32*δ*η*c4β*c1p2*c1p5*c2 + 1215/256*δ*η*c4β*c1p2*c1p5*c4 − 135/256*δ*η*c4β*c1p2*c1p5*+ 27/16*δ*η*c4β*s1*s2*c1p5 + 243/64*δ*η*c4β*s1*s4*c1p5 − 27/32*δ*η*c4β*c1p6*c2 − 243/256*δ*η*c4β*c1p6*c4 + 27/256*δ*η*c4β*c1p6*− 135/64*δ*c4β*c1p2*c1p5*c2 − 1215/512*δ*c4β*c1p2*c1p5*c4 + 135/512*δ*c4β*c1p2*c1p5*− 27/32*δ*c4β*s1*s2*c1p5 − 243/128*δ*c4β*s1*s4*c1p5 + 27/64*δ*c4β*c1p6*c2 + 243/512*δ*c4β*c1p6*c4 − 27/512*δ*c4β*c1p6*+ 135 16*δ*η*s1p2*c1p5*c2 − 135/32*δ*η*s1p2*c1p5*+ 27/8*δ*η*s1*s2*c1p5 − 27/16*δ*η*c1p6*c2 + 27/32*δ*η*c1p6 − 135/32*δ*s1p2*c1p5*c2 + 135/64*δ*s1p2*c1p5*− 27/16*δ*s1*s2*c1p5 + 27 32*δ*c1p6*c2 − 27/64*δ*c1p6
		  dAdι(198) = − 2511/256*δ*η*s2β*s1*c1p5*c2 + 729/1024*δ*η*s2β*s1*c1p5*c4 + 14283/1024*δ*η*s2β*s1*c1p5*− 837/256*δ*η*s2β*s2*c1p6 + 243/512*δ*η*s2β*s4*c1p6 + 1863/512*δ*η*s4β*s1*c1p5*c2 − 6561/2048*δ*η*s4β*s1*c1p5*c4 − 4131/2048*δ*η*s4β*s1*c1p5*+ 621/512*δ*η*s4β*s2*c1p6 − 2187/1024*δ*η*s4β*s4*c1p6 + 2511/512*δ*s2β*s1*c1p5*c2 − 729/2048*δ*s2β*s1*c1p5*c4 − 35019/2048*δ*s2β*s1*c1p5*+ 837/512*δ*s2β*s2*c1p6 − 243/1024*δ*s2β*s4*c1p6 − 1863/1024*δ*s4β*s1*c1p5*c2 + 6561/4096*δ*s4β*s1*c1p5*c4 + 4131/4096*δ*s4β*s1*c1p5*− 621/1024*δ*s4β*s2*c1p6 + 2187/2048*δ*s4β*s4*c1p6
		  dAdι(199) = − 567/32*δ*η*c2β*sβ2* s1p2*c1p6*c2 + 189/32*δ*η*c2β*sβ2* s1p2*c1p6*− 81/16*δ*η*c2β*sβ2* s1s2*c1p7 + 81/32*δ*η*c2β*sβ2*c1p8*c2 − 27/32*δ*η*c2β*sβ2*c1p8 + 567/64*δ*c2β*sβ2* s1p2*c1p6*c2 − 189/64*δ*c2β*sβ2* s1p2*c1p6*+ 81 32*δ*c2β*sβ2* s1s2*c1p7 − 81/64*δ*c2β*sβ2*c1p8*c2 + 27/64*δ*c2β*sβ2*c1p8 − 567/64*δ*η*sβ2* s1p2*c1p6*c2 − 567/64*δ*η*sβ2* s1p2*c1p6*− 81/32*δ*η*sβ2* s1s2*c1p7 + 81/64*δ*η*sβ2*c1p8*c2 + 81/64*δ*η*sβ2*c1p8 + 567/128*δ*sβ2* s1p2*c1p6*c2 + 567/128*δ*sβ2* s1p2*c1p6*+ 81/64*δ*sβ2* s1s2*c1p7 − 81/128*δ*sβ2*c1p8*c2 − 81/128*δ*sβ2*c1p8
		  dAdι(200) = − 81/8*δ*η*cβ*sβ^3*s1p3*c1p7 + 81/32*δ*η*cβ*sβ^3*s1*c1p9 + 81/16*δ*cβ*sβ^3*s1p3*c1p7 − 81/64*δ*cβ*sβ^3*s1*c1p9
		  dAdι(201) = 4375/64*δ*η*s2β*s1p5*c1p5 − 4375/96*δ*η*s2β*s1p3*c1p7 + 13125/128*δ*η*s4β*s1p5*c1p5 − 4375/64*δ*η*s4β*s1p3*c1p7 − 4375/128*δ*s2β*s1p5*c1p5 + 4375/192*δ*s2β*s1p3*c1p7 − 13125/256*δ*s4β*s1p5*c1p5 + 4375/128*δ*s4β*s1p3*c1p7
		  dAdι(202) = 4375/96* δ*η*c2β*s1p4*c1p6 − 625/32* δ*η*c2β*s1p2*c1p8 − 4375/192*δ*c2β*s1p4*c1p6 + 625/64*δ*c2β*s1p2*c1p8 + 4375/32* δ*η*c4β*s1p4*c1p6 − 1875/32* δ*η*c4β*s1p2*c1p8 − 4375/64*δ*c4β*s1p4*c1p6 + 1875/64*δ*c4β*s1p2*c1p8
		  dAdι(203) = 625/32* δ*η*s2β*s1p3*c1p7 − 625/128* δ*η*s2β*s1*c1p9 − 5625/64* δ*η*s4β*s1p3*c1p7 + 5625/256* δ*η**s4β*s1*c1p9 − 625/64*δ*s2β*s1p3*c1p7 + 625/256*δ*s2β*s1*c1p9 + 5625/128*δ*s4β*s1p3*c1p7 − 5625/512*δ*s4β*s1*c1p9
		  dAdι(204) = 1875/16*δ*η*c2β*sβ2*s1p2*c1p8 − 625/48*δ*ηc2β*sβ2*c101 − 1875/32*δ*c2β*sβ2*s1p2*c1p8 + 625/96*δ*c2β*sβ2*c101 + 1875 32 δη*sβ2*s1p2*c1p8  − 625/96*δ*η*sβ2*c101  − 1875/64*δ*sβ2*s1p2*c1p8  + 625/192*δ*sβ2*c101 
		  dAdι(205) = − 3125/96*δ*η*cβ*sβ^3*s1*c1p9 + 3125/192*δ*cβ*sβ^3*s1*c1p9
		  dAdι(206) = 3*η*sβ*s2*c22*χsyDN − 6*sβ*s2*c22*χsyDN
		  dAdι(207) = − 4/3*η*sβ*s1*c1p3*c2*χsyDN + 10/3*η*sβ*s1*c1p3*χsyDN − 2/3*η*sβ*s2*c1p4*χsyDN − 40/3*sβ*s1*c1p3*c2*χsyDN + 28/3*sβ*s1*c1p3*χsyDN − 20/3*sβ*s2*c1p4*χsyDN
		  dAdι(208) = 5/3*η*cβ*s1p2*c1p4*χsyDN − 1/3*η*cβ*c1p6*χsyDN + 50/3*cβ*s1p2*c1p4*χsyDN − 10/3*cβ*c1p6*χsyDN
		  dAdι(209) = − 1/2*η*cβ*s1p2*c1p2*c2*χsyDN − 7/2*η*cβ*s1p2*c1p2*χsyDN − 1/3*η*cβ*s1*s2*c1p3*χsyDN + 1/6*η*cβ*c1p4*c2*χsyDN + 7/6*η*cβ*c1p4*χsyDN − 5*cβ*s1p2*c1p2*c2*χsyDN + cβ*s1p2*c1p2*χsyDN − 10/3*cβ*s1*s2*c1p3*χsyDN + 5/3*cβ*c1p4 c2*χsyDN − 1/3*cβ*c1p4*χsyDN
		  dAdι(210) = 1/6*η*cβ*s1p4*c2*χsyDN − 7/6*η*cβ*s1p4*χsyDN + 1/3*η*cβ*s1p3*s2*c1*χsyDN − 1/2*η*cβ*s1p2*c1p2*c2*χsyDN + 7/2*η*cβ*s1p2*c 2 1*χsyDN + 5/3*cβ*s1p4*c2*χsyDN + 1/3*cβ*s1p4*χsyDN + 10/3 cβ*s1p3* s2*c1*χsyDN − 5cβ*s1p2*c1p2*c2*χsyDN − 1 1 cβ*s1p2*c1p2*χsyDN
		  dAdι(211) = − 2/3*η*sβ*s1p4*s2*χsyDN + 4/3*η*sβ*s1p3*c1*c2*χsyDN + 10/3*η*sβ*s1p3*c1*χsyDN − 20/3*sβ*s1p4*s2*χsyDN + 40/3*sβ*s1p3*c1*c2*χsyDN + 28/3*sβ*s1p3*c1*χsyDN
		  dAdι(212) = 1/3*η*cβ*s1p6*χsyDN − 5/3*η*cβ*s1p4*c1p2*χsyDN + 10/3*cβ*s1p6*χsyDN − 50/3*cβ*s1p4*c1p2*χsyDN
		  dAdι(213) = η*sβ*s2p3*χsyDN − 2*η*sβ*s2*c22*χsyDN − 2*sβ*s2p3*χsyDN + 4*sβ*s2*c22*χsyDN
		  dAdι(214) = − 1/3*η*sβ*s2p3*χsyDN + 2/3*η*sβ*s2*c22*χsyDN − 10/3*sβ*s2p3*χsyDN + 20/3*sβ*s2*c22*χsyDN
		  dAdι(215) = 3/2*η*cβ*s22*c2*χsyDN − 3*cβ*s22*c2*χsyDN
		  dAdι(216) = 5/8*η*cβ*c2χsyDN + 3/8*η*cβ*c6*χsyDN − 5/4*cβ*c2*χsyDN − 3/4*cβ*c6*χsyDN
		  dAdι(217) = − 1/2*η*cβ*s2*s4*χsxDN + 1/4*η*cβ*c2*c4*χsxDN + 3/4*η*cβ*c2*χsxDN + cβ*s2*s4*χsxDN − 1/2*cβ*c2*c4*χsxDN − 3/2*cβ*c2*χsxDN − 2*η*sβ*s2*s4*χszDN + η*sβ*c2*c4*χszDN + 4*cβ*s2*s4*χszDN − 2*sβ*c2*c4*χszDN
		  dAdι(218) = η*cβ*s2p3*χszDN − 2*η*cβ*s2*c22*χszDN − 2*cβ*s2p3*χszDN + 4*cβ*s2*c22*χszDN − ηsβ*s2p3*χsxDN + 2*η*sβ*s2*c22*χsxDN +2*sβ*s2p3*χsxDN − 4sβ*s2*c22*χsxDN
		  dAdι(219) = −6*η*cβ*s22*c2*χsxDN + 12*cβ*s22*c2*χsxD
		  dAdι(220) = −2*η*c3β*s1p4*χsxDN + 6*η*c3β*s1p2 *c1p2*χsxDN + 2*η*c3β*s1p4*χszDN − 6*η*c3β*s1p2 *c1p2*χszDN + 1 6 *η*cβ*s1p4*c2*χsxDN + 5/6 *η*cβ*s1p4*χsxDN + 1/3 *η*cβ*s1p3*s2*c1*χsxDN − 1/2 *η*cβ*s1p2 *c1p2*c2*χsxDN − 5/2 *η*cβ*s1p2 *c1p2*χsxDN + 5/3*cβ*s1p4*c2*χsxDN + 1/3*cβ*s1p4*χsxDN + 10/3*cβ*s1p3*s2*c1*χsxDN − 5cβ*s1p2 *c1p2*c2*χsxDN − cβ*s1p2 *c1p2*χsxDN + 2/3 *η*sβ*s1p4*c2*χszDN +  η*sβ*s1p4*χszDN + 4/3 *η*sβ*s1p3*s2*c1*χszDN − 2*η*sβ*s1p2*c1p2*c2*χszDN − 3*η*sβ*s1p2*c1p2*χszDN + 20/3*sβ*s1p4*c2*χszDN + 2*sβ*s1p4*χszDN + 40/3*sβ*s1p3*s2*c1*χszDN − 20*sβ*s1p2 *c1p2*c2*χszDN − 6sβ*s1p2 *c1p2*χszDN
		  dAdι(221) = 2*η*c3β*s1p3*c1*χszDN + 2*η*c3β*s1p3*c1*χsxDN − 2/3 *η*cβ*s1p4*s2*χszDN + 4/3 *η*cβ*s1p3*c1*c2*χszDN + 10*η*cβ*s1p3*c1*χszDN − 20/3*cβ*s1p4*s2*χszDN + 40/3*cβ*s1p3*c1*c2*χszDN + 8*cβ*s1p3*c1*χszDN + 2/3 *η*sβ*s1p4*s2*χsxDN − 4/3 *η*sβ*s1p3*c1*c2*χsxDN + 20/3 *η*sβ*s1p3*c1*χsxDN + 20/3*sβ*s1p4*s2*χsxDN − 40/3*sβ*s1p3*c1*c2*χsxDN − 28/3*sβ*s1p3*c1*χsxDN
		  dAdι(222) = − 1/3*η*cβ*s1p6*χsxDN + 5/3*η*cβ*s1p4*c1p2*χsxDN − 10/3*cβ*s1p6*χsxDN + 50/3*cβ*s1p4 *c1p2*χsxDN
		  dAdι(223) = 6*η*c2β*sβ*s2*c2*χsxDN − 12*η*cβ*sβ2*s2*c2*χszDN − 7/3*η*sβ*s2*c2*χsxDN + 2/3*sβ*s2*c2*χsxDN
		  dAdι(224) = −6*δ*η*c3β*s1p2*c1p2*χsxDN + 2*δ*η*c3β*c1p4*χsxDN + 6*δ*η*c3β*s1p2*c1p2*χszDN − 2*δ*η*c3β*c1p4*χszDN − 1/2 *δ*η*cβ*s1p2*c1p2* c2*χsxDN + 5/2 *δ*η*cβ*s1p2*c1p2*χsxDN − 1/3 *δ*η*cβ*s1*s2c 3/1*χsxDN + 1/6 *δ*η*cβ*c1p4*c2*χsxDN − 5/6 *δ*η*cβ*c1p4*χsxDN − 5*cβ*s1p2*c1p2* c2*χsxDN +cβ*s1p2*c1p2*χsxDN − 10/3*cβ*s1*s2*c1p3*χsxDN + 5/3*cβ*c1p4*c2*χsxDN − 1/3*cβ*c1p4*χsxDN − 2*δ*η*sβ*s1p2*c1p2* c2*χszDN +3*δ*η*sβ*s1p2*c1p2*χszDN − 4/3 *δ*η*sβ*s1*s2*c1p3*χszDN + 2/3 *δ*η*sβ*c1p4*c2*χszDN − δ*η*sβ*c1p4*χszDN − 20*sβ*s1p2*c1p2* c2*χszDN +6*sβ*s1p2*c1p2*χszDN − 40/3 sβ*s1*s2*c1p3*χszDN + 20/3 sβ*c1p4*c2*χszDN − 2*sβ*c1p4*χszDN
		  dAdι(225) = 2*δ*η*c3β*s1*c1p3*χszDN + 2*δ*η*c3β*s1*c1p3*χsxDN − 4/3 *δ*η*cβc2*s1*c1p3*χszDN + 10*δ*η*cβ*s1*c1p3*χszDN − 2/3 *δ*η*cβ*s2*c1p4*χszDN − 40/3*cβ*c2*s1*c1p3*χszDN + 8*cβ*s1*c1p3*χszDN − 20/3*cβ*s2*c1p4*χszDN + 4/3 *δ*η*sβ*c2*s1*c1p3*χsxDN + 20/3 *δ*η*sβ*s1*c1p3*χsxDN + 2/3 *δ*η*sβ*s2*c1p4*χsxDN + 40/3*sβ*c2s1*c1p3*χsxDN − 28/3*sβ*s1*c1p3*χsxDN + 20/3*sβ*s2*c1p4*χsxDN
		  dAdι(226) =- 5/3*δ*η*cβ*s1p2*c1p4*χsxDN + 1/3*δ*η*cβ*c1p6*χsxDN − 50/3*cβ*s1p2*c1p4*χsxDN + 10/3*cβ*c1p6*χsxDN
		  dAdι(227) = − 6*δ*sβ*s2*c22*χayDN
		  dAdι(228) = − 40/3*δ*sβ*s1*c1p3*c2*χayDN + 28/3*δ*sβ*s1*c1p3*χayDN − 20/3*δ*sβ*s2*c1p4*χayDN
		  dAdι(229) = −5*δ*cβ*s1p2*c1p2*c2*χayDN + δ*cβ*s1p2*c1p2*χayDN − 10/3*δ*cβ*s1*s2*c1p3*χayDN + 5/3*δ*cβ*c1p4*c2*χayDN − 1/3*δ*cβ*c1p4*χayDN
		  dAdι(230) = 50/3*δ*cβ*s1p2*c1p4*χayDN − 10/3*δ*cβ*c1p6*χayDN
		  dAdι(231) = 5/3*δ*cβ*s1p4* c2*χayDN + 1/3*δ*cβ*s1p4*χayDN + 10/3*δ*cβ*s1p3*s2*c1*χayDN − 5*δ*cβ*s1p2*c1p2*c2*χayDN − δ*cβ*s1p2*c1p2*χayDN
		  dAdι(232) = − 20/3 δsβ*s1p4*s2*χayDN + 40/3*δ*sβ*s1p3*c1*c2*χayDN + 28/3*δ*sβ*s1p3*c1*χayDN
		  dAdι(233) = 10/3* δ*cβ*s1p6*χayDN − 50/3* δ*cβ*s1p4*c1p2*χayDN
		  dAdι(234) = −2* δ*sβ*s2p3*χayDN + 4* δ*sβ*s2*c22*χayDN
		  dAdι(235) = − 10/3* δ*sβ*s2p3*χayDN + 20/3* δ*sβ*s2*c22*χayDN
		  dAdι(236) = − 3/2*δ*cβ*s1p2*c1*χayDN
		  dAdι(237) = − 5/4*δ*cβ*c2*χayDN − 3/4*δ*cβ*c6*χayDN
		  dAdι(238) = δ*cβ*s2*s4*χaxDN − 1/2 *δ*cβc2c4*χaxDN − 3/2 *δ*cβ*c2*χaxDN + 4*δ*sβ*s2*s4*χazDN − 2*δ*sβ*c2*c4*χazDN
		  dAdι(239) = −2*δ*cβ*s2p3*χazDN + 4*δ*cβ*s2*c22*χazDN + 2*δ*sβ*s2p3*χaxDN − 4*δ*sβ*s2*c22*χaxDN
		  dAdι(240) = 3*δ*cβ*s22*c2*χaxDN
		  dAdι(241) = 5/3 *δ*cβ*s1p4* c2*χaxDN + 1/3 *δ*cβ*s1p4*χaxDN + 10 3 *δ*cβ*s1p3* s2c1*χaxDN − 5*δ*cβ*s1p2*c1p2* c2*χaxDN −  δ*cβ*s1p2*c1p2*χaxDN + 20/3 *δ*sβ*s1p4* c2*χazDN + 2*δ*sβ*s1p4*χazDN + 40/3 *δ*sβ*s1p3* s2c1*χazDN − 20*δ*sβ*s1p2*c1p2* c2*χazDN − 6*δ*sβ*s1p2*c1p2*χazDN
		  dAdι(242) = − 20/3* δ*cβ*s1p4*s2*χazDN + 40/3* δ*cβ*s1p3*c1c2*χazDN + 8δcβ*s1p3*c1*χazDN + 20/3* δ*sβ*s1p4*s2*χaxDN − 40/3* δ*sβ*s1p3*c1c2*χaxDN − 28/3* δ*sβ*s1p3*c1*χaxDN
		  dAdι(243) = − 10/3*δ*cβ*s1p6*χaxDN + 50/3*δ*cβ*s1p4*c1p2*χaxDN
		  dAdι(244) = 2/3*δ*sβ*s2*c2*χaxDN
		  dAdι(245) = − 5*δ*cβ*s1p2*c1p2 c2*χaxDN + δ*cβ*s1p2*c1p2*χaxDN − 10/3*δ*cβ*s1s2*c1p3*χaxDN + 5/3*δ*cβ*c1p4*c2*χaxDN − 1/3*δ*cβ*c1p4*χaxDN −20*δ*sβ*s1p2*c1p2 c2*χazDN + 6δ*sβ*s1p2*c1p2*χazDN − 40/3*δ*sβs1s2*c1p3*χazDN + 20/3*δ*sβ*c1p4 c2*χazDN − 2δ*sβ*c1p4*χazDN
		  dAdι(246) = − 40/3*δ*cβ*s1*c1p3*c2*χazDN + 8/3*δ*cβ*s1*c1p3*χazDN − 20/3*δ*cβ*s2*c1p4*χazDN + 40/3*δ*sβ*s1*c1p3*c2*χaxDN − 28/3*δ*sβ*s1*c1p3*χaxDN + 20/3*δ*sβ*s2*c1p4*χaxDN 
		  dAdι(247) = 50*δ*cβ*s1p2*c1p4*χaxDN + 10*δ*cβ*c1p6*χaxDN 
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDAdβ()
		  // load the constant parameters
		  Var ιDN As Double  =self.ιDN 
		  Var β As Double  = self.β 
		  Var c2 As Double  = Trig.c2
		  Var s2 As Double = Trig.s2
		  Var c1 As Double = Trig.c1
		  Var s1   As Double = Trig.s1
		  Var c3   As Double = Trig.c3
		  Var s3  As Double  = Trig.s3
		  Var c4   As Double = Trig.c4
		  Var s4  As Double  = Trig.s4
		  Var c5  As Double  = Trig.c5
		  Var s5   As Double = Trig.s5
		  Var c6 As Double   = Trig.c6
		  Var s6   As Double = Trig.s6
		  Var c7   As Double = Trig.c7
		  Var s7 As Double   = Trig.s7
		  Var c8  As Double  = Trig.c8
		  Var s8  As Double  = Trig.s8
		  Var c9  As Double  = Trig.c9
		  Var s9   As Double = Trig.s9
		  Var c10  As Double  = Trig.c10
		  Var s10  As Double  = Trig.s10
		  Var c1p2  As Double  = Trig.c1p2
		  Var c1p3  As Double  = Trig.c1p3   
		  Var c1p4  As Double  =Trig. c1p4   
		  Var c1p5 As Double  = Trig.c1p5   
		  Var c1p6 As Double   = Trig.c1p6   
		  Var c1p7  As Double  = Trig.c1p7 
		  Var c1p8 As Double   = Trig.c1p8  
		  Var c1p9   As Double = Trig.c1p9  
		  Var c1p10  As Double  = Trig.c1p10
		  Var s1p2   As Double = Trig.s1p2   
		  Var s1p3  As Double  = Trig.s1p3   
		  Var s1p4As Double    = Trig.s1p4   
		  Var s1p5   As Double = Trig.s1p5  
		  Var s1p6 As Double   = Trig.s1p6   
		  Var s1p7 As Double   = Trig.s1p7   
		  Var s1p8  As Double  = Trig.s1p8   
		  Var s1p9 As Double   = Trig.s1p
		  Var s1p10 As Double  = Trig.s1p10
		  Var c2p2  As Double  = Trig.c2p2
		  Var c2p3   As Double = Trig.c2p3
		  Var c2p4 As Double  = Trig.c2p4
		  Var s2p2  As Double  = Trig.s2p2
		  Var s2p3  As Double  = Trig.s2p3
		  Var s2p4 As Double   = Trig.s2p4
		  Var s2p5 As Double   = Trig.s2p5
		  
		  // Define local β trig functions
		  Var cβ  As Double  = Trig.cβ
		  Var sβ  As Double  = Trig.sβ
		  Var c2β As Double   = Trig.c2β
		  Var s2β  As Double  = Trig.s2β
		  Var c3β As Double   = Trig.c3β
		  Var s3β  As Double  = Trig.s3β
		  Var c4β As Double   = Trig.c4β
		  Var s4β  As Double  = Trig.s4β
		  Var c5β As Double   = Trig.c5β
		  Var s5β As Double   = Trig.s5β
		  Var cβ2  As Double  = Trig.cβ2
		  Var cβ3 As Double   = Trig.cβ3
		  Var sβ2  As Double  = Trig.sβ2
		  Var sβ3  As Double  = Trig.sβ3
		  
		  // Calulate dAdβ
		  dAdβ(0) = 
		  dAdβ(1) = 
		  dAdβ(2) = 
		  dAdβ(3) = 
		  dAdβ(4) = 
		  dAdβ(5) = 
		  dAdβ(6) = 
		  dAdβ(7) = 
		  dAdβ(8) = 
		  dAdβ(9) = 
		  dAdβ(10) = 
		  dAdβ(11) = 
		  dAdβ(12) = 
		  dAdβ(13) = 
		  dAdβ(14) = 
		  dAdβ(15) = 
		  dAdβ(16) = 
		  dAdβ(17) = 
		  dAdβ(18) = 
		  dAdβ(19) = 
		  dAdβ(20) = 
		  dAdβ(21) = 
		  dAdβ(22) = 
		  dAdβ(23) = 
		  dAdβ(24) = 
		  dAdβ(25) = 
		  dAdβ(26) = 
		  dAdβ(27) = 
		  dAdβ(28) = 
		  dAdβ(29) = 
		  dAdβ(30) = 
		  dAdβ(31) = 
		  dAdβ(32) = 
		  dAdβ(33) = 
		  dAdβ(34) = 
		  dAdβ(35) = 
		  dAdβ(36) = 
		  dAdβ(37) = 
		  dAdβ(38) = 
		  dAdβ(39) = 
		  dAdβ(40) = 
		  dAdβ(41) = 
		  dAdβ(42) = 
		  dAdβ(43) = 
		  dAdβ(44) = 
		  dAdβ(45) = 
		  dAdβ(46) = 
		  dAdβ(47) = 
		  dAdβ(48) = 
		  dAdβ(49) = 
		  dAdβ(50) = 
		  dAdβ(51) = 
		  dAdβ(52) = 
		  dAdβ(53) = 
		  dAdβ(54) = 
		  dAdβ(55) = 
		  dAdβ(56) = 
		  dAdβ(57) = 
		  dAdβ(58) = 
		  dAdβ(59) = 
		  dAdβ(60) = 
		  dAdβ(61) = 
		  dAdβ(62) = 
		  dAdβ(63) = 
		  dAdβ(64) = 
		  dAdβ(65) = 
		  dAdβ(66) = 
		  dAdβ(67) = 
		  dAdβ(68) = 
		  dAdβ(69) = 
		  dAdβ(70) = 
		  dAdβ(71) = 
		  dAdβ(72) = 
		  dAdβ(73) = 
		  dAdβ(74) = 
		  dAdβ(75) = 
		  dAdβ(76) = 
		  dAdβ(77) = 
		  dAdβ(78) = 
		  dAdβ(79) = 
		  dAdβ(80) = 
		  dAdβ(81) = 
		  dAdβ(82) = 
		  dAdβ(83) = 
		  dAdβ(84) = 
		  dAdβ(85) = 
		  dAdβ(86) = 
		  dAdβ(87) = 
		  dAdβ(88) = 
		  dAdβ(89) = 
		  dAdβ(90) = 
		  dAdβ(91) = 
		  dAdβ(92) = 
		  dAdβ(93) = 
		  dAdβ(94) = 
		  dAdβ(95) = 
		  dAdβ(96) = 
		  dAdβ(97) = 
		  dAdβ(98) = 
		  dAdβ(99) = 
		  dAdβ(100) = 
		  dAdβ(101) = 
		  dAdβ(102) = 
		  dAdβ(103) = 
		  dAdβ(104) = 
		  dAdβ(105) = 
		  dAdβ(106) = 
		  dAdβ(107) = 
		  dAdβ(108) = 
		  dAdβ(109) = 
		  dAdβ(110) = 
		  dAdβ(111) = 
		  dAdβ(112) = 
		  dAdβ(113) = 
		  dAdβ(114) = 
		  dAdβ(115) = 
		  dAdβ(116) = 
		  dAdβ(117) = 
		  dAdβ(118) = 
		  dAdβ(119) = 
		  dAdβ(120) = 
		  dAdβ(121) = 
		  dAdβ(122) = 
		  dAdβ(123) = 
		  dAdβ(124) = 
		  dAdβ(125) = 
		  dAdβ(126) = 
		  dAdβ(127) = 
		  dAdβ(128) = 
		  dAdβ(129) = 4*cβ *c1*s1p3
		  dAdβ(130) = 2*sβ *s1p4
		  dAdβ(131) = −4*s1*cβ *c1p3
		  dAdβ(132) = 2*sβ *c1p4
		  dAdβ(133) = − 45/4* δ*c2β *c1p2*s1p4
		  dAdβ(134) = −9*δ*s2β *c1*s5
		  dAdβ(135) = 9/4 *δ*c2β *s1p6
		  dAdβ(136) = − 43/128* δ*sβ2 *c2 + 43/128* δ*cβ2 *c2 − 23/64 *δ*c2β* c4 + 5/128 *δ*c2β *c6 + 1/64 *δ*sβ2 − 1/64* δ*cβ2
		  dAdβ(137) = − 1/2 *δ*s2β *s1p3*c2*c1 + 1/2 *δ*s2β *s1p3*c1
		  dAdβ(138) = 1/4 *δ*c2β *c1p2*s1p4
		  dAdβ(139) = δ*sβ *cβ *s4
		  dAdβ(140) = − 43/128* δ*sβ2 *c2 + 43/128 *δ*cβ2 *c2 + 23/64 *δ*c2β *c4 + 5/128 *δ*c2β* c6 − 1/64 *δ*sβ2 + 1/64 *δ*cβ2
		  dAdβ(141) = − 1/2 *δ*s2β *s1*c2*c1p3 + 1/2 *δ*s2β *s1*c1p3
		  dAdβ(142) = − 1/4 *δ*c2β *c1p4*s1p2
		  dAdβ(143) = 45/4 *δ*c2β *c1p4*s1p2
		  dAdβ(144) = −9*δ*s2β *s1*c1p5
		  dAdβ(145) = − 9/4 *δ*c2β *c1p6
		  dAdβ(146) = −12*η*cβ *c1p3*s1p5 + 4*cβ *c1p3*s1p5
		  dAdβ(147) = − 8/3 *η*sβ *c1p2*s1p6
		  dAdβ(148) = −4*η*cβ *c1*s1p7 + 4/3 *cβ *c1*s1p7
		  dAdβ(149) = −8*η*sβ2 *s1p8 + 8*η*cβ2 *s1p8 + 8/3 *sβ2 *s1p8 − 8/3 *cβ2 *s1p8
		  dAdβ(150) = − 9/4 *η*cβ *s1p3*c2*c1 + 3/4 *cβ *s1p3*c2*c1 − 3/8 *η*cβ *s1p3*c4*c1 + 1/8 *cβ *s1p3*c4*c1 + 103/24 *η*cβ *s1p3*c1 − 79/8 *cβ *s1p3*c1
		  dAdβ(151) = 7/2 *η*sβ *s1p4*c2 − 7/6 *sβ *s1p4*c2 − 1/8 *η*sβ *s1p4*c4 + 1/24 *sβ *s1p4*c4 + 119/24* η*sβ *s1p4 − 47/8 *sβ *s1p4
		  dAdβ(152) = − 6*η*sβ *s2β *s1p5*c2*c1 + 3*η*cβ *c2β *s1p5*c2*c1 + η*cβ *s1p5*c2*c1 + 2*sβ *s2β *s1p5*c2*c1− cβ *c2β *s1p5*c2*c1 − 1/3 *cβ* s1p5*c2*c1 − 4*η*cβ *s1p5*c1 + 4/3 *cβ *s1p5*c1
		  dAdβ(153) = −2*η*sβ3 *c1p2*s1p6 + 4*η*sβ *cβ2 *s1p6*c1p2 + 2/3 *sβ3 *c1p2*s1p6 − 4/3 *sβ *cβ2 *c1p2*s1p6
		  dAdβ(154) = − 15/2 *η*sβ3 *s2p2*c2 + 15*η*sβ *cβ2 *s2p2*c2 + 5/2 *sβ3 *s2p2*c2 − 5*sβ *cβ2 *s2p2*c2
		  dAdβ(155) = − 9/4 *η*cβ *s1*c1p3*c2 + 3/4 *cβ *s1*c1p3*c2 + 3/8 *η*cβ *s1*c1p3*c4 − 1/8* cβ *s1*c1p3*c4− 103/24 *η*cβ *s1*c1p3 + 79/8 *cβ *s1*c1p3
		  dAdβ(156) = − 7/2 *η*sβ *c1p4*c2 + 7/6 *sβ *c1p4*c2 − 1/8 *η*sβ *c1p4*c4 + 1/24 *sβ *c1p4*c4 + 119/24 *η*sβ *c1p4− 47/8 *sβ *c1p4
		  dAdβ(157) = − 6*η*sβ *s2β *s1*c1p5*c2 + 3*η*cβ *c2β *s1*c1p5*c2 + η*cβ *s1*c1p5*c2 + 2*sβ *s2β *s1*c1p5*c2−cβ *c2β *s1*c1p5*c2 − 1/3 *cβ *s1*c1p5*c2 + 4*η*cβ *s1*c1p5 − 4/3 *cβ *s1*c1p5
		  dAdβ(158) = −2*η*sβ3 *s1p2*c1p6 + 4*η*sβ *cβ2 *s1p2*c1p6 + 2/3 *sβ3 *s1p2*c1p6 − 4/3 *sβ *cβ2 *s1p2*c1p6
		  dAdβ(159) = 12*η*cβ *c1p5*s1p3 − 4*cβ *c1p5*s1p3
		  dAdβ(160) = −4*η*sβ *c1p6*s1p2 + 4/3 *sβ *c1p6*s1p2
		  dAdβ(161) = 48*η*sβ *s2β *c1p7*s1 − 24*η*cβ *c2β *c1p7*s1 − 8*η*cβ *c1p7*s1 − 16*sβ *s2β *c1p7*s1 + 8*cβ *c2β *c1p7*s1 + 8/3 *cβ *c1p7*s1
		  dAdβ(162) = −8*η*sβ3* c1p8 + 16*η*sβ *cβ2 *c1p8 + 8/3 *sβ3 *c1p8 − 16/3 *sβ *cβ2 *c1p8
		  dAdβ(163) = 0
		  dAdβ(164) = 0
		  dAdβ(165) = sβ *cβ *c2*χxa − 1/2 *sβ2 *c2*χaz + 1/2 *cβ2 *c2*χaz − sβ *cβ *χxa + 1/2 *sβ2 *χaz − 1/2 *cβ2 *χaz
		  dAdβ(166) = −sβ2 *s2*χxa + cβ2 *s2*χxa − 2*sβ *cβ *s2*χaz
		  dAdβ(167) = −sβ *cβ *c2*χxa + 1/2 *sβ2 *c2*χaz − 1/2 *cβ2 *c2*χaz − sβ* cβ *χxa + 1/2 *sβ2 *χaz − 1/2 *cβ2 *χaz
		  dAdβ(168) = 0
		  dAdβ(169) = 0
		  dAdβ(170) = δ*sβ *cβ *c2*χsx − 1/2 *δ*sβ2 *c2*χsz + 1/2 *δ*cβ2 *c2*χsz − δ*sβ *cβ *χsx + 1/2* δ*sβ2 *χsz − 1/2* δ*cβ2 *χsz
		  dAdβ(171) = −δ*sβ2 *s2*χsx + δ*cβ2 *s2*χsx − 2*δ*sβ *cβ *s2*χsz
		  dAdβ(172) = −δ*sβ *cβ *c2*χsx + 1/2* δ*sβ2 *c2*χsz − 1/2* δ*cβ2 *c2*χsz − δ*sβ *cβ *χsx + 1/2 *δ*sβ2 *χsz − 1/2 *δ*cβ2 *χsz
		  dAdβ(173) = 8*π*cβ *c1*s1p3
		  dAdβ(174) = 4*π*sβ *s1p4
		  dAdβ(175) = −8*π*cβ *c1p3*s1
		  dAdβ(176) = 4*π*sβ *c1p4
		  dAdβ(177) = 4375/96 *δ*η*c2β *s1p6*c1p4 + 4375/32 *δ*η*c4β *s1p6*c1p4 − 4375/192* δ*c2β *s1p6*c1p4 − 4375/64 *δ*c4β *s1p6*c1p4
		  dAdβ(178) = 625/24 *δ*η*s2β *s1p7*c1p3 + 625/4 *δ*η*s4β *s1p7*c1p3 − 625/48 *δ*s2β *s1p7*c1p3 − 625/8 *δ*s4β *s1p7*c1p3
		  dAdβ(179) = 625/64 *δ*η*c2β *s1p8*c1p2 − 5625/64 *δ*η*c4β *s1p8*c1p2 − 625/128 *δ*c2β *s1p8*c1p2 + 5625/128 *δ*c4β *s1p8*c1p2
		  dAdβ(180) = 625/12 *δ*η*sβ2 *s2β *s1p9*c1 − 625/12 *δ*η*sβ *cβ *c2β *s1p9*c1 − 625/24 *δ*η*sβ *cβ *s1p9*c1− 625/24* δ*sβ2* s2β *s1p9*c1 + 625/24 *δ*sβ* cβ* c2β *s1p9*c1 + 625/48* δ*sβ* cβ* s1p9*c1
		  dAdβ(181) = 625/96 *δ*η*sβ4* s1p10 − 625/32 *δ*η*sβ2* cβ2 *s1p10 − 625/192* δ*sβ4 *s1p10 + 625/64* δ*sβ2 *cβ2 *s1p10
		  dAdβ(182) = 459/64 *δ*η*c2β *c1p2*c2*s1p4 − 2079/64 *δ*η*c4β* c1p2*c2*s1p4 − 459/128 *δ*c2β *c1p2*c2*s1p4+ 2079/128* δ*c4β *c1p2*c2*s1p4 + 567/256 *δ*η*c2β *c1p2*c4*s1p4 + 1701/256 *δ*η*c4β *c1p2*c4*s1p4− 567/512* δ*c2β *c1p2*c4*s1p4 − 1701/512* δ*c4β *c1p2*c4*s1p4 − 4923/256 *δ*η*c2β *c1p2*s1p4 − 945/256 *δ*η*c4β *c1p2*s1p4+ 22203/512* δ*c2β *c1p2*s1p4 + 945/512 *δ*c4β *c1p2*s1p4
		  dAdβ(183) = − 27/8 *δ*η*s2β* c1*c2*s1p5 + 27/16* δ*s2β *c1*c2*s1p5 + 27/8 *δ*s4β *c1*c2*s1p5 + 81/64 *δ*η*s2β *c1*c4*s1p5+ 243/32 *δ*η*s4β *c1*c4*s1p5 − 81/128 *δ*s2β *c1*c4*s1p5 − 243/64 *δ*s4β *c1*c4*s1p5 − 1233/64 *δ*η*s2β *c1*s1p5 − 27/32 *δ*η*s4β *c1*s1p5+ 4689/128 *δ*s2β* c1*s1p5 + 27/64 *δ*s4β *c1*s1p5
		  dAdβ(184) = 621/128 *δ*η*s2β^2 *c1*c2*s1p6 − 621/128 *δ*η*c2β^2 *c1*c2*s1p6 + 837/128 *δ*η*c2β* c1*c2*s1p6 − 621/256* δ*s2β^2 *c1*c2*s1p6+ 621/256* δ*c2β^2 *c1*c2*s1p6 − 837/256 *δ*c2β *c1*c2*s1p6 + 2187/512 *δ*η*s2β^2 *c1*c4*s1p6 − 2187/512 *δ*η*c2β^2* c1*c4*s1p6 + 243/512 *δ*η*c2β *c1*c4*s1p6− 2187/1024* δ*s2β^2 *c1*c4*s1p6 + 2187/1024* δ*c2β^2 *c1*c4*s1p6 − 243/1024* δ*c2β *c1*c4*s1p6 + 1377/512 *δ*η*s2β^2 *c1*s1p6 − 1377/512 *δ*η*c2β^2 *c1*s1p6+ 4761/512 *δ*η*c2β *c1*s1p6 − 1377/1024* δ*s2β^2 *c1*s1p6 + 1377/1024* δ*c2β^2 *c1*s1p6 − 11673/1024 *δ*c2β *c1*s1p6
		  dAdβ(185) = 81/8 *δ*η*cβ *s2β *sβ2* c1*c2*s1p7 − 81/8 *δ*η*cβ* c2β *sβ *c1*c2*s1p7 − 81/16 *δ*η*cβ* sβ *c1*c2*s1p7− 81/16 *δ*s2β *sβ2 *c1*c2*s1p7 + 81/16 *δ*cβ *c2β *sβ *c1*c2*s1p7 + 81/32 *δ*cβ* sβ *c1*c2*s1p7 + 27/8 *δ*η*s2β* sβ2 *c1*s1p7− 27/8 *δ*η*cβ *c2β *sβ *c1*s1p7 + 81/16 *δ*η*cβ *sβ *c1*s1p7 − 27/16 *δ*s2β* sβ2 *c1*s1p7 + 27/16 *δ*cβ *c2β* sβ *c1*s1p7− 81/32 *δ*cβ *sβ *c1*s1p7
		  dAdβ(186) = 81/32 *δ*η*sβ4 *s1p8*c1p2 − 243/32 *δ*η*sβ2 *cβ2 *s1p8*c1p2 − 81/64* δ*sβ4 *s1p8*c1p2 + 243/64 *δ*sβ2 *cβ2 *s1p8*c1p2
		  dAdβ(187) = − 7/24576 *δ*η*c2β *c10 − 7/8192 *δ*η*c4β* c10 + 7/49152 *δ*c2β *c10 + 7/16384 *δ*c4β* c10 + 173/24576 *δ*η*sβ *c2*c3β− 173/49152 *δ*sβ *c2*c3β − 6031/24576 *δ*η*sβ2 *c2 + 6031/24576 *δ*η*cβ2 *c2 + 10511/49152* δ*sβ2 *c2 − 10511/49152 *δ*cβ2 *c2− 1/8192 *δ*η*sβ* c3β + 1/16384 *δ*sβ* c3β − 11/3072 *δ*η*s2β^2* c4 + 11/3072 *δ*η*c2β^2 *c4 − 679/3072 *δ*η*c2β *c4+ 11/6144 *δ*s2β^2* c4 − 11/6144 *δ*c2β^2 *c4 + 557/2048* δ*c2β *c4 + 91/8192 *δ*η*s2β^2* c6 − 91/8192 *δ*η*c2β^2 *c6 − 201/8192* δ*η*c2β *c6− 91/16384* δ*s2β^2 *c6 + 91/16384* δ*c2β^2 *c6 − 1719/16384 *δ*c2β *c6 − 37/12288 *δ*η*c2β* c8 + 91/4096 *δ*η*c4β *c8+ 37/24576 *δ*c2β *c8 − 91/8192 *δ*c4β *c8 − 85/8192 *δ*η*sβ2 + 85/8192 *δ*η*cβ2 − 683/16384 *δ*sβ2 + 683/16384* δ*cβ2
		  dAdβ(188) = 37/128 *δ*η*s2β *s1p3*c2*c1 + 347/256 *δ*s2β *s1p3*c2*c1 − 3/64 *δ*η*s2β *s1p3*c4*c1 + 3/128 *δ*s2β *s1p3*c4*c1+ 1/384 *δ*η*s2β *s1p3*c6*c1 − 1/768 *δ*s2β *s1p3*c6*c1 + 35/64 *δ*η*s2β *s1p3*c1 − 79/384 *δ*η*s4β *s1p3*c1 + 23/384 *δ*s2β *s1p3*c1+ 79/768 *δ*s4β *s1p3*c1 − 19/128 *δ*η*s4β *s1p3*c3 + 19/256 *δ*s4β *s1p3*c3 − 9/128 *δ*η*s4β *s1p3*c5 + 9/256 *δ*s4β *s1p3*c5+ 1/128 *δ*η*s4β *s1p3*c7 − 1/256 *δ*s4β *s1p3*c7
		  dAdβ(189) = 13/64 *δ*η*c2β *c1p2*c2*s1p4 + 11/64 *δ*η*c4β *c1p2*c2*s1p4 − 13/128 *δ*c2β *c1p2*c2*s1p4− 11/128 *δ*c4β *c1p2*c2*s1p4 + 1/256 *δ*η*c2β *c1p2*c4*s1p4 − 9/256 *δ*η*c4β *c1p2*c4*s1p4 − 1/512 *δ*c2β *c1p2*c4*s1p4+ 9/512 *δ*c4β *c1p2*c4*s1p4 − 29/256 *δ*η*c2β *c1p2*s1p4 + 5/256 *δ*η*c4β *c1p2*s1p4 − 355/512* δ*c2β *c1p2*s1p4 − 5/512 *δ*c4β *c1p2*s1p4
		  dAdβ(190) = 1/12 *δ*η*sβ2 *s2β *c1p3*c2*s1p5 − 1/12 *δ*η*cβ* c2β *sβ *c1p3*c2*s1p5 − 1/24 *δ*η*cβ *sβ *c1p3*c2*s1p5− 1/24* δ*sβ2 *s2β *c1p3*c2*s1p5 + 1/24 *δ*cβ *c2β *sβ *c1p3*c2*s1p5 + 1/48 *δ*cβ* sβ *c1p3*c2*s1p5 − 1/12 *δ*η*sβ2 *s2β *c1p3*s1p5+ 1/12 *δ*η*cβ* c2β *sβ *c1p3*s1p5 + 7/24* δ*η*cβ* sβ *c1p3*s1p5 + 1/24* δ*sβ2 *s2β *c1p3*s1p5 − 1/24 *δ*cβ *c2β *sβ *c1p3*s1p5− 7/48 *δ*cβ* sβ *c1p3*s1p5
		  dAdβ(191) = 1/48 *δ*η*sβ4 *c1p4*s1p6 − 1/16 *δ*η*sβ2 *cβ2* c1p4*s1p6 − 1/96 *δ*sβ4 *c1p4*s1p6 + 1/32 *δ*sβ2 *cβ2 *c1p4*s1p6
		  dAdβ(192) = 1/64 *δ*η*s2β* sβ2* s4 − 1/64 *δ*η*cβ* c2β* sβ* s4 + 45/64 *δ*η*cβ* sβ* s4 − 7/128 *δ*η*s2β *sβ2 *s8+ 7/128 *δ*η*cβ *c2β *sβ *s8 − 5/128 *δ*η*cβ* sβ *s8 − 1/256* δ*sβ3 *s4 + 1/128 *δ*cβ2 *sβ *s4 − 77/128 *δ*cβ* sβ *s4− 7/256* δ*sβ2 *s2β* s8 + 7/256 *δ*cβ *c2β *sβ *s8 + 5/256 *δ*cβ* sβ *s8
		  dAdβ(193) = 189/16 *δ*η*s2β *sβ2 *c2*s2p3 − 189/16 *δ*η*cβ *c2β *sβ *c2*s2p3 − 135/16 *δ*η*cβ *sβ *c2*s2p3 − 189/32 *δ*s2β *sβ2 *c2*s2p3+ 189/32 *δ*cβ* c2β* sβ *c2*s2p3 + 135/32 *δ*cβ* sβ* c2*s2p3
		  dAdβ(194) = − 7/24576 *δ*η*c2β *c10 − 7/8192 *δ*η*c4β *c10 + 7/49152 *δ*c2β *c10 + 7/16384 *δ*c4β *c10 + 173/24576 *δ*η*sβ *c2*c3β− 173/49152 *δ*sβ *c2*c3β − 6031/24576 *δ*η*sβ2 *c2 + 6031/24576 *δ*η*cβ2 *c2 + 10511/49152* δ*sβ2 *c2 − 10511/49152* δ*cβ2* c2+ 1/8192* δ*η*sβ *c3β + 11/3072 *δ*η*s2β^2 *c4 − 11/3072 *δ*η*c2β^2 *c4 + 679/3072 *δ*η*c2β* c4 − 11/6144 *δ*s2β^2 *c4 + 11/6144 *δ*c2β^2 *c4− 557/2048 *δ*c2β *c4 + 91/8192* δ*η*s2β^2* c6 − 91/8192 *δ*η*c2β^2 *c6 − 201/8192 *δ*η*c2β *c6 − 91/16384 *δ*s2β^2 *c6 + 91/16384* δ*c2β^2 *c6− 1719/16384 *δ*c2β *c6 + 37/12288 *δ*η*c2β* c8 − 91/4096 *δ*η*c4β *c8 − 37/24576 *δ*c2β *c8 + 91/8192 *δ*c4β *c8 + 85/8192 *δ*η*sβ2− 85/8192 *δ*η*cβ2 + 341/8192* δ*sβ2 − 341/8192* δ*cβ2
		  dAdβ(195) = − 37/128 *δ*η*s2β *c1p3*c2*s1 − 347/256 *δ*s2β *c1p3*c2*s1 − 3/64 *δ*η*s2β *c1p3*c4*s1 + 3/128 *δ*s2β *c1p3*c4*s1 - 1/384 *δ*η*s2β *c1p3*c6*s1 + 1/768 *δ*s2β *c1p3*c6*s1 + 35/64 *δ*η*s2β *c1p3*s1 − 79/384 *δ*η*s4β *c1p3*s1 + 19/128 *δ*η*s4β *c1p3*s3− 9/128 *δ*η*s4β *c1p3*s5 − 1/128 *δ*η*s4β *c1p3*s7 + 23/384 *δ*s2β *c1p3*s1 + 79/768 *δ*s4β *c1p3*s1 − 19/256 *δ*s4β *c1p3*s3+ 9/256 *δ*s4β* c1p3*s5 + 1/256 *δ*s4β *c1p3*s7
		  dAdβ(196) = − 459/128 *δ*η*c2β *c1p4*c2*s1p2 + 2079/128 *δ*η*c4β *c1p4*c2*s1p2 + 567/512 *δ*η*c2β *c1p4*c4*s1p2+ 1701/512 *δ*η*c4β *c1p4*c4*s1p2 + 4923/256 *δ*η*c2β *c1p4*s1p2 − 945/512 *δ*η*c4β *c1p4*s1p2
		  dAdβ(197) = 27/8 *δ*η*s2β *c1p5*c2*s1 + 27/4 *δ*η*s4β *c1p5*c2*s1 − 27/16* δ*s2β *c1p5*c2*s1 − 27/8 *δ*s4β *c1p5*c2*s1+ 81/64 *δ*η*s2β *c1p5*c4*s1 + 243/32 *δ*η*s4β *c1p5*c4*s1 − 81/128 *δ*s2β *c1p5*c4*s1 − 243/64 *δ*s4β *c1p5*c4*s1 − 1233/64 *δ*η*s2β *c1p5*s1− 27/32 *δ*η*s4β *c1p5*s1 + 4689/128 *δ*s2β *c1p5*s1 + 27/64 *δ*s4β *c1p5*s1
		  dAdβ(198) = 837/128 *δ*η*c2β *c1p6*c2 − 621/128* δ*η*c4β *c1p6*c2 − 837/256 *δ*c2β *c1p6*c2 + 621/256 *δ*c4β *c1p6*c2 − 243/512 *δ*η*c2β *c1p6*c4+ 2187/512 *δ*η*c4β *c1p6*c4 + 243/1024* δ*c2β *c1p6*c4 − 2187/1024* δ*c4β *c1p6*c4 − 4761/512 *δ*η*c2β *c1p6 + 1377/512 *δ*η*c4β *c1p6+ 11673/1024 *δ*c2β *c1p6 − 1377/1024* δ*c4β *c1p6
		  dAdβ(199) = − 81/8 *δ*η*sβ2 *s2β *c1p7*c2*s1 + 81/8 *δ*η*cβ *c2β *sβ *c1p7*c2*s1 + 81/16 *δ*η*cβ *sβ *c1p7*c2*s1+ 81/16 *δ*sβ2 *s2β *c1p7*c2*s1 − 81/16 *δ*cβ *c2β *sβ *c1p7*c2*s1 − 81/32 *δ*cβ *sβ *c1p7*c2*s1 + 27/8 *δ*η*sβ2 *s2β *c1p7*s1− 27/8 *δ*η*cβ *c2β *sβ *c1p7*s1 + 81/16 *δ*η*cβ *sβ *c1p7*s1 − 27/16* δ*sβ2 *s2β *c1p7*s1+ 27/16 *δ*cβ *c2β *sβ *c1p7*s1 − 81/32 *δ*cβ *sβ *c1p7*s1
		  dAdβ(200) = − 81/32 *δ*η*sβ4 *s1p2*c1p8 + 243/32 *δ*η*sβ2 *cβ2 *s1p2*c1p8 + 81/64* δ*sβ4 *s1p2*c1p8 − 243/64* δ*sβ2 *cβ2 *s1p2*c1p8
		  dAdβ(201) = − 4375/96 *δ*η*c2β *s1p4*c1p6 − 4375/32 *δ*η*c4β *s1p4*c1p6 + 4375/192* δ*c2β *s1p4*c1p6 + 4375/64 *δ*c4β *s1p4*c1p6
		  dAdβ(202) = 625/24 *δ*η*s2β *s1p3*c1p7 + 625/4 *δ*η*s4β *s1p3*c1p7 − 625/48 *δ*s2β *s1p3*c1p7 − 625/8* δ*s4β *s1p3*c1p7
		  dAdβ(203) = − 625/64 *δ*η*c2β *s1p2*c1p8 + 5625/64 *δ*η*c4β *s1p2*c1p8 + 625/128* δ*c2β *s1p2*c1p8 − 5625/128* δ*c4β *s1p2*c1p8
		  dAdβ(204) = 625/12 *δ*η*sβ2 *s2β *s1*c1p9 − 625/12 *δ*η*sβ *cβ* c2β *s1*c1p9 − 625/24 *δ*η*sβ *cβ *s1*c1p9− 625/24* δ*sβ2 *s2β *s1*c1p9 + 625/24* δ*sβ *cβ *c2β *s1*c1p9 + 625/48 *δ*sβ *cβ *s1*c1p9
		  dAdβ(205) = − 625/96* δ*η*sβ4 *c1p10 + 625/32* δ*η*sβ2 *cβ2 *c1p10 + 625/192* δ*sβ4 *c1p10 − 625/64* δ*sβ2 *cβ2 *c1p10
		  dAdβ(206) = −η*cβ *c2p3*χsy + 2*cβ *c2p3*χsy
		  dAdβ(207) = 2/3 *η*cβ *c2*c1p4*χsy + 20/3 *cβ *c2*c1p4*χsy − 5/3 *η*cβ *c1p4*χsy − 14/3 *cβ *c1p4*χsy
		  dAdβ(208) = 2/3 *η*sβ *s1*c1p5*χsy + 20/3 *sβ *s1*c1p5*χsy
		  dAdβ(209) = − 1/3 *η*sβ *s1*c2*c1p3*χsy − 10/3 *sβ *s1*c2*c1p3*χsy − 7/3 *η*sβ *s1*c1p3*χsy + 2/3* sβ *s1*c1p3*χsy
		  dAdβ(210) = 1/3 *η*sβ *s1p3*c2*c1*χsy + 10/3 *sβ *s1p3*c2*c1*χsy − 7/3 *η*sβ *s1p3*c1*χsy + 2/3 *sβ *s1p3*c1*χsy
		  dAdβ(211) = 2/3 *η*cβ *s1p4*c2*χsy + 20/3 *cβ *s1p4*c2*χsy + 5/3 *η*cβ *s1p4*χsy + 14/3 *cβ *s1p4*χsy
		  dAdβ(212) = 2/3* η*sβ *s1p5*c1*χsy + 20/3 *sβ *s1p5*c1*χsy
		  dAdβ(213) = −η*cβ *s2p2*c2*χsy + 2*cβ *s2p2*c2*χsy
		  dAdβ(214) = 1/3 *η*cβ* s2p2*c2*χsy + 10/3 *cβ *s2p2*c2*χsy
		  dAdβ(215) = − 1/2 *η*sβ *s2p3*χsy + sβ *s2p3*χsy
		  dAdβ(216) = − 5/8 *η*sβ *s2*χsy − 1/8 *η*sβ *s6*χsy + 5/4 *sβ* s2*χsy + 1/4 *sβ *s6*χsy
		  dAdβ(217) = − 1/4 *η*sβ *s2*c4*χsx + 1/2 *sβ *s2*c4*χsx + η*cβ *s2*c4*χsz − 2*cβ *s2*c4*χsz − 3/4 *η*sβ *s2*χsx + 3/2 *sβ *s2*χsx
		  dAdβ(218) = η*cβ *c2*s2p2*χsx − 2*cβ*c2*s2p2*χsx + η*sβ* s2p2*c2*χsz − 2*sβ *c2*s2p2*χsz
		  dAdβ(219) = 2*η*sβ *s2p3*χsx − 4*sβ *s2p3*χsx
		  dAdβ(220) = 1/3 *η*sβ* c1*c2*s1p3*χsx + 10/3 *sβ *c1*c2*s1p3*χsx − 4/3 *η*cβ *c1*c2*s1p3*χsz − 40/3 *cβ *c1*c2*s1p3*χsz+ 5/3 *η*sβ *c1*s1p3*χsx + 2/3* sβ* c1*s1p3*χsx − 2*η*cβ *c1*s1p3*χsz − 4*cβ* c1*s1p3*χsz
		  dAdβ(221) = − 2/3 *η*cβ *c2*s1p4*χsx − 20/3 *cβ *c2*s1p4*χsx − 2/3 *η*sβ *c2*s1p4*χsz − 20/3 *sβ *c2*s1p4*χsz+ 10/3* η*cβ *s1p4*χsx − 14/3 *cβ *s1p4*χsx − 5*η*sβ *s1p4*χsz − 4*sβ *s1p4*χsz
		  dAdβ(222) = − 2/3 *η*sβ *s1p5*c1*χsx − 20/3 *sβ *s1p5*c1*χsx
		  dAdβ(223) = −6*η*sβ *s2β *s2p2*χsx + 3*η*cβ *c2β* s2p2*χsx − 7/6 *η*cβ* s2p2*χsx + 1/3 *cβ *s2p2*χsx + 6*η*sβ3* s2p2*χsz − 12*η*sβ* cβ2* s2p2*χsz
		  dAdβ(224) = − 1/3* η*sβ *c1p3*c2*s1*χsx − 10/3 *sβ *c1p3*c2*s1*χsx + 4/3 *η*cβ *c1p3*c2*s1*χsz + 40/3 *cβ *c1p3*c2*s1*χsz+ 5/3 *η*sβ *c1p3*s1*χsx + 2/3 *sβ *c1p3*s1*χsx − 2*η*cβ *c1p3*s1*χsz − 4*cβ *c1p3*s1*χsz
		  dAdβ(225) = − 2/3 *η*cβ *c1p4*c2*χsx − 20/3 *cβ *c1p4*c2*χsx − 2/3 *η*sβ *c1p4*c2*χsz − 20/3 *sβ *c1p4*c2*χsz − 10/3 *η*cβ *c1p4*χsx+ 14/3 *cβ *c1p4*χsx + 5*η*sβ *c1p4*χsz + 4*sβ *c1p4*χsz
		  dAdβ(226) = − 2/3 *η*sβ *s1*c1p5*χsx − 20/3 *sβ *s1*c1p5*χsx
		  dAdβ(227) = 2*δ*cβ *c2p3*χay
		  dAdβ(228) = 20/3* δ*cβ *c2*c1p4*χay − 14/3 *δ*cβ *c1p4*χay
		  dAdβ(229) = − 10/3 *δ*sβ *s1*c2*c1p3*χay + 2/3 *δ*sβ *s1*c1p3*χay
		  dAdβ(230) = 20/3 *δ*sβ *s1*c1p5*χay
		  dAdβ(231) = 10/3 *δ*sβ *s1p3*c2*c1*χay + 2/3 *δ*sβ *s1p3*c1*χay
		  dAdβ(232) = 20/3 *δ*cβ *s1p4*c2*χay + 14/3 *δ*cβ *s1p4*χay
		  dAdβ(233) = 20/3 *δ*sβ *c1*s1p5*χay
		  dAdβ(234) = 2*δ*cβ *c2*s2p2*χay
		  dAdβ(235) = 10/3* δ*cβ* s2p2*c2*χay
		  dAdβ(236) = δ*sβ *s1p3*χay
		  dAdβ(237) = 5/4 *δ*sβ *s2*χay + 1/4 *δ*sβ *s6*χay
		  dAdβ(238) = 1/2 *δ*sβ *s2*c4*χxa − 2*δ*cβ *s2*c4*χaz + 3/2 *δ*sβ* s2*χxa
		  dAdβ(239) = −2*δ*cβ *c2*s2p2*χxa − 2*δ*sβ *c2*s2p2*χaz
		  dAdβ(240) = −δ*sβ *s2p3*χxa
		  dAdβ(241) = 10/3 *δ*sβ *s1p3*c2*c1*χxa − 40/3* δ*cβ *s1p3*c2*c1*χaz + 2/3 *δ*sβ *s1p3*c1*χxa − 4*δ*cβ *s1p3*c1*χaz
		  dAdβ(242) = − 20/3 *δ*cβ *s1p4*c2*χxa − 20/3 *δ*sβ *s1p4*c2*χaz − 14/3 *δ*cβ *s1p4*χxa − 4*δ*sβ *s1p4*χaz
		  dAdβ(243) = − 20/3 *δ*sβ *c1*s1p5*χxa
		  dAdβ(244) = 1/3* δ*cβ* s2p2*χxa
		  dAdβ(245) = − 10/3* δ*sβ *s1*c2*c1p3*χxa + 40/3 *δ*cβ *s1*c2*c1p3*χaz + 2/3 *δ*sβ *s1*c1p3*χxa − 4*δ*cβ *s1*c1p3*χaz
		  dAdβ(246) = − 20/3 *δ*cβ *c2*c1p4*χxa − 20/3 *δ*sβ *c2*c1p4*χaz + 14/3 *δ*cβ *c1p4*χxa + 4*δ*sβ *c1p4*χaz
		  dAdβ(247) = − 20/3 *δ*sβ *s1*c1p5*χxa
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDAdδ()
		  //load constant parameters 
		  Var ιDN As Double  =self.ιDN 
		  Var β As Double  = self.β 
		  Var c2 As Double  = Trig.c2
		  Var s2 As Double = Trig.s2
		  Var c1 As Double = Trig.c1
		  Var s1   As Double = Trig.s1
		  Var c3   As Double = Trig.c3
		  Var s3  As Double  = Trig.s3
		  Var c4   As Double = Trig.c4
		  Var s4  As Double  = Trig.s4
		  Var c5  As Double  = Trig.c5
		  Var s5   As Double = Trig.s5
		  Var c6 As Double   = Trig.c6
		  Var s6   As Double = Trig.s6
		  Var c7   As Double = Trig.c7
		  Var s7 As Double   = Trig.s7
		  Var c8  As Double  = Trig.c8
		  Var s8  As Double  = Trig.s8
		  Var c9  As Double  = Trig.c9
		  Var s9   As Double = Trig.s9
		  Var c10  As Double  = Trig.c10
		  Var s10  As Double  = Trig.s10
		  Var c1p2  As Double  = Trig.c1p2
		  Var c1p3  As Double  = Trig.c1p3   
		  Var c1p4  As Double  =Trig. c1p4   
		  Var c1p5 As Double  = Trig.c1p5   
		  Var c1p6 As Double   = Trig.c1p6   
		  Var c1p7  As Double  = Trig.c1p7 
		  Var c1p8 As Double   = Trig.c1p8  
		  Var c1p9   As Double = Trig.c1p9  
		  Var c1p10  As Double  = Trig.c1p10
		  Var s1p2   As Double = Trig.s1p2   
		  Var s1p3  As Double  = Trig.s1p3   
		  Var s1p4As Double    = Trig.s1p4   
		  Var s1p5   As Double = Trig.s1p5  
		  Var s1p6 As Double   = Trig.s1p6   
		  Var s1p7 As Double   = Trig.s1p7   
		  Var s1p8  As Double  = Trig.s1p8   
		  Var s1p9 As Double   = Trig.s1p
		  Var s1p10 As Double  = Trig.s1p10
		  Var c2p2  As Double  = Trig.c2p2
		  Var c2p3   As Double = Trig.c2p3
		  Var c2p4 As Double  = Trig.c2p4
		  Var s2p2  As Double  = Trig.s2p2
		  Var s2p3  As Double  = Trig.s2p3
		  Var s2p4 As Double   = Trig.s2p4
		  Var s2p5 As Double   = Trig.s2p5
		  
		  // Define local β trig functions
		  Var cβ  As Double  = Trig.cβ
		  Var sβ  As Double  = Trig.sβ
		  Var c2β As Double   = Trig.c2β
		  Var s2β  As Double  = Trig.s2β
		  Var c3β As Double   = Trig.c3β
		  Var s3β  As Double  = Trig.s3β
		  Var c4β As Double   = Trig.c4β
		  Var s4β  As Double  = Trig.s4β
		  Var c5β As Double   = Trig.c5β
		  Var s5β As Double   = Trig.s5β
		  Var cβ2  As Double  = Trig.cβ2
		  Var cβ3 As Double   = Trig.cβ3
		  Var sβ2  As Double  = Trig.sβ2
		  Var sβ3  As Double  = Trig.sβ3
		  
		  // Load dAdδ derivatives
		  dAdδ(0) = 0
		  dAdδ(1) = 0
		  dAdδ(2) = 0
		  dAdδ(3) = 0
		  dAdδ(4) = 0
		  dAdδ(5) = − 45/32*sβ*c1p6 − 9/32*s3β*c1p6
		  dAdδ(6) = − 175/256*sβ*c1p2 + 87/64*sβ*c1p2*c2 − 5/64*s3β*c1p2*c2 − 5/256*sβ*c1p2*c4 +15/256*s3β*c1p2*c4 + 13/256*s3β*c1p2
		  dAdδ(7) = 175/256* sβ*s1p2 + 87/64 *sβ*s1p2 *c2 − 5/64 *s3β*s1p2 *c2 + 5/256* sβ*s1p2 * c4 − 15/256 *s3β*c4*s1p2 − 13/256 *s3β*s1p2
		  dAdδ(8) = − 5/32* sβ*c1p4* s1p2 − 1/32 *s3β*c1p4* s1p2
		  dAdδ(9) =  − 45/32* sβ*c1p4* s1p2 + 135/32 *s3β*c1p4* s1p2
		  dAdδ(10) =  45/32* sβ*c1p2* s1p4 - 135/32 *s3β*c1p2* s1p4
		  dAdδ(11) =  5/32* sβ*c1p2* s1p4 + 1/32 *s3β*c1p2* s1p4
		  dAdδ(12) = 27/16* sβ*s1p6 + 9/16 *c2β*sβ*s1p6
		  dAdδ(13) = 45/16*cβ*sβ2*s2p3
		  dAdδ(14) = − 85/256* cβ*s2 − 1/128* cβ*c2β*s2 − 1/32 *cβ*c2β*c2*s2 − 3/128*cβ*c2β*c4*s2 − 11/64*cβ*s4 − 1/256*cβ*s6
		  dAdδ(15) =  45/256* cβ*s2 + 81/128* cβ*c2β*s2 + 27/32 *cβ*c2β*c2*s2 + 27/128*cβ*c2β*c4*s2 + 9/64*cβ*s4 + 9/256*cβ*s6
		  dAdδ(16) = 1/256 *cβ*c2β*s2 − 85/256*cβ*s2 + 11/64*cβ*s4 + 1/64*cβ*c2β*s4 − 1/256*cβ*s6 + 3/256*cβ*c2β*s6
		  dAdδ(17) = 135/256 *cβ*c2β*s2 + 45/256*cβ*s2 - 9/64*cβ*s4 + 27/64*cβ*c2β*s4 + 9/256*cβ*s6 + 27/256*cβ*c2β*s6
		  dAdδ(18) = 1/64*cβ*sβ2 *s2 + 5/64*cβ*sβ2*s6
		  dAdδ(19) = 25/32 *δ*c1p4 + 13/6* δ*c2β*c1p4 − 9/32 *δ*c4β*c1p4 + 5/16 *δ*c1p4 *c2 − 11/4* δ*c2β*c1p4* c2 + 7/16* δ*c4β*c1p4* c2 − 5/32* δ*c1p4* c4 − 1/8* δ*c2β*c1p4* c4 − 7/32* δ*c4β*c1p4* c4
		  dAdδ(20) = − 3*δ*sβ2* c1p8 − δ*c2β*sβ2* c1p8
		  dAdδ(21) = 16*δ*cβ3 *sβ*c1p7* s1
		  dAdδ(22) = − 5/4* δ*s2β*c1p5* s1 + 1/4* δ*c2β*s2β*c1p5 *s1 − δ*cβ2* s2β*c1p5* c2*s1
		  dAdδ(23) = − 5*δ*c1p6*s1p2 − 4*δ*c2β*c1p6*s1p2 − 7*δ*c4β*c1p6*s1p2
		  dAdδ(24) = −  3/4* δ*sβ2 *c1p6* s1p2 − 1/4 *δ*c2β*sβ2 *c1p6* s1p2 
		  dAdδ(25) = − 28*δ*c2β*s2β*c1p5 *s1p3 + 4*δ*s2β*c1p5 s1p3
		  dAdδ(26) = −  8/3 *δ*s2β*c1*s1p3 − 31/8 *δ*s2β*c1*c2*s1p3 − 1/8* δ*s2β*c1*c4*s1p3 + 19/32* δ*s4β*c1*s1p3 + 7/16 *δ*s4β*c3*s1p3 + 7/32 *δ*s4β*c5*s1p3
		  dAdδ(27) = 25/32* δ*s1p4 + 13/6* δ*c2β*s1p4 − 9/32 *δ*c4β*s1p4− 5/16 *δ*c2*s1p4 + 11/4 *δ*c2β*c2*s1p4− 7/16* δ*c4β*c2*s1p4 − 5/32 *δ*c4*s1p4− 1/8 *δ*c2β*c4*s1p4 − 7/32 *δ*c4β*c4*s1p4
		  dAdδ(28) = − 4*δ*s2β*c1p3* s1p5 + 27*δ*c2β*s2β*c1p3* s1p5
		  dAdδ(29) = 5/4 *δ*s2β*c1*s1p5 − 1/4* δ*c2β*s2β*c1*s1p5 − δ*cβ2*s2β*c1*c2*s1p5
		  dAdδ(30) = − 5*δ*c1p2* s1p6 − 4*δ*c2β*c1p2* s1p6 − 7*δ*c4β*c1p2* s1p6 
		  dAdδ(31) = −  3/4 *δ*sβ2* c1p2* s1p6 − 1/4 *δ*c2β*sβ2* c1p2* s1p6
		  dAdδ(32) = − 16*δ*cβ3 *sβ*c1*s1p7
		  dAdδ(33) = − 3*δ*sβ2* s1p8 − δ*c2β*sβ2* s1p8 
		  dAdδ(34) = − 25/64* δ*c2β*sβ2* s2p2 + 45/64 δ*sβ2* s2p2 − 25/64 *δ*sβ2*c4* s2p2− 35/64 *δ*c2β*sβ2*c4* s2p2
		  dAdδ(35) = − 25/8 *δ*sβ2* s2p4 − 35/8 *δ*c2β*sβ2 *s2p4
		  dAdδ(36) = 8/3 *δ*s2β*c1p3* s1 − 31/8* δ*s2β*c1p3* c2*s1 + 1/8 *δ*s2β*c1p3* c4*s1 − 19/32 *δ*s4β*c1p3* s1 + 7/16 *δ*s4β*s3*c1p3* s3 − 7/32* δ*s4β*c1p3* s5
		  dAdδ(37) = 0
		  dAdδ(38) = 0
		  dAdδ(39) = 0
		  dAdδ(40) = 0
		  dAdδ(41) = 0
		  dAdδ(42) = cβ*c1p2*χsxDN − sβ*c1p2*χszDN
		  dAdδ(43) = 2 *cβ*χsxDN − 1/2 *cβ*c2*χsxDN − sβ*s1p2*χszDN
		  dAdδ(44) = − cβ*s1p2*χsyDN
		  dAdδ(45) = − sβ*s2*χsyDN
		  dAdδ(46) = − cβ*c1p2*χsyDN
		  dAdδ(47) = 0
		  dAdδ(48) = 0
		  dAdδ(49) = 0
		  dAdδ(50) = 0
		  dAdδ(51) = 0
		  dAdδ(52) = 390625/131072 *sβ3 *c1p10 − 1171875/131072* δ^ 2 *sβ3 *c1p10 + 390625/1179648 *c2β^2* sβ3 *c1p10 − 1171875/1179648*δ^2* c2β^2 *sβ3 *c1p10 − 1171875/196608 *δ^2 *c2β*sβ3 *c1p10
		  dAdδ(53) = 19/16384*s3β*c1p2*c2- 57/16384*δ^2*s3β*c1p2*c2+ 35/49152*s5β*c1p2*c2- 105/49152*δ^2*s5β*c1p2*c2+ 1873/8192*sβ*c1p2*c2- 5619/8192*δ^2*sβ*c1p2*c2+ 1901/8192*s3β*c1p2*c2- 35/24576*s5β*c1p2*c2- 10675/12288*sβ*c1p2*c2- 47/32768*s3β*c1p2*c4+ 141/32768*δ^2*s3β*c1p2*c4- 21/32768*s5β*c1p2*c4+ 63/32768*δ^2*s5β*c1p2*c4- 2833/16384*s3β*c1p2*c4+ 21/16384*s5β*c1p2*c4+ 337/49152*sβ*c1p2*c4- 1011/49152*δ^2*sβ*c1p2*c4+ 1103/24576*sβ*c1p2*c4- 91/16384*s3β*c1p2*c6+ 273/16384*δ^2*s3β*c1p2*c6+ 7/16384*s5β*c1p2*c6- 21/16384*δ^2*s5β*c1p2*c6+ 91/8192*s3β*c1p2*c6- 7/8192*s5β*c1p2*c6+ 59*12288*sβ*c1p2*c6- 177/12288*δ^2*sβ*c1p2*c6+ 7/131072*s3β*c1p2*c8- 21/131072*δ^2*s3β*c1p2*c8- 35/131072*s5β*c1p2*c8+ 105/131072*δ^2*s5β*c1p2*c8+ 7/49152*sβ*c1p2*c8- 21/49152*δ^2*sβ*c1p2*c8- 7/65536*s3β*c1p2*c8+ 35*65536*s5β*c1p2*c8- 7/98304*sβ*c1p2*c8- 331/131072*s3β*c1p2- 993/131072*δ^2*s3β*c1p2- 155/393216*s5β*c1p2+ 465/393216*δ^2*s5β*c1p2- 7449/65536*sβ*c1p2+ 22347/65536*δ^2*sβ*c1p2- 9653*65536*s3β*c1p2+ 155/196608*s5β*c1p2+ 43723/98304*sβ*c1p2
		  dAdδ(54) = 6399/8192*s3β*c1p6*c2- 19197/8192*δ^2*s3β*c1p6*c2- 2187/8192*s5β*c1p6*c2+6561/8192*δ^2*s5β*c1p6*c2- 2403/8192*sβ*c1p6*c2+7209/8192*δ^2*sβ*c1p6*c2- 6399/4096*s3β*c1p6*c2+2187/4096*s5β*c1p6*c2+2403/2048*sβ*c1p6*c2+3159/32768*s3β*c1p6*c4- 9477/32768*δ^2*s3β*c1p6*c4+3645/32768*s5β*c1p6*c4- 10935/32768*δ^2*s5β*c1p6*c4+1701/32768*sβ*c1p6*c4- 5103/32768*δ^2*sβ*c1p6*c4- 3159/16384*s3β*c1p6*c4- 3645/16384*s5β*c1p6*c4- 1701/8192*sβ*c1p6*c4- 24507/32768*s3β*c1p6- 73521/32768*δ^2*s3β*c1p6+5751/32768*s5β*c1p6+17253/32768*δ^2*s5β*c1p6- 4689/16384*sβ*c1p6+14067/16384*δ^2*sβ*c1p6+38331/16384*s3β*c1p6- 5751/16384*s5β*c1p6+39249/8192*sβ*c1p6
		  dAdδ(55) = 11875/768* cβ*sβ2* c1p9* s1 + 3125 /768* c3β*sβ2 *c1p9* s1 − 11875/ 1536* cβ*sβ2 *c1p9 *s1 + 35625/1536 *δ^2 *cβ*sβ2 *c1p9* s1 − 3125/1536 *c3β*sβ2 *c1p9* s1 + 9375/1536 *δ^2* c3β*sβ2* c1p9* s1
		  dAdδ(56) = − 351/256*cβ*sβ2*c1p7*s1+ 243/256*cβ*c2β*sβ2*c1p7*s1− 567/256*cβ*sβ2*c2*c1p7*s1− 405/256*cβ*c2β*sβ2*c2*c1p7*s1+ 351/512*cβ*sβ2*c1p7*s1− 1053/512*δ^2*cβ*sβ2*c1p7*s1− 243/512*cβ*c2β*sβ2*c1p7*s1+ 729/512*δ^2*cβ*c2β*sβ2*c1p7*s1+ 567/512*cβ*sβ2*c2*c1p7*s1− 1701/512*δ^2*cβ*sβ2*c2*c1p7*s1+ 405/512*cβ*c2β*sβ2*c2*c1p7*s1− 1215/512*δ^2*cβ*c2β*sβ2*c2*c1p7*s1
		  dAdδ(57) = − 243 /512* sβ2* c1p8* s1p2 − 729/512* δ ^2 *sβ3 *c1p8* s1p2 − 81/ 512* c2β*sβ3* c1p8* s1p2 − 243/ 512* δ^ 2* c2β*sβ3 *c1p8* s1p2
		  dAdδ(58) = 1901/8192*s3β*c2*s1p2− 35/24576*s5β*c2*s1p2− 10675/12288*sβ*c2*s1p2+ 2833/16384*s3β*c4*s1p2− 21/16384*s5β*c4*s1p2− 1103/24576*sβ*c4*s1p2+ 91/8192*s3β*c6*s1p2− 7/8192*s5β*c6*s1p2+ 59/12288*sβ*c6*s1p2+ 7/65536*s3β*c8*s1p2− 35/65536*s5β*c8*s1p2+ 7/98304*sβ*c8*s1p2+ 9653/65536*s3β*s1p2− 155/196608*s5β*s1p2− 43723/98304*sβ*s1p2+ 19/16384*s3β*c2*s1p2− 57/16384*δ^2*s3β*c2*s1p2+ 35/49152*s5β*c2*s1p2− 105/49152*δ^2*s5β*c2*s1p2+ 1873/8192*sβ*c2*s1p2− 5619/8192*δ^2*sβ*c2*s1p2+ 47/32768*s3β*c4*s1p2− 141/32768*δ^2*s3β*c4*s1p2+ 21/32768*s5β*c4*s1p2− 63/32768*δ^2*s5β*c4*s1p2− 337/49152*sβ*c4*s1p2+ 1011/49152*δ^2*sβ*c4*s1p2− 91/16384*s3β*c6*s1p2+ 273/16384*δ^2*s3β*c6*s1p2+ 7/16384*s5β*c6*s1p2− 21/16384*δ^2*s5β*c6*s1p2− 59/24576*sβ*c6*s1p2+ 177/24576*δ^2*sβ*c6*s1p2− 7/131072*s3β*c8*s1p2+ 21/131072*δ^2*s3β*c8*s1p2+ 35/131072*s5β*c8*s1p2− 105/131072*δ^2*s5β*c8*s1p2− 7/49152*sβ*c8*s1p2+ 21/49152*δ^2*sβ*c8*s1p2+ 331/131072*s3β*s1p2− 993/131072*δ^2*s3β*s1p2+ 155/393216*s5β*s1p2− 465/393216*δ^2*s5β*s1p2+ 7449/65536*sβ*s1p2− 22347/65536*δ^2*sβ*s1p2
		  dAdδ(59) = 151/4096*s3β*c2*c1p4*s1p2−453/4096*δ^2*s3β*c2*c1p4*s1p2−3/4096*s5β*c2*c1p4*s1p2+9/4096*δ^2*s5β*c2*c1p4*s1p2−27/2048*sβ*c2*c1p4*s1p2+81/2048*δ^2*sβ*c2*c1p4*s1p2−151/2048*s3β*c2*c1p4*s1p2+3/2048*s5β*c2*c1p4*s1p2+27/1024*sβ*c2*c1p4*s1p2+13/16384*s3β*c4*c1p4*s1p2−39/16384*δ^2*s3β*c4*c1p4*s1p2+15/16384*s5β*c4*c1p4*s1p2−45/16384*δ^2*s5β*c4*c1p4*s1p2+7/8192*sβ*c4*c1p4*s1p2−21*8192*δ^2*sβ*c4*c1p4*s1p2−13/8192*s3β*c4*c1p4*s1p2−15/8192*s5β*c4*c1p4*s1p2−7/4096*sβ*c4*c1p4*s1p2−57/16384*s3β*c1p4*s1p2+171/16384*δ^2*s3β*c1p4*s1p2+13/16384*s5β*c1p4*s1p2−39/16384*δ^2*s5β*c1p4*s1p2+245/8192*sβ*c1p4*s1p2−735/8192*δ^2*sβ*c1p4*s1p2+825/8192*s3β*c1p4*s1p2−13/8192*s5β*c1p4*s1p2+1675/4096*sβ*c1p4*s1p2
		  dAdδ(60) = 4375/2048*sβ*c1p8*s1p2− 13125/2048*δ^2*sβ*c1p8*s1p2+ 8125/4096*s3β*c1p8*s1p2− 24375/4096*δ^2*s3β*c1p8*s1p2+ 9375/4096*s5β*c1p8*s1p2− 28125/4096*δ^2*s5β*c1p8*s1p2− 4375/1024*sβ*c1p8*s1p2− 8125/2048*s3β*c1p8*s1p2− 9375/2048*s5β*c1p8*s1p2
		  dAdδ(61) = 20475/4096*sβ*c1p4*s1p2−149391/8192*s3β*c1p4*s1p2+2187/1024*c1p4*sβc2*s1p2+10017/2048*s3β*c1p4*c2*s1p2−1701/2048*s5β*c1p4*c2*s1p2+7371*8192*s5β*c1p4*s1p2−567/4096*sβ*c1p4*c4*s1p2−1701/8192*s3β*c1p4*c4*s1p2+8505*8192*s5β*c1p4*c4*s1p2−3195/16384*sβ*c1p4*s1p2+9585/16384*δ^2*sβ*c1p4*s1p2+45711/16384*s3β*c1p4*s1p2−137133/16384*δ^2*s3β*c1p4*s1p2+567/8192*sβ*c1p4*c4*s1p2−1701/8192*δ^2*sβ*c1p4*c4*s1p2+1701/16384*s3β*c1p4*c4*s1p2−5103/16384*δ^2*s3β*c1p4*c4*s1p2−8505/16384*s5β*c1p4*c4*s1p2+25515/16384*δ^2*s5β*c1p4*c4*s1p2−7371/16384*s5β*c1p4*s1p2+22113/16384*δ^2*s5β*c1p4*s1p2−2187/2048*sβ*c1p4*c2*s1p2+6561/2048*δ^2*sβ*c1p4*c2*s1p2−10017/4096*s3β*c1p4*c2*s1p2+30051/4096*δ^2*s3β*c1p4*c2*s1p2+1701/4096*s5β*c1p4*c2*s1p2−5103/4096*δ^2*s5β*c1p4*c2*s1p2
		  dAdδ(62) = 4375/384*cβ*c1p7*s1p3+625/256*c3β*c1p7*s1p3+3125/256*c5β*c1p7*s1p3− 4375/768*cβ*c1p7*s1p3+13125/768*δ^2*cβ*c1p7*s1p3− 625/512*c3β*c1p7*s1p3+1875/512*δ^2*c3β*c1p7*s1p3− 3125/512*c5β*c1p7*s1p3+9375/512*δ^2*c5β*c1p7*s1p3
		  dAdδ(63) = 5/768*cβ*c2β*sβ2*c1p5*c2s1p3− 15/768*δ^2*cβ*c2β*sβ2*c1p5*c2s1p3− 5/384*c2β*cβ*c1p5*c2s1p3− 1/768*c2β*cβ*sβ2*c1p5*s1p3+ 3/768*δ^2*c2β*cβ*sβ2*c1p5*s1p3+ 1/384*c2β*cβ*sβ2*c1p5*s1p3+ 7/768*c2cβ*sβ2*c1p5*s1p3− 21/768*δ^2*c2cβ*sβ2*c1p5*s1p3− 7/384*cβ*sβ2*c1p5*c2s1p3+ 37/768*cβ*sβ2*c1p5*s1p3− 111/768*δ^2*cβ*sβ2*c1p5*s1p3− 37/384*cβ*sβ2*c1p5*s1p3
		  dAdδ(64) = 1/256 − 3/256*δ^2 + 1/768*c2β − 1/256*δ^2*c2β − 1/128*sβ^3*c1p6*s1p4 − 1/384*c2β*sβ^3*c1p6*s1p4
		  dAdδ(65) = 151/4096*s3β*c1p2*c2*s1p4− 453/4096*δ^2*s3β*c1p2*c2*s1p4− 3/4096*s5β*c1p2*c2*s1p4+9/4096*δ^2*s5β*c1p2*c2*s1p4− 27/2048*sβ*c1p2*c2*s1p4+81/2048*δ^2*sβ*c1p2*c2*s1p4− 151/2048*s3β*c1p2*c2+3/2048*s5β*c1p2*c2+27/1024*sβ*c1p2*c2− 13/16384*s3β*c1p2*c4*s1p4+39/16384*δ^2*s3β*c1p2*c4*s1p4− 15/16384*s5β*c1p2*c4*s1p4+45/16384*δ^2*s5β*c1p2*c4*s1p4− 7/8192*sβ*c1p2*c4*s1p4+21/8192*δ^2*sβ*c1p2*c4*s1p4+13/8192*s3β*c1p2*c4*s1p4+15/8192*s5β*c1p2*c4*s1p4+7/4096*sβ*c1p2*c4*s1p4+57/16384*s3β*c1p2*s1p4− 171/16384*δ^2*s3β*c1p2*s1p4− 13/16384*s5β*c1p2*s1p4+39/16384*δ^2*s5β*c1p2*s1p4− 245/8192*sβ*c1p2*s1p4+735/8192*δ^2*sβ*c1p2*s1p4− 825/8192*s3β*c1p2− 1675/4096*sβ*c1p2
		  dAdδ(66) = 4375/2048*s3β*c1p6*s1p4− 13125/2048*δ^2*s3β*c1p6*s1p4− 21875/2048*s5β*c1p6*s1p4+ 65625/2048*δ^2*s5β*c1p6*s1p4+ 4375/3072*sβ*c1p6*s1p4− 13125/3072*δ^2*sβ*c1p6*s1p4−4375/1024*s3β*c1p6*s1p4+ 21875/1024*s5β*c1p6*s1p4− 4375/1536*sβ*c1p6*s1p4
		  dAdδ(67) = − 10017/4096*s3β*c1p2*c2*s1p4+30051/4096*δ^2*s3β*c1p2*c2*s1p4+1701/4096*s5β*c1p2*c2*s1p4− 5103/4096*δ^2*s5β*c1p2*c2*s1p4− 2187/2048*sβ*c1p2*c2*s1p4+6561/2048*δ^2*sβ*c1p2*c2*s1p4+10017/2048*s3β*c1p2*c2*s1p4− 1701/2048*s5βc2*c1p2*s1p4+2187/1024*sβ*c1p2*c2*s1p4− 1701/16384*s3β*c1p2*c4*s1p4+5103/16384*δ^2*s3β*c1p2*c4*s1p4+8505/16384*s5β*c1p2*c4*s1p4− 25515/16384*δ^2*s5β*c1p2*c4*s1p4− 567/8192*sβ*c1p2*c4*s1p4+1701/8192*δ^2*sβ*c1p2*c4*s1p4+1701/8192*s3β*c1p2*c4*s1p4− 8505/8192*s5β*c1p2*c4*s1p4+567/4096*sβ*c1p2*c4*s1p4− 45711/16384*s3β*c1p2*s1p4+137133/16384*δ^2*s3β*c1p2*s1p4+7371/16384*s5β*c1p2*s1p4− 22113/16384*δ^2*s5β*c1p2*s1p4+3195/8192*sβ*c1p2*s1p4− 9585/8192*δ^2*sβ*c1p2*s1p4+149391/32768*s3β*c1p2*s1p4− 7371/32768*s5β*c1p2*s1p4− 20475/16384*sβ*c1p2*s1p4
		  dAdδ(69) = − 1/768*c2β*sβ^3*c1p4*s1p6 + 3/768*δ^2*c2β*sβ^3*c1p4*s1p6 − 1/256*sβ^3*c1p4*s1p6+3/256*δ^2*sβ^3*c1p4*s1p6+1/128*sβ^3*c1p4*s1p6+1/384*c2β*sβ^3*s1p6
		  dAdδ(70) = − 1053/512*c2β*sβ*c2*s1p6+ 1701/2048*c2β*sβ*c4*s1p6− 8145/2048*c2β*sβ*s1p6+ 2187/2048*c4β*sβ*c2*s1p6+ 297/2048*sβ2*c2c4*s1p6+ 3645/8192*c4β*sβ*c4*s1p6+ 5751/8192*c4β*sβ*s1p6+ 5103*/8192*sβ*c4*s1p6− 55539/8192*sβ*s1p6+ 1053/4096*c2β*sβ*c2*s1p6− 3159/4096*δ^2*c2β*sβ*c2*s1p6− 1701/16384*c2β*sβ*c4*s1p6+ 5103/16384*δ^2*c2β*sβ*c4*s1p6+ 4689/16384*c2β*sβ*s1p6− 14067/16384*δ^2*c2β*sβ*s1p6− 2187/16384*c4β*sβ*c2*s1p6+ 6561/16384*δ^2*c4β*sβ*c2*s1p6− 297/16384*sβ*c2*s1p6+ 891/16384*δ^2*sβ*c2*s1p6− 3645/65536*c4β*sβ*c4*s1p6+ 10935/65536*δ^2*c4β*sβ*c4*s1p6− 5751/65536*c4β*s1p6+ 17253/65536*δ^2*c4β*s1p6− 5103/65536*sβ*c4*s1p6+ 15309/65536*δ^2*sβ*c4*s1p6+ 14067/65536*sβ*s1p6− 42201/65536*δ^2*sβ*s1p6
		  dAdδ(71) = − 4375/2048*s3β*c1p4*s1p6+ 13125/2048*δ^2*s3β*c1p4*s1p6+ 21875/2048*s5β*c1p4*s1p6− 65625/2048*δ^2*s5β*c1p4*s1p6− 4375/3072*sβ*c1p4*s1p6+ 13125/3072*δ^2*sβ*c1p4*s1p6+ 4375/1024*s3β*c1p4*s1p6+ 21875/1024*s5β*c1p4*s1p6+ 4375/1536*sβ*c1p4*s1p6
		  dAdδ(72) = − 625/512*c3β*s1p7*c1p3+ 1875/512*δ^2*c3β*s1p7*c1p3+ 625/256*c3β*s1p7*c1p3− 3125/512*c5β*s1p7*c1p3+ 9375/512*δ^2*c5β*s1p7*c1p3+ 3125/256*c5β*s1p7*c1p3− 4375/768*cβ*s1p7*c1p3+ 13125/768*δ^2*cβ*s1p7*c1p3+ 4375/384*cβ*s1p7*c1p3
		  dAdδ(73) = − 405/512*c2βcβ*sβ2*c1*c2*s1p7+ 1215/512*δ^2*c2βcβ*sβ2*c1*c2*s1p7− 243/512*c2βcβ*sβ2*c1*s1p7+ 729/512*δ^2*c2βcβ*sβ2*c1*s1p7− 567/512*cβ*sβ2*c1*c2*s1p7+ 1701/512*δ^2*cβ*sβ2*c1*c2*s1p7+ 351/512*cβ*sβ2*c1*s1p7− 1053/512*δ^2*cβ*sβ2*c1*s1p7+ 405/256*c2β*sβ2*cβc1*c2*s1p7+ 243/256*c2βcβ*sβ2*c1*s1p7+ 567/256*cβ*sβ2*c1*c2*s1p7− 351/256*cβ*sβ2*c1*s1p7
		  dAdδ(74) = − 243/512*sβ^3*s1p8*c1p2 + 729/512 *δ^2 *sβ^3*s1p8*c1p2 − 81/512 *c2β*sβ^3*s1p8*c1p2 + 243/512 *δ^2 *c2β*sβ^3*s1p8*c1p2 + 243/256 *sβ^3*s1p8*c1p2 − 81/256 *c2β*sβ^3*s1p8*c1p2
		  dAdδ(75) = − 8125/4096*s3β*c1p2*s1p8+  24375/4096*δ^2*s3β*c1p2*s1p8− 9375/4096*s5β*c1p2*s1p8+  28125/4096*δ^2*s5β*c1p2*s1p8− 4375/2048*sβ*c1p2*s1p8+  13125/2048*δ^2*sβ*c1p2*s1p8+  8125/2048*s3β*c1p2*s1p8+  9375/2048*s5β*c1p2*s1p8+  4375/1024*sβ*c1p2*s1p8
		  dAdδ(76) = − 3125/1536*c3β*sβ2*c1*s1p9+ 9375/1536*δ^2*c3β*sβ2*c1*s1p9+ 3125/768*c3β*sβ2*c1*s1p9− 11875/1536*cβ*sβ2*c1*s1p9+ 35625/1536*δ^2*cβ*sβ2*c1*s1p9+ 11875/768*cβ*sβ2*c1*s1p9
		  dAdδ(77) = 625/1536 *c2β*sβ^3*s1p10 + 1875/1536 *δ^2*c2β*sβ^3*s1p10 + 625/768 *c2β*sβ^3*s1p10 − 625/512 *sβ^3*s1p10 + 1875/512 δ^2 *sβ^3*s1p10 + 625/256 *sβ^3*s1p10
		  dAdδ(78) = 5103/8192*c2βcβ*sβ2*c4*s2p3+ 15309/8192*δ^2*c2βcβ*sβ2*c4*s2p3+ 5103/4096*c2βcβ*sβ2*c4*s2p3− 3969/8192*c2βcβ*sβ2*s2p3+ 11907/8192*δ^2*c2βcβ*sβ2*s2p3+ 3969/4096*c2βcβ*sβ2*s2p3− 1701/8192*cβ*sβ2*c4*s2p3+ 5103/8192*δ^2*cβ*sβ2*c4*s2p3+ 1701/4096*cβ*sβ2*c4*s2p3+ 10197/8192*cβ*sβ2*s2p3− 30591/8192*δ*2*cβ*sβ2*s2p3− 44757/8192*cβ*sβ2*s2p3
		  dAdδ(79) = 13125/8192*c3β*sβ2*s2p5 + 39375/8192 *δ^2 *c3β*sβ2*s2p5 + 13125/4096 *c3β*sβ2*s2p5 − 21875/8192 *cβ*sβ2*s2p5 + 65625/8192* δ^2* cβ*sβ2*s2p5 + 21875/4096 *cβ*sβ2*s2p5
		  dAdδ(80) = 243/2048*c2βcβc2s2- 729*2048*δ^2*c2βcβc2s2- 2835/1024*c2βcβc2s2- 5319/16384*c2βcβc4s2+15957/16384*δ^2*c2βcβc4s2+135/8192*c2βcβc4s2+5967/32768*c2βcβs2- 17901/32768*δ^2*c2βcβs2- 37071/16384*c2βcβs2+4005/16384*cβc2s2- 12015/16384*δ^2*cβc2s2- 10917/8192*cβc2s2- 243/262144*c3βs10+729/262144*δ^2*c3βs10- 2565*65536*c3βs8+7695/65536*δ^2*c3βs8+243/131072*c3βs10+2565/32768*c3βs8- 567/65536*c4βcβs2+1701/65536*δ^2*c4βcβs2+81/16384*c4βcβs4- 243/16384*δ^2*c4βcβs4- 1053/131072*c4βcβs6+3159/131072*δ^2*c4βcβs6+567/32768*c4βcβs2- 81/8192*c4βcβs4+1053/65536*c4βcβs6+6633/32768*cβc4s2- 19899/32768*δ^2*cβc4s2- 10089/16384*cβc4s2- 1215/262144*c5βs10+3645/262144*δ^2*c5βs10- 729/65536*c5βs8+2187/65536*δ^2*c5βs8+1215/131072*c5βs10+729/32768*c5βs8- 513/16384*cβc6s2+1539/16384*δ^2*cβc6s2+513*8192*cβc6s2- 567*65536*cβc8s2+1701*65536*δ^2*cβc8s2+567/32768*cβc8s2+2457/16384*cβs2- 7371/16384*δ^2*cβs2- 7641/8192*cβs2
		  dAdδ(81) = 5643/16384*c2β*cβ*s6- 16929/16384*δ^2*c2β*cβ*s6- 18603/8192*c2β*cβ*s2- 2835/2048*c2β*cβ*s4- 135/65536*c2β*cβ*s6- 405/65536*δ^2*c2β*cβ*s6- 243/262144*c3β*s10- 729/262144*δ^2*c3β*s10- 2565/65536*c3β*s8- 7695/65536*δ^2*c3β*s8- 243/131072*c3β*s10- 2565/131072*δ^2*c3β*s10- 2565/32768*c3β*s8- 567/65536*c4β*cβ*s6- 1701/65536*δ^2*c4β*cβ*s6- 567/32768*c4β*cβ*s2- 81/8192*c4β*cβ*s4- 1053/65536*c4β*cβ*s6- 567/65536*δ^2*c4β*cβ*s6- 1215/262144*c5β*s10- 3645/262144*δ^2*c5β*s10- 729/65536*c5β*s8- 2187/65536*δ^2*c5β*s8- 1215/131072*c5β*s10- 729/131072*δ^2*c5β*s10- 729/32768*c5β*s8-  567/131072*cβ*s10- 1701/131072*δ^2*cβ*s10- 3195/65536*cβ*s6- 9585/65536*δ^2*cβ*s6- 513/32768*cβ*s8- 1539/32768*δ^2*cβ*s8- 567/65536*cβ*s10- 20475/32768*cβ*s2- 5715/8192*cβ*s4- 20745/65536*cβ*s6- 513/16384*cβ*s8
		  dAdδ(82) = 11/3072*c2β*cβ*c2s2- 33/3072*δ^2*c2β*cβ*c2s2+ 133/1536*c2β*cβ*c2s2+ 77/8192*c2β*cβ*c4s2- 231/8192*δ^2*c2β*cβ*c4s2+ 211/4096*c2β*cβ*c4s2+ 257/49152*c2β*cβ*s2- 771/49152*δ^2*c2β*cβ*s2+ 319/24576*c2β*cβ*s2+ 933/4096*c2cβ*s2- 1391/24576*δ^2*c2s2- 1391/6144*c2s2+ 1/131072*c3β*s10- 3/131072*δ^2*c3β*s10+ 45/32768*c3β*s8- 135/32768*δ^2*c3β*s8- 1/65536*c3β*s10- 45/16384*c3β*s8- 1/98304*c4β*cβ*s2+ 3/98304*δ^2*c4β*cβ*s2+ 1/6144*c4β*cβ*s4- 3/6144*δ^2*c4β*cβ*s4- 1/65536*c4β*cβ*s6+ 3/65536*δ^2*c4β*cβ*s6+ 1/49152*c4β*cβ*s2- 1/12288*c4β*cβ*s4+ 1/32768*c4β*cβ*s6- 49/49152*c4cβ*s2+ 147/49152*δ^2*c4cβ*s2+ 625/24576*cβ*c4s2+ 5/131072*c5β*s10- 15/131072*δ^2*c5β*s10+ 1/32768*c5β*s8- 3*32768*δ^2*c5β*s8- 5/65536*c5β*s10- 1/16384*c5β*s8+ 11/24576*cβ*c6s2- 33/24576*δ^2*cβ*c6s2- 11/12288*cβ*c6s2+ 7/98304*cβ*c8s2- 21/98304*δ^2*cβ*c8s2- 7/49152*cβ*c8s2- 1493/24576*cβ*s2+ 4479/24576*δ^2*cβ*s2+ 871/4096*cβ*s2
		  dAdδ(83) =  13/24576*c2β*cβ*s2- 39/24576*δ^2*c2β*cβ*s2- 11/6144*c2β*cβ*s4+ 33/6144*δ^2*c2β*cβ*s4+ 77/16384*c2β*cβ*s6- 231/16384*δ^2*c2β*cβ*s6- 157/49152*c2β*cβ*s2- 133/12288*c2β*cβ*s4+ 211/32768*c2β*cβ*s6+ 1/131072*c3β*s10- 3/131072*δ^2*c3β*s10- 45/32768*c3β*s8+ 135*32768*δ^2*c3β*s8- 1/65536*c3β*s10+ 45*16384*c3β*s8- 1/98304*c4β*cβ*s2+ 3/98304*δ^2*c4β*cβ*s2- 1/6144*c4β*cβ*s4+ 3/6144*δ^2*c4β*cβ*s4- 1/65536*c4β*cβ*s6+ 3/65536*δ^2*c4β*cβ*s6+ 1/49152*c4β*cβ*s2+ 1/12288*c4β*cβ*s4+ 1/32768*c4β*cβ*s6+ 5/131072*c5β*s10- 15/131072*δ^2*c5β*s10- 1/32768*c5β*s8+ 3/32768*δ^2*c5β*s8- 5/65536*c5β*s10+ 1/16384*c5β*s8+ 7/98304*cβ*s10- 21/98304*δ^2*cβ*s10- 5923/98304*cβ*s2+ 17769/98304*δ^2*cβ*s2+ 701/24576*cβ*s4- 2103/24576*δ^2*cβ*s4- 35/65536*cβ*s6+ 105/65536*δ^2*cβ*s6- 11/49152*cβ*s8+ 33/49152*δ^2*cβ*s8- 7/98304*cβ*s10+ 9827/196608*cβ*s2- 1405/49152*cβ*s4+ 419/131072*cβ*s6+ 11/24576*cβ*s8
		  dAdδ(84) =  - 429/32768*cβ*sβ2*s6+ 1287/32768*δ^2*cβ*sβ2*s6- 1/16384*c2β*cβ*sβ2*s2+ 3/16384*δ^2*c2β*cβ*sβ2*s2+ 7/32768*c2β*cβ*sβ2*s2+ 1/8192*c2β*cβ*sβ2*s2- 21/65536*c3β*sβ2*s10+ 63/65536*δ*2*c3β*sβ2*s10+ 21/32768*c3β*sβ2*s10- 35/65536*cβ*sβ2*s10+ 105/65536*δ^2*cβ*sβ2*s10- 43/16384*cβ*sβ2*s2+ 129/16384*δ^2*cβ*sβ2*s2+ 35/32768*cβ*sβ2*s10- 341/32768*cβ*sβ2*s2+ 341/16384*δ^2*cβ*sβ2*s2- 3411/65536*cβ*sβ2*s6+ 10233/65536*δ^2*cβ*sβ2*s6
		  dAdδ(85) = 1/2* δ*cβ* sβ *c2p3*χsx
		  dAdδ(86) = − 1/12* δ*c2β *c1p4*c2*χsz + 7/4* δ*c2β *c1p4*χsz + 1/6* δ*s2β *c1p4*c2*χsx − 1/4* δ*c1p4*c2*χsz+ 19/8 *δ*s2β *c1p4*χsx + 5/4* δ*c1p4*χsz
		  dAdδ(87) = − 1/4* δ*c1p5*s1*χsx − 1/12 *δ*c2β* c1p5*s1*χsx
		  dAdδ(88) = − 1/4 *δ*c1*s1p5*χsx − 1/12* δ*c2β* c1*s1p5*χsx
		  dAdδ(89) = − 7/24* δ*c2β *s1*c1p3*c2*χsx − 79/24* δ*c2β *s1*c1p3*χsx + 1/8 *δ*s1*c1p3*c2*χsx − 1/3 *δ*s2β *s1*c1p3*c2*χsz+ 17/8* δ*s1*c1p3*χsx + 7/2 *δ*s2β *s1*c1p3*χsz
		  dAdδ(90) =  7/48∗c2β∗c2∗c1p3∗s1∗χsxDN − 7/16∗δ^2∗c2β∗c2∗c1p3∗s1∗χsxDN +35/6∗c2β∗c2∗c1p3∗s1∗χsxDN +79/48∗c2β∗c2∗c1p3∗s1∗χsx − 237/48∗δ^2∗c2β∗c1∗s1p3∗χsx − 13/6∗c2β∗c1∗s1p3∗χsx DN−1/16∗c2β∗c1∗s1p3∗χsxDN +3/16∗δ^2∗c2∗c1p3∗s1∗χsxDN −5/2∗c2β∗c1∗s1p3∗χsxDN +1/6∗s2β∗c2∗c1p3∗s1∗χszDN − 1/2∗δ^2∗s2β∗c2∗c1p3∗s1∗χszDN +20/3∗s2β∗c2∗c1p3∗s1∗χszDN − 17/16∗c1p3∗s1∗χsxDN+51/16∗δ^2∗c1p3∗s1∗χsxDN+3/2∗c1p3∗s1∗χsxDN− 7∗s2β∗c1p3∗s1∗χszDN +21∗δ^2∗s2β∗c1∗s1p3∗χszDN − 2∗s2β∗c1∗s1p3∗χszDN
		  dAdδ(91) = − 1/12* δ*c2β *c2*s1p4*χsz − 7/4 *δ*c2β *s1p4*χsz + 1/6 *δ*s2β *c2*s1p4*χsx − 1/4 *δ*c2*s1p4*χsz− 19/12 *δ*s2β* s1p4*χsx − 5/4* δ*s1p4*χsz
		  dAdδ(92) = − 3/4 *δ*s2β *c2*s2p2*χsz
		  dAdδ(93) =  3/16 *δ*s2p3*χsx + 1/16 *δ*c2β *s2p3*χsx
		  dAdδ(94) = − 1/6 *δ*cβ* sβ *c2*s2p2*χsx − 1/4 *δ*sβ2 *c2*s2p2*χsz
		  dAdδ(95) = 3/8* δ*c2*s2p2*χsz + 1/8 *δ*c2β *c2*s2p2*χsz − 1/4 *δ*s2β* c2*s2p2*χsx
		  dAdδ(96) = − 11/64* δ*c2β *s2*χsx − 3/16 *δ*s2p3*χsx − 7/64* δ*c2β *s6*χsx + 1/8* δ*s2β *s2*χsz − 1/8* δ*s2β *s6*χsz
		  dAdδ(97) = 15/32* δ*s2*χsy − 3/32* δ*c2β *s2*χsy + 9/32* δ*c4*s2*χsy − 5/32* δ*c2β *c4*s2*χsy
		  dAdδ(98) = − 1/2* δ*cβ *sβ *c2*s2p2*χsy
		  dAdδ(99) = 3/16* δ*s2p3*χsy + 1/16 *δ*c2β *s2p3*χsy
		  dAdδ(100) = 5/24* δ*c2β *c2*c1*s1p3*χsy + 31/24* δ*c2β* c1*s1p3*χsy − 3/8* δ*c2*c1*s1p3*χsy− 1/8* δ*c1*s1p3*χsy
		  dAdδ(101) = 5/12 *δ*s2β* s1p4*χsy + 1/6* δ*s2β *c2*s1p4*χsy
		  dAdδ(102) = − 1/4 *δ*c1*s1p5*χsy − 1/12* δ*c2β *c1*s1p5*χsy
		  dAdδ(103) = 11/12* δ*cβ *sβ* s2p2*χsy
		  dAdδ(104) = − 1/8 *δ*c1p3*s1*χsy + 31/24* δ*c2β* c1p3*s1*χsy + 3/8* δ*c1p3*c2*s1*χsy − 5/24* δ*c2β* c1p3*c2*s1*χsy
		  dAdδ(105) = − 5/12* δ*s2β* c1p4*χsy + 1/6 *δ*s2β* c1p4*c2*χsy
		  dAdδ(106) = − 1/4 *δ*c1p5*s1*χsy − 1/12*δ*c2β* c1p5*s1*χsy
		  dAdδ(107) = 2*sβ *cβ* c2p3*χax
		  dAdδ(108) = −3*c1p4*χaz − c2β *c1p4*χaz + 5*c1p4*c2*χaz + 5/3 *c2β* c1p4*c2*χaz + 7/3 *s2β* c1p4*χax − 10/3 *s2β* c1p4*c2*χax
		  dAdδ(109) = 5*c1p5*s1*χax + 5/3* c2β* c1p5*s1*χax
		  dAdδ(110) = 5*c1*s1p5*χax + 5/3 *c2β *c1*s1p5*χax
		  dAdδ(111) = 35/6 *c2β* c2*c1p3*s1*χax − 13/6 *c2β *c1p3*s1*χax − 5/2 *c2*c1p3*s1*χax + 20/3 *s2β *c2*c1p3*s1*χaz+ 3/2 *c1p3*s1*χax − 2*s2β *c1p3*s1*χaz
		  dAdδ(112) = − 35/6 *c2β *c2*c1*s1p3*χax − 13/6 *c2β* c1*s1p3*χax + 5/2* c2*c1*s1p3*χax − 20/3 *s2β *c2*c1*s1p3*χaz+ 3/2 *c1*s1p3*χax − 2*s2β *c1*s1p3*χaz
		  dAdδ(113) = 5/3 *c2β* c2*s1p4*χaz + *c2β* s1p4*χaz − 10/3 *s2β *c2*s1p4*χax + 5*c2*s1p4*χaz− 7/3 *s2β *s1p4*χax + 3*s1p4*χaz
		  dAdδ(114) = − 3*sβ2 *c2*s2p2*χaz
		  dAdδ(115) = 3/4 *s2p3*χax + 1/4 *c2β* s2p3*χax
		  dAdδ(116) = 10/3 *cβ* sβ* c2*s2p2*χax + 5*sβ2* c2*s2p2*χaz
		  dAdδ(117) = 3/2 *c2*s2p2*χaz + 1/2 *c2β *c2*s2p2*χaz − s2β *c2*s2p2*χax
		  dAdδ(118) = − 11/16 *c2β* s2*χax − 7/16 *c2β *s6*χax − 3/4 *s2p3*χax + 1/2 *s2β* s2*χaz − 1/2 *s2β *s6*χaz
		  dAdδ(119) = − 5/8 *c2β* c4*s2*χay − 3/8 *c2β *s2*χay + 9/8 *c4*s2*χay + 15/8 *s2*χay
		  dAdδ(120) = − 2*cβ *sβ *c2*s2p2*χay
		  dAdδ(121) = 3/4 *s2p3*χay + 1/4 *c2β* s2p3*χay
		  dAdδ(122) = − 25/6 *c2β *c2*c1*s1p3*χay − 11/6* c2β *c1*s1p3*χay + 15/2 *c2*c1*s1p3*χay + 5/2 *c1*s1p3*χay
		  dAdδ(123) = − 7/3 *s2β *s1p4*χay − 10/3 *s2β *s1p4*c2*χay
		  dAdδ(124) = 5*c1*s1p5*χay + 5/3 *c2β *c1*s1p5*χay
		  dAdδ(125) = − 1/3 *cβ *sβ *s2p2*χay
		  dAdδ(126) = 25/6 *c2β *c2*c1p3*s1*χay − 11/6* c2β* c1p3*s1*χay − 15/2 *c2*c1p3*s1*χay + 5/2 *c1p3*s1*χay
		  dAdδ(127) = 7/3 *s2β* c1p4*χay − 10/3 *s2β* c1p4*c2*χay
		  dAdδ(128) = 5*c1p5*s1*χay + 5/3* c2β* c1p5*s1*χay
		  dAdδ(129) = 0
		  dAdδ(130) = 0
		  dAdδ(131) = 0
		  dAdδ(132) = 0
		  dAdδ(133) = − 45/8* s2β* c1p2*s1p4
		  dAdδ(134) = 9/2 *c2β* c1*s1p5
		  dAdδ(135) = 9/8 *s2β *s1p6
		  dAdδ(136) = − 1/64 *cβ *sβ + 43/128 *cβ *sβ* c2 − 23/128 *s2β *c4 + 5/256* s2β *c6
		  dAdδ(137) = 1/4 *c2β *c2*c1*s1p3 − 1/4* c2β *c1*s1p3 − c1*s1p3
		  dAdδ(138) = 1/8 *s2β *c1p2*s1p4
		  dAdδ(139) = 1/2* sβ2*s4
		  dAdδ(140) = 1/64* cβ *sβ + 43/128 *cβ *sβ *c2 + 23/128 *s2β *c4 + 5/256 *s2β* c6
		  dAdδ(141) = 1/4 *c2β *c2*c1p3*s1 − 1/4* c2β* c1p3*s1 − c1p3*s1
		  dAdδ(142) = − 1/8* s2β *c1p4*s1p2
		  dAdδ(143) = 45/8* s2β* c1p4*s1p2
		  dAdδ(144) = 9/2 *c2β *c1p5*s1
		  dAdδ(145) = − 9/8 *s2β *c1p6
		  dAdδ(146) = 14*δ*s3β* c1p3*s1p5 + 6*δ*sβ* c1p3*s1p5
		  dAdδ(147) = − 28/3* δ*c3β* c1p2*s1p6 − 4/3 *δ*cβ *c1p2*s1p6
		  dAdδ(148) = − 6*δ*s3β* c1*s1p7 + 2*δ*sβ *c1*s1p7
		  dAdδ(149) = − 4*δ*cβ* sβ *s1p8
		  dAdδ(150) = − 19/8* δ*s3β *c2*c1*s1p3 + 9/8* δ*sβ *c2*c1*s1p3 + 7/16* δ*s3β *c4*c1*s1p3 + 3/16 *δ*sβ *c4*c1*s1p3− 9/16 *δ*s3β *c1*s1p3 − 103/48* δ*sβ *c1*s1p3
		  dAdδ(151) = 1/4 *δ*c3β* c2*s1p4 + 7/4* δ*cβ* c2*s1p4 − 7/16 *δ*c3β *c4*s1p4 + 3/16 *δ*c3β* s1p4− 1/32* δ*cβ *c4*s1p4 + 119/48 *δ*cβ *s1p4
		  dAdδ(152) = − 3/2 *δ*c2β *sβ *c1*c2*s1p5 − 1/2* δ*sβ *c1*c2*s1p5 + 2*δ*sβ *c1*s1p5
		  dAdδ(153) = −δ*cβ* sβ2 *c1p2*s6
		  dAdδ(154) = − 15/4 *δ*cβ *sβ2 *c2*s2p2
		  dAdδ(155) = − 19/8 *δ*s3β *c2*c1p3*s1 + 9/8* δ*sβ *c2*c1p3*s1 − 7/16* δ*s3β* c4*c1p3*s1 − 3/16* δ*sβ *c4*c1p3*s1+ 9/16 *δ*s3β* c1p3*s1 + 103/48 *δ*sβ* c1p3*s1
		  dAdδ(156) = − 1/4 *δ*c3β *c2*c1p4 − 7/4 *δ*cβ *c2*c1p4 − 7/16* δ*c3β *c4*c1p4 + 3/16 *δ*c3β *c1p4− 1/16 *δ*cβ *c4*c1p4 + 119/48 *δ*cβ *c4
		  dAdδ(157) = − 3/2 *δ*c2β *sβ *c1p5*c2*s1 − 1/2* δ*sβ *s1*c1p5*c2 − 2*δ*sβ* c1p5*s1
		  dAdδ(158) = −δ*cβ *sβ2 *c1p6*s2
		  dAdδ(159) = −14*δ*s3β* c1p5*s1p3 − 6*δ*sβ *c1p5*s1p3
		  dAdδ(160) = −14*δ*c3β *c1p6*s1p2 − 2*δ*cβ *c1p6*s1p2
		  dAdδ(161) = 12*δ*c2β *sβ *c1p7*s1 + 4*δ*sβ* c1p7*s1
		  dAdδ(162) = −4*δ*cβ *sβ2 *c1p8
		  dAdδ(163) = 0
		  dAdδ(164) = 0
		  dAdδ(165) = 0
		  dAdδ(166) = 0
		  dAdδ(167) = 0
		  dAdδ(168) = 1/2 *χsy + 1/2 *c2*χsy
		  dAdδ(169) = s1p2*χsy
		  dAdδ(170) = − 1/2 *cβ2* c2*χsx + 1/2 *cβ2* χsx + 1/2* cβ *sβ *c2*χsz − 1/2* cβ* sβ *χsz
		  dAdδ(171) = cβ *sβ* s2*χsx − sβ2* s2*χsz
		  dAdδ(172) = 1/2 *cβ2 *c2*χsx + 1/2* cβ2 *χsx − 1/2 *cβ *sβ *c2*χsz − 1/2 *cβ *sβ *χsz
		  dAdδ(173) = 0
		  dAdδ(174) = 0
		  dAdδ(175) = 0
		  dAdδ(176) = 0
		  dAdδ(177) = 4375/768 *s2β *s1p6*c1p4 − 13125/768 *δ^2*s2β *s1p6*c1p4 + 4375/512 *s4β *s1p6*c1p4 − 13125/512 *δ^2*s4β *s1p6*c1p4− 4375/384 *s2β* s1p6*c1p4 − 4375/256 *s4β *s1p6*c1p4
		  dAdδ(178) = − 625/192 *c2β *s1p7*c1p3 + 1875/192* δ^2*c2β *s1p7*c1p3 + 625/96 *c2β *s1p7*c1p3 − 625/64* c4β *s1p7*c1p3+ 1875/64* δ^2*c4β* s1p7*c1p3 + 625/32* c4β* s1p7*c1p3
		  dAdδ(179) = 625/512* s2β* s1p8*c1p2 − 1875/512* δ^2*s2β* s1p8*c1p2 − 5625/1024 *s4β *s1p8*c1p2 + 16875/1024 *δ^2*s4β* s1p8*c1p2− 625/256 *s2β *s1p8*c1p2 + 5625/512 *s4β* s1p8*c1p2
		  dAdδ(180) = − 625/96* c2β *sβ2* s1p9*c1 + 1875/96 *δ^2*c2β *sβ2* s1p9*c1 + 625/48 *c2β *sβ2* s1p9*c1 − 625/192 *sβ2* s1p9*c1
		  + 1875/192 *δ^2*sβ2* s1p9*c1 − 625/48* sβ2* s1p9*c1 + 625/96 *sβ2 *s1p9*c1
		  dAdδ(181) = 625/768* cβ* sβ3* s1p10 − 1875/768* δ^2*cβ *sβ3* s1p10 − 625/96* cβ *sβ3* s1p10
		  dAdδ(182) = 459/512*s2β*c2*c1p2*s1p4− 1377/512*δ^2*s2β*c2*c1p2*s1p4− 2079/1024*s4β*c2*c1p2*s1p4+ 6237/1024*δ^2*s4β*c2*c1p2*s1p4− 459/256*s2β*c2*c1p2*s1p4+ 2079/512*s4β*c2*c1p2*s1p4+ 567/2048*s2β*c4*c1p2*s1p4− 1701/2048*δ^2*s2β*c4*c1p2*s1p4+ 1701/4096*s4β*c4*c1p2*s1p4− 5103/4096*δ^2*s4β*c4*c1p2*s1p4− 567/1024*s2β*c4*c1p2*s1p4− 1701/2048*s4β*c4*c1p2*s1p4− 4923/2048*s2β*c1p2*s1p4+ 14769/2048*δ^2*s2β*c1p2*s1p4− 945/4096*s4β*c1p2*s1p4+ 2835/4096*δ^2*s4β*c1p2*s1p4+ 22203/4096*s2β*c1p2*s1p4− 66609/4096*δ^2*s2β*c1p2*s1p4+ 945/8192*s4β*c1p2*s1p4
		  dAdδ(183) = 27/64*c2β*c2*c1*s1p5− 81/64*δ^2*c2β*c2*c1*s1p5− 27/32*c2β*c2*c1*s1p5− 81/512*c2β*c4*c1*s1p5+ 243/512*δ^2*c2β*c4*c1*s1p5+ 81/256*c2β*c4*c1*s1p5+ 1233/512*c2β*c1*s1p5− 3699/512*δ^2*c2β*c1*s1p5− 4689/1024*c2β*c1*s1p5+ 27/64*c4β*c2*c1*s1p5− 81/64*δ^2*c4β*c2*c1*s1p5− 27/32*c4β*c2*c1*s1p5+ 27/32*c2*c1*s1p5− 81/32*δ^2*c2*c1*s1p5− 27/16*c2*c1*s1p5− 243/512*c4β*c4*c1*s1p5+ 729/512*δ^2*c4β*c4*c1*s1p5+ 243/256*c4β*c4*c1*s1p5+ 27/512*c4β*c1*s1p5− 81/512*δ^2*c4β*c1*s1p5− 27/256*c4β*c1*s1p5+ 27/64*c1*s1p5− 81/64*δ^2*c1*s1p5− 27*32*c1*s1p5
		  dAdδ(184) = − 621/1024*c2β*s2β*c2*s1p6+ 1863/1024*δ^2*c2β*s2β*c2*s1p6+ 621/512*c2β*s2β*c2*s1p6− 2187/4096*c2β*s2β*c4*s1p6+ 6561/4096*δ^2*c2β*s2β*c4*s1p6+ 2187/2048*c2β*s2β*c4*s1p6− 1377/4096*c2β*s2β*s1p6+ 4131/4096*δ^2*c2β*s2β*s1p6+ 1377/2048*c2β*s2β*s1p6+ 837*1024*c2*s2β*s1p6− 2511/1024*δ^2*c2*s2β*s1p6− 837/512*c2*s2β*s1p6+ 243/4096*s2β*s1p6*c4− 729/4096*δ^2*s2β*s1p6*c4− 243/2048*s2β*s1p6*c4+ 4761/4096*s2β*s1p6− 14283/4096*δ^2*s2β*s1p6− 11673/2048*s2β*s1p6
		  dAdδ(185) = − 81/64* sβ2 *c2β *c2*c1*s1p7 + 243/64* δ^2*sβ2 *c2β *c2*c1*s1p7 + 81/32 *sβ2 *c2β *c2*c1*s1p7− 27/64 *sβ2 *c2β *c1*s1p7 + 81/64* δ^2*sβ2 *c2β* c1*s1p7 + 27/32 *sβ2 *c2β* c1*s1p7 − 81/128* sβ2 *c2*c1*s1p7+ 243/128 *δ^2*sβ2 *c2*c1*s1p7 + 81/64 *sβ2 *c2*c1*s1p7 + 81/128 *sβ2 *c1*s1p7 − 243/128 *δ^2*sβ2 *c1*s1p7− 81/64 *sβ2 *c1*s7
		  dAdδ(186) = 81/256* cβ *sβ3* c1p2*s1p8 − 243/256* δ^2*cβ *sβ3* c1p2*s1p8 − 81/32* cβ *sβ3* c1p2*s1p8
		  dAdδ(187) = − 7/196608*s2β*c10+ 21/196608*δ^2*s2β*c10− 7/131072*s4β*c10+ 21/131072*δ^2*s4β*c10+ 7/98304*s2β*c10+ 7/65536*s4β*c10+ 11/24576*s2β*c2β*c4− 33/24576*δ^2*s2β*c2β*c4− 11/12288*s2β*c2β*c4− 91/65536*s2β*c2β*c6+ 273/65536*δ^2*s2β*c2β*c6+ 91/32768*s2β*c2β*c6− 173/98304*cβ*s3β*c2+ 519/98304*δ^2*cβ*s3β*c2+ 6031/98304*cβ*sβ*c2− 18093/98304*δ^2*cβ*sβ*c2+ 173/49152*cβ*s3β*c2− 10511/49152*cβ*sβ*c2− 679/24576*s2β*c4+ 2037/24576*δ^2*s2β*c4+ 557/4096*s2β*c4− 201/65536*s2β*c6+ 603/65536*δ^2*s2β*c6− 1719/32768*s2β*c6− 37/98304*s2β*c8+ 111/98304*δ^2*s2β*c8+ 91/65536*s4β*c8+ 37/49152*s2β*c8− 91/32768*s4β*c8+ 1/32768*s3β*cβ− 3/32768*δ^2*s3β*cβ+ 85/32768*sβ*cβ+ 255/32768*δ^2*sβ*cβ− 1/16384*s3β*cβ+ 683/16384*sβ*cβ
		  dAdδ(188) = − 37/1024*c2β*c2*c1*s1p3+ 111/1024*δ^2*c2β*c2*c1*s1p3− 347/2048*c2β*c2*c1*s1p3+ 3/512*c2β*c4*c1*s1p3− 9/512*δ^2*c2β*c4*c1*s1p3− 3/256*c2β*c4*c1*s1p3− 1/3072*c2β*c6*c1*s1p3+ 3/3072*δ^2*c2β*c6*c1*s1p3+ 1/1536*c2β*c6*c1*s1p3− 35/512*c2β*c1*s1p3+ 105/512*δ^2*c2β*c1*s1p3− 23/3072*c2β*c1*s1p3+ 1/128*c2*c1*s1p3− 3/128*δ^2*c2*c1*s1p3− 1/64*c2*c1*s1p3+ 79/6144*c4β*c1*s1p3− 237/6144*δ^2*c4β*c1*s1p3− 79/3072*c4β*c1*s1p3+ 1/128*c4*c1*s1p3− 3/128*δ^2*c4*c1*s1p3− 1/64*c4*c1*s1p3− 11/64*c1*s1p3+ 33/64*δ^2*c1*s1p3+ 19/64*c1*s1p3+ 19/2048*c4β*c3*s1p3− 57/2048*δ^2*c4β*c3*s1p3− 19/1024*c4β*c3*s1p3+ 9/2048*c4β*c5*s1p3− 27/2048*δ^2*c4β*c5*s1p3− 9/1024*c4β*c5*s1p3− 1/2048*c4β*c7*s1p3+ 3/2048*δ^2*c4β*c7*s1p3+ 1/1024*c4β*c7*s1p3
		  dAdδ(189) = 13/512* s2β* c2*c1p2*s1p4 − 39/512* δ^2*s2β *c2*c1p2*s1p4 + 11/1024* s4β* c2*c1p2*s1p4 − 33/1024* δ^2*s4β *c2*c1p2*s1p4− 13/256 *s2β *c2*c1p2*s1p4 − 11/512 *s4β *c2*c1p2*s1p4 + 1/2048 *s2β *c4*c1p2*s1p4 − 3/2048 *δ^2*s2β *c4*c1p2*s1p4− 9/4096 *s4β *c4*c1p2*s1p4 + 27/4096 *δ^2*s4β *c4*c1p2*s1p4 − 1/1024 *s2β *c4*c1p2*s1p4 + 9/2048 *s4β* c4*c1p2*s1p4− 29/2048 *s2β *c1p2*s1p4 + 87/2048* δ^2*s2β *c1p2*s1p4 + 5/4096 *s4β* c1p2*s1p4 − 15/4096* δ^2*s4β *c1p2*s1p4− 355/4096 *s2β *c1p2*s1p4 + 1065/4096* δ^2*s2β *c1p2*s1p4 − 5/2048 *s4β *c1p2*s1p4
		  dAdδ(190) = − 1/96 *sβ2 *c2β *c2*c1p3*s1p5 + 3/96* δ^2*sβ2 *c2β *c2*c1p3*s1p5 + 1/48 *sβ2 *c2β *c2*c1p3*s1p5+ 1/96 *sβ2 *c2β *c1p3*s1p5 − 3/96 *δ^2*sβ2 *c2β *c1p3*s1p5 − 1/48 *sβ2* c2β* c1p3*s1p5 − 1/192 *sβ2 *c2*c1p3*s1p5+ 3/192* δ^2*sβ2 *c2*c1p3*s1p5 + 1/96 *sβ2 *c2*c1p3*s1p5 + 7/192 *sβ2* c1p3*s1p5 − 21/192* δ^2*sβ2 *c1p3*s1p5− 7/96 *sβ2* c1p3*s1p5
		  dAdδ(191) = 1/384 *cβ *sβ3 *c1p4*s1p6 − 3/384 *δ^2*cβ *sβ3 *c1p4*s1p6 − 1/48 *cβ *sβ3 *c1p4*s1p6
		  dAdδ(192) = − 1/512^ sβ2 *c2β *s4 + 3/512* δ^2*sβ2 *c2β *s4 + 7/1024 *sβ2 *c2β *s8 − 21/1024* δ^2*sβ2 *c2β *s8+ 7/512 *sβ2 *c2β *s8 + 1/1024 *sβ2 *cβ *s4 − 3/1024* δ^2*sβ2 *cβ *s4 + 45/512 *sβ2* s4− 135/512* δ^2*sβ2 *s4 − 5/1024 *sβ2 *s8 + 15/1024* δ^2*sβ2 *s8 − 77/1024* sβ2* s4 + 5/2048 *sβ2 *s8
		  dAdδ(193) = − 189/128 *c2β *sβ2* s2p3*c2 + 567/128 *δ^2*c2β *sβ2* s2p3*c2 + 189/64 *c2β *sβ2* s2p3*c2 − 135/128* sβ2* s2p3*c2+ 405/128* δ^2*sβ2* s2p3*c2 + 135/64 *sβ2 *s2p3*c2
		  dAdδ(194) = − 7/196608*s2β*c10+21/196608*δ^2*s2β*c10− 7/131072*s4β*c10+21/131072*δ^2*s4β*c10+7/98304*s2β*c10+7/65536*s4β*c10− 11/24576*s2β*c2β*c4+33/24576*δ^2*s2β*c2β*c4+11/12288*s2β*c2β*c4− 91/65536*s2β*c2β*c6+273/65536*δ^2*s2β*c2β*c6+91/32768*s2β*c2β*c6− 173/98304*s3β*cβ*c2+519/98304*δ^2*s3β*cβ*c2+6031/98304*sβ*cβ*c2− 18093/98304*δ^2*sβ*cβ*c2+173/49152*s3β*cβ*c2− 10511/49152*sβ*cβ*c2+679/24576*s2β*c4− 2037/24576*δ^2*s2β*c4− 557/4096*s2β*c4− 201/65536*s2β*c6+603/65536*δ^2*s2β*c6− 1719/32768*s2β*c6+37/98304*s2β*c8− 111/98304*δ^2*s2β*c8− 91/65536*s4β*c8− 37/49152*s2β*c8+91/32768*s4β*c8− 1/32768*s3β*cβ+3/32768*δ^2*s3β*cβ− 85/32768*sβ*cβ+255/32768*δ^2*sβ*cβ− 341/8192*sβ*cβ
		  dAdδ(195) = 37/1024*c2β*c2*c1p3*s1− 111/1024*δ*2*c2β*c2*c1p3*s1+ 347/2048*c2β*c2*c1p3*s1+ 3/512*c2β*c4*c1p3*s1− 9/512*δ*2*c2β*c4*c1p3*s1− 3/256*c2β*c4*c1p3*s1+ 1/3072*c2β*c6*c1p3*s1− 3/3072*δ*2*c2β*c6*c1p3*s1− 1/1536*c2β*c6*c1p3*s1− 35/512*c2β*c1p3*s1+ 105/512*δ*2*c2β*c1p3*s1− 23/3072*c2β*c1p3*s1− 1/128*c2*c1p3*s1+ 3/128*δ*2*c2*c1p3*s1+ 1/64*c2*c1p3*s1+ 79/6144*c4β*c1p3*s1− 237/6144*δ*2*c4β*c1p3*s1− 19/2048*c4β*c1p3*s3+ 57/2048*δ*2*c4β*c1p3*s3+ 9/2048*c4β*c1p3*s5− 27/2048*δ*2*c4β*c1p3*s5+ 1/2048*c4β*c1p3*s7− 3/2048*δ*2*c4β*c1p3*s7− 79/3072*c4β*c1p3*s1+ 19/1024*c4β*c1p3*s3− 9/1024*c4β*c1p3*s5− 1/1024*c4β*c1p3*s7− 1/128*c4*c1p3*s1+ 3/128*δ*2*c4*c1p3*s1+ 1/64*c4*c1p3*s1− 11/64*c1p3*s1+ 33/64*δ*2*c1p3*s1+ 19/32*c1p3*s1
		  dAdδ(196) = − 459/1024 *s2β *c2*c1p4*s1p2 + 1377/1024 *δ^2*s2β* c2*c1p4*s1p2 + 2079/2048 *s4β *c2*c1p4*s1p2 − 6237/2048 *δ^2*s4β *c2*c1p4*s1p2+ 567/4096* s2β *c4*c1p4*s1p2 − 1701/4096* δ^2*s2β *c4*c1p4*s1p2 + 1701/8192 *s4β *c4*c1p4*s1p2 − 5103/8192* δ^2*s4β *c4*c1p4*s1p2+ 4923/2048 *s2β *c1p4*s1p2 − 14769/2048 *δ^2*s2β *c1p4*s1p2 − 945/8192 *s4β* c1p4*s1p2 + 2835/8192 *δ^2*s4β* c1p4*s2
		  dAdδ(197) = − 27/64*c2β*c2*c1p5*s1+ 81/64*δ^2*c2β*c2*c1p5*s1+ 27/32*c2β*c2*c1p5*s1− 81/512*c2β*c4*c1p5*s1+ 243/512*δ^2*c2β*c4*c1p5*s1+ 81/256*c2β*c4*c1p5*s1+ 1233/512*c2β*c1p5*s1− 3699/512*δ^2*c2β*c1p5*s1− 4689/1024*c2β*c1p5*s1− 27/64*c4β*c1p5*c2s1+ 81/64*δ^2*c4β*c1p5*c2s1+ 27/32*c4β*c1p5*c2s1− 27/32*c2*c1p5*s1+ 81/32*δ^2*c2*c1p5*s1+ 27/16*c2*c1p5*s1− 243/512*c4β*c4*c1p5*s1+ 729/512*δ^2*c4β*c4*c1p5*s1+ 243/256*c4β*c4*c1p5*s1+ 27/512*c4β*c1p5*s1− 81/512*δ^2*c4β*c1p5*s1− 27/256*c4β*c1p5*s1+ 27/64* c1p5*s1− 81/64*δ^2* c1p5*s1− 27/32* c1p5*s1
		  dAdδ(198) = 837/1024*s2β*c2*c1p6− 2511/1024*δ^2*s2β*c2*c1p6− 621/2048*s4β*c2*c1p6+ 1863/2048*δ^2*s4β*c2*c1p6− 837/512*s2β*c2*c1p6+ 621/1024*s4β*c2*c1p6− 243/4096*s2β*c4*c1p6+ 729/4096*δ^2*s2β*c4*c1p6+ 2187/8192*s4β*c4*c1p6− 6561/8192*δ^2*s4β*c4*c1p6+ 243/2048*s2β*c4*c1p6− 2187/4096*s4β*c4*c1p6− 4761/4096*s2β*c1p6+ 14283/4096*δ^2*s2β*c1p6+ 1377/8192*s4β*c1p6− 4131/8192*δ^2*s4β*c1p6+ 11673/8192*s2β*c1p6− 1377/4096*s4β*c1p6
		  dAdδ(199) = 81/64 *c2β* sβ2 *c2*c1p7*s1 − 243/64 *δ^2*c2β* sβ2 *c2*c1p7*s1 − 81/32 *c2β *sβ2 *c2*c1p7*s1 − 27/64 *c2β* sβ2 *c1p7*s1+ 81/64 *δ^2*c2β *sβ2* c1p7*s1 + 27/32 *c2β* sβ2 *c1p7*s1 + 81/128 *sβ2 *c2*c1p7*s1 − 243/128 *δ^2*sβ2 *c2*c1p7*s1− 81/64 *sβ2 *c2*c1p7*s1 + 81/128 *sβ2 *c1p7*s1 − 243/128 *δ^2*sβ2 *c1p7*s1 − 81/64 *sβ2 *c1p7*s1
		  dAdδ(200) = 81/128* cβ *sβ3* c1p8*s1p2 − 243/128 *δ^2*cβ *sβ3 *c1p8*s1p2 − 81/64 *cβ* sβ3 *c1p8*s1p2
		  dAdδ(201) = − 4375/768 *s2β *c1p6*s1p4 + 13125/768 *δ^2*s2β *c1p6*s1p4 − 4375/512 *s4β *c1p6*s1p4 + 13125/512 *δ^2*s4β* c1p6*s1p4+ 4375/384* s2β* c1p6*s1p4 + 4375/256 *s4β* c1p6*s1p4
		  dAdδ(202) = − 625/192 *c2β *s1p3*c1p7 + 1875/192* δ^2*c2β *s1p3*c1p7 + 625/96* c2β *s1p3*c1p7 − 625/64 *c4β *s1p3*c1p7+ 1875/64 *δ^2*c4β* s1p3*c1p7 + 625/32 *c4β *s1p3*c1p7
		  dAdδ(203) = − 625/512 *s2β* s1p2*c1p8 + 1875/512* δ^2*s2β *s1p2*c1p8 + 5625/1024 *s4β *s1p2*c1p8 − 16875/1024 *δ^2*s4β* s1p2*c1p8+ 625/256* s2β* s1p2*c1p8 − 5625/512 *s4β* s1p2*c1p8
		  dAdδ(204) = − 625/96 *c2β* sβ2 *s1*c1p9 + 1875/96 *δ^2*c2β* sβ2* s1*c1p9 + 625/48 *c2β *sβ2 *s1*c1p9 − 625/192* sβ2 *s1*c1p9+ 1875/192 *δ^2*sβ2 *s1*c1p9 + 625/96 *sβ2 *s1*c1p9
		  dAdδ(205) = 625/384 *cβ *sβ3 *c1p10 − 1875/384 *δ^2*cβ *sβ3 *c1p10 − 625/192* cβ *sβ3* c1p10
		  dAdδ(206) = 1/2 *δ*sβ *c2p3*χsy
		  dAdδ(207) = − 1/3* δ*sβ *c1p4*c2*χsy + 5/6* δ*sβ *c1p4*χsy
		  dAdδ(208) = 1/3 *δ*cβ *s1*c1p5*χsy
		  dAdδ(209) = − 1/6* δ*cβ *s1*c2*c1p3*χsy − 7/6* δ*cβ *s1*c1p3*χsy
		  dAdδ(210) = 1/6 *δ*cβ *s1p3*c2*c1*χsy − 7/6 *δ*cβ *s1p3*c1*χsy
		  dAdδ(211) = − 1/3* δ*sβ *c2*s1p4*χsy − 5/6 *δ*sβ* s1p4*χsy
		  dAdδ(212) = 1/3 *δ*cβ *c1*s1p5*χsy
		  dAdδ(213) = 1/2* δ*sβ *c2*s2p2*χsy
		  dAdδ(214) = − 1/6* δ*sβ *s2p2*c2*χsy
		  dAdδ(215) = − 1/4 *δ*s2p3*cβ *χsy
		  dAdδ(216) = − 5/16* δ*cβ *s2*χsy − 1/16* δ*cβ *s6*χsy
		  dAdδ(217) = − 3/8 *δ*cβ *s2*χsx − 1/8* δ*cβ* c4*s2*χsx − 1/2* δ*sβ *c4*s2*χsz
		  dAdδ(218) = 1/2 *δ*cβ* s2p2*c2*χsz − 1/2* δ*sβ *s2p2*c2*χsx
		  dAdδ(219) = δ*cβ *s2p3*χsx
		  dAdδ(220) = 1/6* δ*cβ *s1p3*c2*c1*χsx + 2/3* δ*sβ *s1p3*c2*c1*χsz − 2*δ*c3β* s1p3*c1*χsx + 5/6* δ*cβ *s1p3*c1*χsx + 2*δ*s3β *s1p3*c1*χsz + δ*sβ* s1p3*c1*χsz
		  dAdδ(221) = − 1/3* δ*cβ *c2*s1p4*χsz + 1/3* δ*sβ *s1p4*c2*χsx − 1/2* δ*c3β *s1p4*χsz − 5/2* δ*cβ *s1p4*χsz − 1/2* δ*s3β* s1p4*χsx− 5/3 *δ*sβ* s1p4*χsx
		  dAdδ(222) = − 1/3* δ*cβ *c1*s1p5*χsx
		  dAdδ(223) = − 3/2* δ*sβ *c2β *s2p2*χsx + 3*δ*sβ2 *cβ* s2p2*χsz + 7/12 *δ*sβ *s2p2*χsx
		  dAdδ(224) = − 1/6 *δ*cβ *c2*c1p3*s1*χsx − 2/3 *δ*sβ *s1*c2*c1p3*χsz − 2*δ*c3β *c1p3*s1*χsx + 5/6* δ*cβ *c1p3*s1*χsx+2*δ*s3β *s1*c1p3*χsz + δ*sβ *s1*c1p3*χsz
		  dAdδ(225) = − 1/3 *δ*cβ *c2*c1p4*χsz + 1/3* δ*sβ *c2*c1p4*χsx + 1/2* δ*c3β *c1p4*χsz + 5/2 *δ*cβ* c1p4*χsz + 1/2 *δ*s3β* c1p4*χsx+ 5/3 *δ*sβ* c1p4*χsx
		  dAdδ(226) = − 1/3 *δ*cβ *c1p5*s1*χsx
		  dAdδ(227) = 2*sβ *c2p3*χay
		  dAdδ(228) = − 14/3 *sβ *c1p4*χay + 20/3 *sβ *c2*c1p4*χay
		  dAdδ(229) = − 2/3 *cβ *c1p3*s1*χay + 10/3 *cβ *c2*c1p3*s1*χay
		  dAdδ(230) = − 20/3 *cβ *c1p5*s1*χay
		  dAdδ(231) = − 10/3 *cβ *s1p3*c2*c1*χay − 2/3 *cβ *s1p3*c1*χay
		  dAdδ(232) = 20/3 *sβ* s1p4*c2*χay + 14/3 *sβ *s1p4*χay
		  dAdδ(233) = − 20/3 *cβ *c1*s1p5*χay
		  dAdδ(234) = 2*sβ *c2*s2p2*χay
		  dAdδ(235) = 10/3 *sβ *c2*s2p2*χay
		  dAdδ(236) = −cβ *s1p3*χay
		  dAdδ(237) = − 5/4 *cβ *s2*χay − 1/4 *cβ *s6*χay
		  dAdδ(238) = − 1/2 *cβ* s2*c4*χax − 2*sβ *s2*c4*χaz − 3/2* cβ *s2*χax
		  dAdδ(239) = 2*cβ* c2*s2p2*χaz − 2*sβ *c2*s2p2*χax
		  dAdδ(240) = cβ* s2p3*χax
		  dAdδ(241) = − 10/3 *cβ *c2*c1*s1p3*χax − 40/3 *sβ* s1p3*c2*c1*χaz − 2/3 *cβ* c1*s1p3*χax − 4*sβ *s1p3*c1*χaz
		  dAdδ(242) = 20/3 *cβ* s1p4*c2*χaz − 20/3 *sβ *s1p4*c2*χax + 4*cβ *s1p4*χaz − 14/3 *sβ *s1p4*χax
		  dAdδ(243) = 20/3 *cβ* c1*s1p5*χax
		  dAdδ(244) = 1/3 *sβ *s2p2*χax
		  dAdδ(245) = 10/3 *cβ *c2*c1p3*s1*χax + 40/3 *sβ *s1*c2*c1p3*χaz − 2/3 *cβ *s1*c1p3*χax − 4*sβ *s1*c1p3*χaz
		  dAdδ(246) = 20/3 *cβ *c2*c1p4*χaz − 20/3 *sβ *c2*c1p4*χax − 4*cβ* c1p4*χaz + 14/3 *sβ* c1p4*χax
		  dAdδ(247) = 20/3* cβ* c1p5*s1*χax
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDAdχaxDN()
		  // load the constant parameters
		  Var ιDN As Double  =self.ιDN 
		  Var β As Double  = self.β 
		  Var c2 As Double  = Trig.c2
		  Var s2 As Double = Trig.s2
		  Var c1 As Double = Trig.c1
		  Var s1   As Double = Trig.s1
		  Var c3   As Double = Trig.c3
		  Var s3  As Double  = Trig.s3
		  Var c4   As Double = Trig.c4
		  Var s4  As Double  = Trig.s4
		  Var c5  As Double  = Trig.c5
		  Var s5   As Double = Trig.s5
		  Var c6 As Double   = Trig.c6
		  Var s6   As Double = Trig.s6
		  Var c7   As Double = Trig.c7
		  Var s7 As Double   = Trig.s7
		  Var c8  As Double  = Trig.c8
		  Var s8  As Double  = Trig.s8
		  Var c9  As Double  = Trig.c9
		  Var s9   As Double = Trig.s9
		  Var c10  As Double  = Trig.c10
		  Var s10  As Double  = Trig.s10
		  Var c1p2  As Double  = Trig.c1p2
		  Var c1p3  As Double  = Trig.c1p3   
		  Var c1p4  As Double  =Trig. c1p4   
		  Var c1p5 As Double  = Trig.c1p5   
		  Var c1p6 As Double   = Trig.c1p6   
		  Var c1p7  As Double  = Trig.c1p7 
		  Var c1p8 As Double   = Trig.c1p8  
		  Var c1p9   As Double = Trig.c1p9  
		  Var c1p10  As Double  = Trig.c1p10
		  Var s1p2   As Double = Trig.s1p2   
		  Var s1p3  As Double  = Trig.s1p3   
		  Var s1p4As Double    = Trig.s1p4   
		  Var s1p5   As Double = Trig.s1p5  
		  Var s1p6 As Double   = Trig.s1p6   
		  Var s1p7 As Double   = Trig.s1p7   
		  Var s1p8  As Double  = Trig.s1p8   
		  Var s1p9 As Double   = Trig.s1p
		  Var s1p10 As Double  = Trig.s1p10
		  Var c2p2  As Double  = Trig.c2p2
		  Var c2p3   As Double = Trig.c2p3
		  Var c2p4 As Double  = Trig.c2p4
		  Var s2p2  As Double  = Trig.s2p2
		  Var s2p3  As Double  = Trig.s2p3
		  Var s2p4 As Double   = Trig.s2p4
		  Var s2p5 As Double   = Trig.s2p5
		  
		  // Define local β trig functions
		  Var cβ  As Double  = Trig.cβ
		  Var sβ  As Double  = Trig.sβ
		  Var c2β As Double   = Trig.c2β
		  Var s2β  As Double  = Trig.s2β
		  Var c3β As Double   = Trig.c3β
		  Var s3β  As Double  = Trig.s3β
		  Var c4β As Double   = Trig.c4β
		  Var s4β  As Double  = Trig.s4β
		  Var c5β As Double   = Trig.c5β
		  Var s5β As Double   = Trig.s5β
		  Var cβ2  As Double  = Trig.cβ2
		  Var cβ3 As Double   = Trig.cβ3
		  Var sβ2  As Double  = Trig.sβ2
		  Var sβ3  As Double  = Trig.sβ3
		  
		  
		  // Load the χaxDN derivatives
		  dAdχaxDN(0) = 0
		  dAdχaxDN(1) = 0
		  dAdχaxDN(2) = 0
		  dAdχaxDN(3) = 0
		  dAdχaxDN(4) = 0
		  dAdχaxDN(5) = 0
		  dAdχaxDN(6) = 0
		  dAdχaxDN(7) = 0
		  dAdχaxDN(8) = 0
		  dAdχaxDN(9) = 0
		  dAdχaxDN(10) = 0
		  dAdχaxDN(11) = 0
		  dAdχaxDN(12) = 0
		  dAdχaxDN(13) = 0
		  dAdχaxDN(14) = 0
		  dAdχaxDN(15) = 0
		  dAdχaxDN(16) = 0
		  dAdχaxDN(17) = 0
		  dAdχaxDN(18) = 0
		  dAdχaxDN(19) = 0
		  dAdχaxDN(20) = 0
		  dAdχaxDN(21) = 0
		  dAdχaxDN(22) = 0
		  dAdχaxDN(23) = 0
		  dAdχaxDN(24) = 0
		  dAdχaxDN(25) = 0
		  dAdχaxDN(26) = 0
		  dAdχaxDN(27) = 0
		  dAdχaxDN(28) = 0
		  dAdχaxDN(29) = 0
		  dAdχaxDN(30) = 0
		  dAdχaxDN(31) = 0
		  dAdχaxDN(32) = 0
		  dAdχaxDN(33) = 0
		  dAdχaxDN(34) = 0
		  dAdχaxDN(35) = 0
		  dAdχaxDN(36) = 0
		  dAdχaxDN(37) = cβ*c1p2
		  dAdχaxDN(38) = 0.5*cβ-0.5*cβ*c2
		  dAdχaxDN(39) = 0
		  dAdχaxDN(40) = 0
		  dAdχaxDN(41) = 0
		  dAdχaxDN(42) = 0
		  dAdχaxDN(43) = 0
		  dAdχaxDN(44) = 0
		  dAdχaxDN(45) = 0
		  dAdχaxDN(46) = 0
		  dAdχaxDN(47) = 0
		  dAdχaxDN(48) = 0
		  dAdχaxDN(49) = 0
		  dAdχaxDN(50) = 0
		  dAdχaxDN(51) = 0
		  dAdχaxDN(52) = 0
		  dAdχaxDN(53) = 0
		  dAdχaxDN(54) = 0
		  dAdχaxDN(55) = 0
		  dAdχaxDN(56) = 0
		  dAdχaxDN(57) = 0
		  dAdχaxDN(58) = 0
		  dAdχaxDN(59) = 0
		  dAdχaxDN(60) = 0
		  dAdχaxDN(61) = 0
		  dAdχaxDN(62) = 0
		  dAdχaxDN(63) = 0
		  dAdχaxDN(64) = 0
		  dAdχaxDN(65) = 0
		  dAdχaxDN(66) = 0
		  dAdχaxDN(67) = 0
		  dAdχaxDN(68) = 0
		  dAdχaxDN(69) = 0
		  dAdχaxDN(70) = 0
		  dAdχaxDN(71) = 0
		  dAdχaxDN(72) = 0
		  dAdχaxDN(73) = 0
		  dAdχaxDN(74) = 0
		  dAdχaxDN(75) = 0
		  dAdχaxDN(76) = 0
		  dAdχaxDN(77) = 0
		  dAdχaxDN(78) = 0
		  dAdχaxDN(79) = 0
		  dAdχaxDN(80) = 0
		  dAdχaxDN(81) = 0
		  dAdχaxDN(82) = 0
		  dAdχaxDN(83) = 0
		  dAdχaxDN(84) = 0
		  dAdχaxDN(85) = 0
		  dAdχaxDN(86) = 0
		  dAdχaxDN(87) = 0
		  dAdχaxDN(88) = 0
		  dAdχaxDN(89) = 0
		  dAdχaxDN(90) = 0
		  dAdχaxDN(91) = 0
		  dAdχaxDN(92) = 0
		  dAdχaxDN(93) = 0
		  dAdχaxDN(94) = 0
		  dAdχaxDN(95) = 0
		  dAdχaxDN(96) = 0
		  dAdχaxDN(97) = 0
		  dAdχaxDN(98) = 0
		  dAdχaxDN(99) = 0
		  dAdχaxDN(100) = 0
		  dAdχaxDN(101) = 0
		  dAdχaxDN(102) = 0
		  dAdχaxDN(103) = 0
		  dAdχaxDN(104) = 0
		  dAdχaxDN(105) = 0
		  dAdχaxDN(106) = 0
		  dAdχaxDN(107) = 2*δ*cβ*sβ*c2p3
		  2 
		  dAdχaxDN(108) = 7/3 *δ*c2β*c1p4 - 10/3*δ*s2β*c1p4*c2
		  dAdχaxDN(109) = 5*δ*c1p5*s1 + 5/3 *δ*c2β*c1p5*s1
		  dAdχaxDN(110) = 5*δ*c1*s1p5 + 5/3 δ*c2β*c1*s1p5 
		  dAdχaxDN(111) = 35/6* δ*c2β *c2*c1p3*s1 − 13/6 *δ*c2β*c1p3*s1 − 5/2 *δ*c2*c1p3*s1 + 3/2 *δ*c3
		  dAdχaxDN(112) = − 35/6 *δ*c2β* c2*c1*s1p3 − 13/6 *δ*c2β* c1*s1p3 + 5/2* δ*c2*c1*s1p3 + 3/2* δ*c1*s1p3 
		  dAdχaxDN(113) = − 10/3 *δ*s2β *c2*s1p4 − 7/3* δ*s2β* s1p4
		  dAdχaxDN(114) = 0
		  dAdχaxDN(115) = 3/4 *δ*s2p3 + 1/4 *δ*c2β *s2p3
		  dAdχaxDN(116) = 10/3* δ*cβ *sβ *c2s2p2 
		  dAdχaxDN(117) = −δ*s2β* c2*s2p2 
		  dAdχaxDN(118) = − 11/16* δ*c2β* s2 − 7/16 *δ*c2β *s6 − 3/4 *δ*s2p3
		  dAdχaxDN(119) = 0
		  dAdχaxDN(120) = 0
		  dAdχaxDN(121) = 0
		  dAdχaxDN(122) = 0
		  dAdχaxDN(123) = 0
		  dAdχaxDN(124) = 0
		  dAdχaxDN(125) = 0
		  dAdχaxDN(126) = 0
		  dAdχaxDN(127) = 0
		  dAdχaxDN(128) = 0
		  dAdχaxDN(129) = 0
		  dAdχaxDN(130) = 0
		  dAdχaxDN(131) = 0
		  dAdχaxDN(132) = 0
		  dAdχaxDN(133) = 0
		  dAdχaxDN(134) = 0
		  dAdχaxDN(135) = 0
		  dAdχaxDN(136) = 0
		  dAdχaxDN(137) = 0
		  dAdχaxDN(138) = 0
		  dAdχaxDN(139) = 0
		  dAdχaxDN(140) = 0
		  dAdχaxDN(141) = 0
		  dAdχaxDN(142) = 0
		  dAdχaxDN(143) = 0
		  dAdχaxDN(144) = 0
		  dAdχaxDN(145) = 0
		  dAdχaxDN(146) = 0
		  dAdχaxDN(147) = 0
		  dAdχaxDN(148) = 0
		  dAdχaxDN(149) = 0
		  dAdχaxDN(150) = 0
		  dAdχaxDN(151) = 0
		  dAdχaxDN(152) = 0
		  dAdχaxDN(153) = 0
		  dAdχaxDN(154) = 0
		  dAdχaxDN(155) = 0
		  dAdχaxDN(156) = 0
		  dAdχaxDN(157) = 0
		  dAdχaxDN(158) = 0
		  dAdχaxDN(159) = 0
		  dAdχaxDN(160) = 0
		  dAdχaxDN(161) = 0
		  dAdχaxDN(162) = 0
		  dAdχaxDN(163) = 0
		  dAdχaxDN(164) = 0
		  dAdχaxDN(165) = 1/2 *cβ2 − 1/2* cβ2* c2
		  dAdχaxDN(166) = cβ *sβ *s2 
		  dAdχaxDN(167) = 1/2* cβ2 + 1/2* cβ2 *c2
		  dAdχaxDN(168) = 0
		  dAdχaxDN(169) = 0
		  dAdχaxDN(170) = 0
		  dAdχaxDN(171) = 0
		  dAdχaxDN(172) = 0
		  dAdχaxDN(173) = 0
		  dAdχaxDN(174) = 0
		  dAdχaxDN(175) = 0
		  dAdχaxDN(176) = 0
		  dAdχaxDN(177) = 0
		  dAdχaxDN(178) = 0
		  dAdχaxDN(179) = 0
		  dAdχaxDN(180) = 0
		  dAdχaxDN(181) = 0
		  dAdχaxDN(182) = 0
		  dAdχaxDN(183) = 0
		  dAdχaxDN(184) = 0
		  dAdχaxDN(185) = 0
		  dAdχaxDN(186) = 0
		  dAdχaxDN(187) = 0
		  dAdχaxDN(188) = 0
		  dAdχaxDN(189) = 0
		  dAdχaxDN(190) = 0
		  dAdχaxDN(191) = 0
		  dAdχaxDN(192) = 0
		  dAdχaxDN(193) = 0
		  dAdχaxDN(194) = 0
		  dAdχaxDN(195) = 0
		  dAdχaxDN(196) = 0
		  dAdχaxDN(197) = 0
		  dAdχaxDN(198) = 0
		  dAdχaxDN(199) = 0
		  dAdχaxDN(200) = 0
		  dAdχaxDN(201) = 0
		  dAdχaxDN(202) = 0
		  dAdχaxDN(203) = 0
		  dAdχaxDN(204) = 0
		  dAdχaxDN(205) = 0
		  dAdχaxDN(206) = 0
		  dAdχaxDN(207) = 0
		  dAdχaxDN(208) = 0
		  dAdχaxDN(209) = 0
		  dAdχaxDN(210) = 0
		  dAdχaxDN(211) = 0
		  dAdχaxDN(212) = 0
		  dAdχaxDN(213) = 0
		  dAdχaxDN(214) = 0
		  dAdχaxDN(215) = 0
		  dAdχaxDN(216) = 0
		  dAdχaxDN(217) = 0
		  dAdχaxDN(218) = 0
		  dAdχaxDN(219) = 0
		  dAdχaxDN(220) = 0
		  dAdχaxDN(221) = 0
		  dAdχaxDN(222) = 0
		  dAdχaxDN(223) = 0
		  dAdχaxDN(224) = 0
		  dAdχaxDN(225) = 0
		  dAdχaxDN(226) = 0
		  dAdχaxDN(227) = 0
		  dAdχaxDN(228) = 0
		  dAdχaxDN(229) = 0
		  dAdχaxDN(230) = 0
		  dAdχaxDN(231) = 0
		  dAdχaxDN(232) = 0
		  dAdχaxDN(233) = 0
		  dAdχaxDN(234) = 0
		  dAdχaxDN(235) = 0
		  dAdχaxDN(236) = 0
		  dAdχaxDN(237) = 0
		  dAdχaxDN(238) = − 1/2 *δ*cβ* s2*c4 − 3/2* δ*cβ* s2
		  dAdχaxDN(239) = −2*δ*sβ* c2*s2p2 
		  dAdχaxDN(240) = δ*cβ* s3
		  dAdχaxDN(241) = − 10/3* δ*cβ *c2*c1*s1p3 − 2/3* δ*cβ* c1*s1p3
		  dAdχaxDN(242) = − 20/3 *δ*sβ* s1p4*c2 − 14/3 *δ*sβ *s1p4
		  dAdχaxDN(243) = 20/3* δ*cβ* c1*s1p5
		  dAdχaxDN(244) = 1/3 *δ*sβ* s2p2
		  dAdχaxDN(245) = 10/3* δ*cβ *c2*c1p3*s1 − 2/3 *δ*cβ *s1*c1p3
		  dAdχaxDN(246) = − 20/3* δ*sβ *c2*c1p4 + 14/3 *δ*sβ *c1p4
		  dAdχaxDN(247) = 20/3* δ*cβ *c1p5*s1
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDAdχayDN()
		  // load the constant parameters
		  Var ιDN As Double  =self.ιDN 
		  Var β As Double  = self.β 
		  Var c2 As Double  = Trig.c2
		  Var s2 As Double = Trig.s2
		  Var c1 As Double = Trig.c1
		  Var s1   As Double = Trig.s1
		  Var c3   As Double = Trig.c3
		  Var s3  As Double  = Trig.s3
		  Var c4   As Double = Trig.c4
		  Var s4  As Double  = Trig.s4
		  Var c5  As Double  = Trig.c5
		  Var s5   As Double = Trig.s5
		  Var c6 As Double   = Trig.c6
		  Var s6   As Double = Trig.s6
		  Var c7   As Double = Trig.c7
		  Var s7 As Double   = Trig.s7
		  Var c8  As Double  = Trig.c8
		  Var s8  As Double  = Trig.s8
		  Var c9  As Double  = Trig.c9
		  Var s9   As Double = Trig.s9
		  Var c10  As Double  = Trig.c10
		  Var s10  As Double  = Trig.s10
		  Var c1p2  As Double  = Trig.c1p2
		  Var c1p3  As Double  = Trig.c1p3   
		  Var c1p4  As Double  =Trig. c1p4   
		  Var c1p5 As Double  = Trig.c1p5   
		  Var c1p6 As Double   = Trig.c1p6   
		  Var c1p7  As Double  = Trig.c1p7 
		  Var c1p8 As Double   = Trig.c1p8  
		  Var c1p9   As Double = Trig.c1p9  
		  Var c1p10  As Double  = Trig.c1p10
		  Var s1p2   As Double = Trig.s1p2   
		  Var s1p3  As Double  = Trig.s1p3   
		  Var s1p4As Double    = Trig.s1p4   
		  Var s1p5   As Double = Trig.s1p5  
		  Var s1p6 As Double   = Trig.s1p6   
		  Var s1p7 As Double   = Trig.s1p7   
		  Var s1p8  As Double  = Trig.s1p8   
		  Var s1p9 As Double   = Trig.s1p
		  Var s1p10 As Double  = Trig.s1p10
		  Var c2p2  As Double  = Trig.c2p2
		  Var c2p3   As Double = Trig.c2p3
		  Var c2p4 As Double  = Trig.c2p4
		  Var s2p2  As Double  = Trig.s2p2
		  Var s2p3  As Double  = Trig.s2p3
		  Var s2p4 As Double   = Trig.s2p4
		  Var s2p5 As Double   = Trig.s2p5
		  
		  // Define local β trig functions
		  Var cβ  As Double  = Trig.cβ
		  Var sβ  As Double  = Trig.sβ
		  Var c2β As Double   = Trig.c2β
		  Var s2β  As Double  = Trig.s2β
		  Var c3β As Double   = Trig.c3β
		  Var s3β  As Double  = Trig.s3β
		  Var c4β As Double   = Trig.c4β
		  Var s4β  As Double  = Trig.s4β
		  Var c5β As Double   = Trig.c5β
		  Var s5β As Double   = Trig.s5β
		  Var cβ2  As Double  = Trig.cβ2
		  Var cβ3 As Double   = Trig.cβ3
		  Var sβ2  As Double  = Trig.sβ2
		  Var sβ3  As Double  = Trig.sβ3
		  
		  
		  //Calculate the dAdχayDN
		  dAdχayDN(0) = 0
		  dAdχayDN(1) = 0
		  dAdχayDN(2) = 0
		  dAdχayDN(3) = 0
		  dAdχayDN(4) = 0
		  dAdχayDN(5) = 0
		  dAdχayDN(6) = 0
		  dAdχayDN(7) = 0
		  dAdχayDN(8) = 0
		  dAdχayDN(9) = 0
		  dAdχayDN(10) = 0
		  dAdχayDN(11) = 0
		  dAdχayDN(12) = 0
		  dAdχayDN(13) = 0
		  dAdχayDN(14) = 0
		  dAdχayDN(15) = 0
		  dAdχayDN(16) = 0
		  dAdχayDN(17) = 0
		  dAdχayDN(18) = 0
		  dAdχayDN(19) = 0
		  dAdχayDN(20) = 0
		  dAdχayDN(21) = 0
		  dAdχayDN(22) = 0
		  dAdχayDN(23) = 0
		  dAdχayDN(24) = 0
		  dAdχayDN(25) = 0
		  dAdχayDN(26) = 0
		  dAdχayDN(27) = 0
		  dAdχayDN(28) = 0
		  dAdχayDN(29) = 0
		  dAdχayDN(30) = 0
		  dAdχayDN(31) = 0
		  dAdχayDN(32) = 0
		  dAdχayDN(33) = 0
		  dAdχayDN(34) = 0
		  dAdχayDN(35) = 0
		  dAdχayDN(36) = 0
		  dAdχayDN(37) = 0
		  dAdχayDN(38) = 0
		  dAdχayDN(39) = 0
		  dAdχayDN(40) = 0
		  dAdχayDN(41) = 0
		  dAdχayDN(42) = 0
		  dAdχayDN(43) = 0
		  dAdχayDN(44) = 0
		  dAdχayDN(45) = 0
		  dAdχayDN(46) = 0
		  dAdχayDN(47) = 0
		  dAdχayDN(48) = 0
		  dAdχayDN(49) = 0
		  dAdχayDN(50) = 0
		  dAdχayDN(51) = 0
		  dAdχayDN(52) = 0
		  dAdχayDN(53) = 0
		  dAdχayDN(54) = 0
		  dAdχayDN(55) = 0
		  dAdχayDN(56) = 0
		  dAdχayDN(57) = 0
		  dAdχayDN(58) = 0
		  dAdχayDN(59) = 0
		  dAdχayDN(60) = 0
		  dAdχayDN(61) = 0
		  dAdχayDN(62) = 0
		  dAdχayDN(63) = 0
		  dAdχayDN(64) = 0
		  dAdχayDN(65) = 0
		  dAdχayDN(66) = 0
		  dAdχayDN(67) = 0
		  dAdχayDN(68) = 0
		  dAdχayDN(69) = 0
		  dAdχayDN(70) = 0
		  dAdχayDN(71) = 0
		  dAdχayDN(72) = 0
		  dAdχayDN(73) = 0
		  dAdχayDN(74) = 0
		  dAdχayDN(75) = 0
		  dAdχayDN(76) = 0
		  dAdχayDN(77) = 0
		  dAdχayDN(78) = 0
		  dAdχayDN(79) = 0
		  dAdχayDN(80) = 0
		  dAdχayDN(81) = 0
		  dAdχayDN(82) = 0
		  dAdχayDN(83) = 0
		  dAdχayDN(84) = 0
		  dAdχayDN(85) = 0
		  dAdχayDN(86) = 0
		  dAdχayDN(87) = 0
		  dAdχayDN(88) = 0
		  dAdχayDN(89) = 0
		  dAdχayDN(90) = 0
		  dAdχayDN(91) = 0
		  dAdχayDN(92) = 0
		  dAdχayDN(93) = 0
		  dAdχayDN(94) = 0
		  dAdχayDN(95) = 0
		  dAdχayDN(96) = 0
		  dAdχayDN(97) = 0
		  dAdχayDN(98) = 0
		  dAdχayDN(99) = 0
		  dAdχayDN(100) = 0
		  dAdχayDN(101) = 0
		  dAdχayDN(102) = 0
		  dAdχayDN(103) = 0
		  dAdχayDN(104) = 0
		  dAdχayDN(105) = 0
		  dAdχayDN(106) = 0
		  dAdχayDN(107) = 0
		  dAdχayDN(108) = 0
		  dAdχayDN(109) = 0
		  dAdχayDN(110) = 0
		  dAdχayDN(111) = 0
		  dAdχayDN(112) = 0
		  dAdχayDN(113) = 0
		  dAdχayDN(114) = 0
		  dAdχayDN(115) = 0
		  dAdχayDN(116) = 0
		  dAdχayDN(117) = 0
		  dAdχayDN(118) = 0
		  dAdχayDN(119) = − 5/8* δ*c2β* c4*s2 − 3/8 *δ*c2β* s2 + 9/8 *δ*c4*s2 + 15/8 *δ*s2
		  dAdχayDN(120) = − 2*δ*cβ *sβ *c2*s2p2
		  dAdχayDN(121) = 3/4 *δ*s2p3 + 1/4* δ*c2β* s2p3
		  dAdχayDN(122) = − 25/6*δ*c2β *c2*c1*s1p3 − 11/6* δ*c2β* c1*s1p3 + 15/2 *δ*c2*c1*s1p3 + 5/2* δ*c1*s1p3
		  dAdχayDN(123) = − 7/3 *δ*s2β* s1p4 − 10/3* δ*s2β* s1p4*c2 
		  dAdχayDN(124) = 5*δ*c1*s1p5 + 5/3* δ*c2β* c1*s1p5
		  dAdχayDN(125) = − 1/3* δ*cβ* sβ *s2p2
		  dAdχayDN(126) = 25/6* δ*c2β *c2*c1p3*s1 − 11/6 *δ*c2β *c1p3*s1 − 15/2 *δ*c2*c1p3*s1 + 5/2* δ*c1p3*s1
		  dAdχayDN(127) = 7/3* δ*s2β* c1p4 − 10/3 *δ*s2β c1p4*c2
		  dAdχayDN(128) = 5*δ*c1p5*s1 + 5/3* δ*c2β* c1p5*s1 
		  dAdχayDN(129) = 0
		  dAdχayDN(130) = 0
		  dAdχayDN(131) = 0
		  dAdχayDN(132) = 0
		  dAdχayDN(133) = 0
		  dAdχayDN(134) = 0
		  dAdχayDN(135) = 0
		  dAdχayDN(136) = 0
		  dAdχayDN(137) = 0
		  dAdχayDN(138) = 0
		  dAdχayDN(139) = 0
		  dAdχayDN(140) = 0
		  dAdχayDN(141) = 0
		  dAdχayDN(142) = 0
		  dAdχayDN(143) = 0
		  dAdχayDN(144) = 0
		  dAdχayDN(145) = 0
		  dAdχayDN(146) = 0
		  dAdχayDN(147) = 0
		  dAdχayDN(148) = 0
		  dAdχayDN(149) = 0
		  dAdχayDN(150) = 0
		  dAdχayDN(151) = 0
		  dAdχayDN(152) = 0
		  dAdχayDN(153) = 0
		  dAdχayDN(154) = 0
		  dAdχayDN(155) = 0
		  dAdχayDN(156) = 0
		  dAdχayDN(157) = 0
		  dAdχayDN(158) = 0
		  dAdχayDN(159) = 0
		  dAdχayDN(160) = 0
		  dAdχayDN(161) = 0
		  dAdχayDN(162) = 0
		  dAdχayDN(163) = 0
		  dAdχayDN(164) = 0
		  dAdχayDN(165) = 0
		  dAdχayDN(166) = 0
		  dAdχayDN(167) = 0
		  dAdχayDN(168) = 0
		  dAdχayDN(169) = 0
		  dAdχayDN(170) = 0
		  dAdχayDN(171) = 0
		  dAdχayDN(172) = 0
		  dAdχayDN(173) = 0
		  dAdχayDN(174) = 0
		  dAdχayDN(175) = 0
		  dAdχayDN(176) = 0
		  dAdχayDN(177) = 0
		  dAdχayDN(178) = 0
		  dAdχayDN(179) = 0
		  dAdχayDN(180) = 0
		  dAdχayDN(181) = 0
		  dAdχayDN(182) = 0
		  dAdχayDN(183) = 0
		  dAdχayDN(184) = 0
		  dAdχayDN(185) = 0
		  dAdχayDN(186) = 0
		  dAdχayDN(187) = 0
		  dAdχayDN(188) = 0
		  dAdχayDN(189) = 0
		  dAdχayDN(190) = 0
		  dAdχayDN(191) = 0
		  dAdχayDN(192) = 0
		  dAdχayDN(193) = 0
		  dAdχayDN(194) = 0
		  dAdχayDN(195) = 0
		  dAdχayDN(196) = 0
		  dAdχayDN(197) = 0
		  dAdχayDN(198) = 0
		  dAdχayDN(199) = 0
		  dAdχayDN(200) = 0
		  dAdχayDN(201) = 0
		  dAdχayDN(202) = 0
		  dAdχayDN(203) = 0
		  dAdχayDN(204) = 0
		  dAdχayDN(205) = 0
		  dAdχayDN(206) = 0
		  dAdχayDN(207) = 0
		  dAdχayDN(208) = 0
		  dAdχayDN(209) = 0
		  dAdχayDN(210) = 0
		  dAdχayDN(211) = 0
		  dAdχayDN(212) = 0
		  dAdχayDN(213) = 0
		  dAdχayDN(214) = 0
		  dAdχayDN(215) = 0
		  dAdχayDN(216) = 0
		  dAdχayDN(217) = 0
		  dAdχayDN(218) = 0
		  dAdχayDN(219) = 0
		  dAdχayDN(220) = 0
		  dAdχayDN(221) = 0
		  dAdχayDN(222) = 0
		  dAdχayDN(223) = 0
		  dAdχayDN(224) = 0
		  dAdχayDN(225) = 0
		  dAdχayDN(226) = 0
		  dAdχayDN(227) = 2*δ*sβ* c2p3
		  dAdχayDN(228) = − 14/3* δ*sβ* c1p4 + 20/3* δ*sβ *c2*c1p4
		  dAdχayDN(229) = − 2/3* δ*cβ *c1p3*s1 + 10/3 *δ*cβ* c2*c1p3*s1
		  dAdχayDN(230) = − 20/3* δ*cβ *c1p5*s1 
		  dAdχayDN(231) = − 10/3* δ*cβ *s1p3*c2*c1 − 2/3 *δ*cβ* s1p3*c1
		  dAdχayDN(232) = 20/3* δ*sβ *s1p4*c2 + 14/3* δ*sβ* s1p4
		  dAdχayDN(233) = − 20/3* δ*cβ* c1*s1p5
		  dAdχayDN(234) = 2*δ*sβ* c2*s2p2
		  dAdχayDN(235) = 10/3* δ*sβ* c2*s2p2
		  dAdχayDN(236) = −δ*cβ* s1p3
		  dAdχayDN(237) = − 5/4* δ*cβ* s2 − 1/4* δ*cβ* s6
		  dAdχayDN(238) = 0
		  dAdχayDN(239) = 0
		  dAdχayDN(240) = 0
		  dAdχayDN(241) = 0
		  dAdχayDN(242) = 0
		  dAdχayDN(243) = 0
		  dAdχayDN(244) = 0
		  dAdχayDN(245) = 0
		  dAdχayDN(246) = 0
		  dAdχayDN(247) = 0
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDAdχazDN()
		  // load the constant parameters
		  Var ιDN As Double  =self.ιDN 
		  Var β As Double  = self.β 
		  Var c2 As Double  = Trig.c2
		  Var s2 As Double = Trig.s2
		  Var c1 As Double = Trig.c1
		  Var s1   As Double = Trig.s1
		  Var c3   As Double = Trig.c3
		  Var s3  As Double  = Trig.s3
		  Var c4   As Double = Trig.c4
		  Var s4  As Double  = Trig.s4
		  Var c5  As Double  = Trig.c5
		  Var s5   As Double = Trig.s5
		  Var c6 As Double   = Trig.c6
		  Var s6   As Double = Trig.s6
		  Var c7   As Double = Trig.c7
		  Var s7 As Double   = Trig.s7
		  Var c8  As Double  = Trig.c8
		  Var s8  As Double  = Trig.s8
		  Var c9  As Double  = Trig.c9
		  Var s9   As Double = Trig.s9
		  Var c10  As Double  = Trig.c10
		  Var s10  As Double  = Trig.s10
		  Var c1p2  As Double  = Trig.c1p2
		  Var c1p3  As Double  = Trig.c1p3   
		  Var c1p4  As Double  =Trig. c1p4   
		  Var c1p5 As Double  = Trig.c1p5   
		  Var c1p6 As Double   = Trig.c1p6   
		  Var c1p7  As Double  = Trig.c1p7 
		  Var c1p8 As Double   = Trig.c1p8  
		  Var c1p9   As Double = Trig.c1p9  
		  Var c1p10  As Double  = Trig.c1p10
		  Var s1p2   As Double = Trig.s1p2   
		  Var s1p3  As Double  = Trig.s1p3   
		  Var s1p4As Double    = Trig.s1p4   
		  Var s1p5   As Double = Trig.s1p5  
		  Var s1p6 As Double   = Trig.s1p6   
		  Var s1p7 As Double   = Trig.s1p7   
		  Var s1p8  As Double  = Trig.s1p8   
		  Var s1p9 As Double   = Trig.s1p
		  Var s1p10 As Double  = Trig.s1p10
		  Var c2p2  As Double  = Trig.c2p2
		  Var c2p3   As Double = Trig.c2p3
		  Var c2p4 As Double  = Trig.c2p4
		  Var s2p2  As Double  = Trig.s2p2
		  Var s2p3  As Double  = Trig.s2p3
		  Var s2p4 As Double   = Trig.s2p4
		  Var s2p5 As Double   = Trig.s2p5
		  
		  // Define local β trig functions
		  Var cβ  As Double  = Trig.cβ
		  Var sβ  As Double  = Trig.sβ
		  Var c2β As Double   = Trig.c2β
		  Var s2β  As Double  = Trig.s2β
		  Var c3β As Double   = Trig.c3β
		  Var s3β  As Double  = Trig.s3β
		  Var c4β As Double   = Trig.c4β
		  Var s4β  As Double  = Trig.s4β
		  Var c5β As Double   = Trig.c5β
		  Var s5β As Double   = Trig.s5β
		  Var cβ2  As Double  = Trig.cβ2
		  Var cβ3 As Double   = Trig.cβ3
		  Var sβ2  As Double  = Trig.sβ2
		  Var sβ3  As Double  = Trig.sβ3
		  
		  
		  // Calculate dAdχazDN
		  dAdχazDN(0) = 0
		  dAdχazDN(1) = 0
		  dAdχazDN(2) = 0
		  dAdχazDN(3) = 0
		  dAdχazDN(4) = 0
		  dAdχazDN(5) = 0
		  dAdχazDN(6) = 0
		  dAdχazDN(7) = 0
		  dAdχazDN(8) = 0
		  dAdχazDN(9) = 0
		  dAdχazDN(10) = 0
		  dAdχazDN(11) = 0
		  dAdχazDN(12) = 0
		  dAdχazDN(13) = 0
		  dAdχazDN(14) = 0
		  dAdχazDN(15) = 0
		  dAdχazDN(16) = 0
		  dAdχazDN(17) = 0
		  dAdχazDN(18) = 0
		  dAdχazDN(19) = 0
		  dAdχazDN(20) = 0
		  dAdχazDN(21) = 0
		  dAdχazDN(22) = 0
		  dAdχazDN(23) = 0
		  dAdχazDN(24) = 0
		  dAdχazDN(25) = 0
		  dAdχazDN(26) = 0
		  dAdχazDN(27) = 0
		  dAdχazDN(28) = 0
		  dAdχazDN(29) = 0
		  dAdχazDN(30) = 0
		  dAdχazDN(31) = 0
		  dAdχazDN(32) = 0
		  dAdχazDN(33) = 0
		  dAdχazDN(34) = 0
		  dAdχazDN(35) = 0
		  dAdχazDN(36) = 0
		  dAdχazDN(37) = −sβ *c1p2
		  dAdχazDN(38) = −sβ *s1p2
		  dAdχazDN(39) = 0
		  dAdχazDN(40) = 0
		  dAdχazDN(41) = 0
		  dAdχazDN(42) = 0
		  dAdχazDN(43) = 0
		  dAdχazDN(44) = 0
		  dAdχazDN(45) = 0
		  dAdχazDN(46) = 0
		  dAdχazDN(47) = 0
		  dAdχazDN(48) = 0
		  dAdχazDN(49) = 0
		  dAdχazDN(50) = 0
		  dAdχazDN(51) = 0
		  dAdχazDN(52) = 0
		  dAdχazDN(53) = 0
		  dAdχazDN(54) = 0
		  dAdχazDN(55) = 0
		  dAdχazDN(56) = 0
		  dAdχazDN(57) = 0
		  dAdχazDN(58) = 0
		  dAdχazDN(59) = 0
		  dAdχazDN(60) = 0
		  dAdχazDN(61) = 0
		  dAdχazDN(62) = 0
		  dAdχazDN(63) = 0
		  dAdχazDN(64) = 0
		  dAdχazDN(65) = 0
		  dAdχazDN(66) = 0
		  dAdχazDN(67) = 0
		  dAdχazDN(68) = 0
		  dAdχazDN(69) = 0
		  dAdχazDN(70) = 0
		  dAdχazDN(71) = 0
		  dAdχazDN(72) = 0
		  dAdχazDN(73) = 0
		  dAdχazDN(74) = 0
		  dAdχazDN(75) = 0
		  dAdχazDN(76) = 0
		  dAdχazDN(77) = 0
		  dAdχazDN(78) = 0
		  dAdχazDN(79) = 0
		  dAdχazDN(80) = 0
		  dAdχazDN(81) = 0
		  dAdχazDN(82) = 0
		  dAdχazDN(83) = 0
		  dAdχazDN(84) = 0
		  dAdχazDN(85) = 0
		  dAdχazDN(86) = 0
		  dAdχazDN(87) = 0
		  dAdχazDN(88) = 0
		  dAdχazDN(89) = 0
		  dAdχazDN(90) = 0
		  dAdχazDN(91) = 0
		  dAdχazDN(92) = 0
		  dAdχazDN(93) = 0
		  dAdχazDN(94) = 0
		  dAdχazDN(95) = 0
		  dAdχazDN(96) = 0
		  dAdχazDN(97) = 0
		  dAdχazDN(98) = 0
		  dAdχazDN(99) = 0
		  dAdχazDN(100) = 0
		  dAdχazDN(101) = 0
		  dAdχazDN(102) = 0
		  dAdχazDN(103) = 0
		  dAdχazDN(104) = 0
		  dAdχazDN(105) = 0
		  dAdχazDN(106) = 0
		  dAdχazDN(107) = 0
		  dAdχazDN(108) = −3*δ*c1p4 − δ*c2β* c1p4 + 5*δ*c1p4*c2 + 5/3* δ*c2β*c1p4*c2
		  dAdχazDN(109) = 0
		  dAdχazDN(110) = 0
		  dAdχazDN(111) =  20/3* δ*s2β* c2*c1p3*s1 − 2*δ*s2β c1p3*s1
		  dAdχazDN(112) =− 20/3* δ*s2β* c2*c1*s1p3 − 2*δ*s2β* c1*s3
		  dAdχazDN(113) = 5/3* δ*c2β* c2*s1p4 + δ*c2β* s1p4 + 5*δ*c2*s1p4 + 3*δ*s1p4 
		  dAdχazDN(114) = − 3*δ*sβ2* c2*s2p2
		  dAdχazDN(115) = 0
		  dAdχazDN(116) = +5*δ*sβ2* c2*s2p2
		  dAdχazDN(117) = 3/2* δ*c2*s2p2 + 1/2* δ*c2β* c2*s2p2
		  dAdχazDN(118) = 0
		  dAdχazDN(119) = 0
		  dAdχazDN(120) = 0
		  dAdχazDN(121) = 0
		  dAdχazDN(122) = 0
		  dAdχazDN(123) = 0
		  dAdχazDN(124) = 0
		  dAdχazDN(125) = 0
		  dAdχazDN(126) = 0
		  dAdχazDN(127) = 0
		  dAdχazDN(128) = 0
		  dAdχazDN(129) = 0
		  dAdχazDN(130) = 0
		  dAdχazDN(131) = 0
		  dAdχazDN(132) = 0
		  dAdχazDN(133) = 0
		  dAdχazDN(134) = 0
		  dAdχazDN(135) = 0
		  dAdχazDN(136) = 0
		  dAdχazDN(137) = 0
		  dAdχazDN(138) = 0
		  dAdχazDN(139) = 0
		  dAdχazDN(140) = 0
		  dAdχazDN(141) = 0
		  dAdχazDN(142) = 0
		  dAdχazDN(143) = 0
		  dAdχazDN(144) = 0
		  dAdχazDN(145) = 0
		  dAdχazDN(146) = 0
		  dAdχazDN(147) = 0
		  dAdχazDN(148) = 0
		  dAdχazDN(149) = 0
		  dAdχazDN(150) = 0
		  dAdχazDN(151) = 0
		  dAdχazDN(152) = 0
		  dAdχazDN(153) = 0
		  dAdχazDN(154) = 0
		  dAdχazDN(155) = 0
		  dAdχazDN(156) = 0
		  dAdχazDN(157) = 0
		  dAdχazDN(158) = 0
		  dAdχazDN(159) = 0
		  dAdχazDN(160) = 0
		  dAdχazDN(161) = 0
		  dAdχazDN(162) = 0
		  dAdχazDN(163) = 0
		  dAdχazDN(164) = 0
		  dAdχazDN(165) = − 1/2* cβ* sβ + 1/2* cβ* sβ *c2
		  dAdχazDN(166) = −sβ2* s2 
		  dAdχazDN(167) = − 1/2 *cβ* sβ − 1/2 *cβ *sβ* c2 
		  dAdχazDN(168) = 0
		  dAdχazDN(169) = 0
		  dAdχazDN(170) = 0
		  dAdχazDN(171) = 0
		  dAdχazDN(172) = 0
		  dAdχazDN(173) = 0
		  dAdχazDN(174) = 0
		  dAdχazDN(175) = 0
		  dAdχazDN(176) = 0
		  dAdχazDN(177) = 0
		  dAdχazDN(178) = 0
		  dAdχazDN(179) = 0
		  dAdχazDN(180) = 0
		  dAdχazDN(181) = 0
		  dAdχazDN(182) = 0
		  dAdχazDN(183) = 0
		  dAdχazDN(184) = 0
		  dAdχazDN(185) = 0
		  dAdχazDN(186) = 0
		  dAdχazDN(187) = 0
		  dAdχazDN(188) = 0
		  dAdχazDN(189) = 0
		  dAdχazDN(190) = 0
		  dAdχazDN(191) = 0
		  dAdχazDN(192) = 0
		  dAdχazDN(193) = 0
		  dAdχazDN(194) = 0
		  dAdχazDN(195) = 0
		  dAdχazDN(196) = 0
		  dAdχazDN(197) = 0
		  dAdχazDN(198) = 0
		  dAdχazDN(199) = 0
		  dAdχazDN(200) = 0
		  dAdχazDN(201) = 0
		  dAdχazDN(202) = 0
		  dAdχazDN(203) = 0
		  dAdχazDN(204) = 0
		  dAdχazDN(205) = 0
		  dAdχazDN(206) = 0
		  dAdχazDN(207) = 0
		  dAdχazDN(208) = 0
		  dAdχazDN(209) = 0
		  dAdχazDN(210) = 0
		  dAdχazDN(211) = 0
		  dAdχazDN(212) = 0
		  dAdχazDN(213) = 0
		  dAdχazDN(214) = 0
		  dAdχazDN(215) = 0
		  dAdχazDN(216) = 0
		  dAdχazDN(217) = 0
		  dAdχazDN(218) = 0
		  dAdχazDN(219) = 0
		  dAdχazDN(220) = 0
		  dAdχazDN(221) = 0
		  dAdχazDN(222) = 0
		  dAdχazDN(223) = 0
		  dAdχazDN(224) = 0
		  dAdχazDN(225) = 0
		  dAdχazDN(226) = 0
		  dAdχazDN(227) = 0
		  dAdχazDN(228) = 0
		  dAdχazDN(229) = 0
		  dAdχazDN(230) = 0
		  dAdχazDN(231) = 0
		  dAdχazDN(232) = 0
		  dAdχazDN(233) = 0
		  dAdχazDN(234) = 0
		  dAdχazDN(235) = 0
		  dAdχazDN(236) = 0
		  dAdχazDN(237) = 0
		  dAdχazDN(238) = −2*δ*sβ *s2*c4 
		  dAdχazDN(239) = 2*δ*cβ* c2*s2p2
		  dAdχazDN(240) = 0
		  dAdχazDN(241) = − 40/3* δ*sβ *s1p3*c2*c1 − 4*δ*sβ* s1p3*c1 
		  dAdχazDN(242) = 20/3* δ*cβ *s1p4*c2 + 4*δ*cβ* s1p4 
		  dAdχazDN(243) = 0
		  dAdχazDN(244) = 0
		  dAdχazDN(245) = + 40/3* δ*sβ* s1*c2*c1p3 − 4*δ*sβ *s1*c1p3
		  
		  dAdχazDN(246) = 20/3 *δ*cβ* c2*c1p4 − 4*δ*cβ *c1p4
		  dAdχazDN(247) = 0
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDAdχsxDN()
		  // load the constant parameters
		  Var ιDN As Double  =self.ιDN 
		  Var β As Double  = self.β 
		  Var c2 As Double  = Trig.c2
		  Var s2 As Double = Trig.s2
		  Var c1 As Double = Trig.c1
		  Var s1   As Double = Trig.s1
		  Var c3   As Double = Trig.c3
		  Var s3  As Double  = Trig.s3
		  Var c4   As Double = Trig.c4
		  Var s4  As Double  = Trig.s4
		  Var c5  As Double  = Trig.c5
		  Var s5   As Double = Trig.s5
		  Var c6 As Double   = Trig.c6
		  Var s6   As Double = Trig.s6
		  Var c7   As Double = Trig.c7
		  Var s7 As Double   = Trig.s7
		  Var c8  As Double  = Trig.c8
		  Var s8  As Double  = Trig.s8
		  Var c9  As Double  = Trig.c9
		  Var s9   As Double = Trig.s9
		  Var c10  As Double  = Trig.c10
		  Var s10  As Double  = Trig.s10
		  Var c1p2  As Double  = Trig.c1p2
		  Var c1p3  As Double  = Trig.c1p3   
		  Var c1p4  As Double  =Trig. c1p4   
		  Var c1p5 As Double  = Trig.c1p5   
		  Var c1p6 As Double   = Trig.c1p6   
		  Var c1p7  As Double  = Trig.c1p7 
		  Var c1p8 As Double   = Trig.c1p8  
		  Var c1p9   As Double = Trig.c1p9  
		  Var c1p10  As Double  = Trig.c1p10
		  Var s1p2   As Double = Trig.s1p2   
		  Var s1p3  As Double  = Trig.s1p3   
		  Var s1p4 As Double    = Trig.s1p4   
		  Var s1p5   As Double = Trig.s1p5  
		  Var s1p6 As Double   = Trig.s1p6   
		  Var s1p7 As Double   = Trig.s1p7   
		  Var s1p8  As Double  = Trig.s1p8   
		  Var s1p9 As Double   = Trig.s1p9
		  Var s1p10 As Double  = Trig.s1p10
		  Var c2p2  As Double  = Trig.c2p2
		  Var c2p3   As Double = Trig.c2p3
		  Var c2p4 As Double  = Trig.c2p4
		  Var s2p2  As Double  = Trig.s2p2
		  Var s2p3  As Double  = Trig.s2p3
		  Var s2p4 As Double   = Trig.s2p4
		  Var s2p5 As Double   = Trig.s2p5
		  
		  // Define local β trig functions
		  Var cβ  As Double  = Trig.cβ
		  Var sβ  As Double  = Trig.sβ
		  Var c2β As Double   = Trig.c2β
		  Var s2β  As Double  = Trig.s2β
		  Var c3β As Double   = Trig.c3β
		  Var s3β  As Double  = Trig.s3β
		  Var c4β As Double   = Trig.c4β
		  Var s4β  As Double  = Trig.s4β
		  Var c5β As Double   = Trig.c5β
		  Var s5β As Double   = Trig.s5β
		  Var cβ2  As Double  = Trig.cβ2
		  Var cβ3 As Double   = Trig.cβ3
		  Var sβ2  As Double  = Trig.sβ2
		  Var sβ3  As Double  = Trig.sβ3
		  
		  //Calculate the dAdχsxDN
		  dAdχsxDN(0) = 0
		  dAdχsxDN(1) = 0
		  dAdχsxDN(2) = 0
		  dAdχsxDN(3) = 0
		  dAdχsxDN(4) = 0
		  dAdχsxDN(5) = 0
		  dAdχsxDN(6) = 0
		  dAdχsxDN(7) = 0
		  dAdχsxDN(8) = 0
		  dAdχsxDN(9) = 0
		  dAdχsxDN(10) = 0
		  dAdχsxDN(11) = 0
		  dAdχsxDN(12) = 0
		  dAdχsxDN(13) = 0
		  dAdχsxDN(14) = 0
		  dAdχsxDN(15) = 0
		  dAdχsxDN(16) = 0
		  dAdχsxDN(17) = 0
		  dAdχsxDN(18) = 0
		  dAdχsxDN(19) = 0
		  dAdχsxDN(20) = 0
		  dAdχsxDN(21) = 0
		  dAdχsxDN(22) = 0
		  dAdχsxDN(23) = 0
		  dAdχsxDN(24) = 0
		  dAdχsxDN(25) = 0
		  dAdχsxDN(26) = 0
		  dAdχsxDN(27) = 0
		  dAdχsxDN(28) = 0
		  dAdχsxDN(29) = 0
		  dAdχsxDN(30) = 0
		  dAdχsxDN(31) = 0
		  dAdχsxDN(32) = 0
		  dAdχsxDN(33) = 0
		  dAdχsxDN(34) = 0
		  dAdχsxDN(35) = 0
		  dAdχsxDN(36) = 0
		  dAdχsxDN(37) = 0
		  dAdχsxDN(38) = 0
		  dAdχsxDN(39) = 0
		  dAdχsxDN(40) = 0
		  dAdχsxDN(41) = 0
		  dAdχsxDN(42) = δ*cβ* c1p2
		  dAdχsxDN(43) = 1/2 *δ*cβ − 1/2* δ*cβ *c2
		  dAdχsxDN(44) = 0
		  dAdχsxDN(45) = 0
		  dAdχsxDN(46) = 0
		  dAdχsxDN(47) = 0
		  dAdχsxDN(48) = 0
		  dAdχsxDN(49) = 0
		  dAdχsxDN(50) = 0
		  dAdχsxDN(51) = 0
		  dAdχsxDN(52) = 0
		  dAdχsxDN(53) = 0
		  dAdχsxDN(54) = 0
		  dAdχsxDN(55) = 0
		  dAdχsxDN(56) = 0
		  dAdχsxDN(57) = 0
		  dAdχsxDN(58) = 0
		  dAdχsxDN(59) = 0
		  dAdχsxDN(60) = 0
		  dAdχsxDN(61) = 0
		  dAdχsxDN(62) = 0
		  dAdχsxDN(63) = 0
		  dAdχsxDN(64) = 0
		  dAdχsxDN(65) = 0
		  dAdχsxDN(66) = 0
		  dAdχsxDN(67) = 0
		  dAdχsxDN(68) = 0
		  dAdχsxDN(69) = 0
		  dAdχsxDN(70) = 0
		  dAdχsxDN(71) = 0
		  dAdχsxDN(72) = 0
		  dAdχsxDN(73) = 0
		  dAdχsxDN(74) = 0
		  dAdχsxDN(75) = 0
		  dAdχsxDN(76) = 0
		  dAdχsxDN(77) = 0
		  dAdχsxDN(78) = 0
		  dAdχsxDN(79) = 0
		  dAdχsxDN(80) = 0
		  dAdχsxDN(81) = 0
		  dAdχsxDN(82) = 0
		  dAdχsxDN(83) = 0
		  dAdχsxDN(84) = 0
		  dAdχsxDN(85) = 2*c2p3*cβ*sβ  -η*cβ *sβ*c2p3 
		  dAdχsxDN(86) =  -1/3* η*  s2β* c1p4* c2  - 10/38* s2β* c1p4* c2  - 19/6 *η * s2β* c1p4  +7/3 *s2β *c1p4   
		  dAdχsxDN(87) =  1/2* η * c1p5*s1  +1/6 *η * c2β*c1p5*s1  +  5* c1p5*s1  +5/3* c2β*c1p5*s1
		  dAdχsxDN(88) = 1/2* η *c1* s1p5  + 1 /6 * η* c2β *c1 *s1p5   +  5 *c1* s1p5  + 5 /3* c2β *c1 *s1p5
		  dAdχsxDN(89) =  7 /12* η * c2β* s1* c1p3* c2 +  35 /6* c2β* c1p3* c2* s1   +  79 /12* η* c2β *s1* c1p3    -  13 /6* c2β* s1* c1p3   -  1 /4* η * s1* c1p3 *c2  -  5 /2*  s1* c1p3* c2-  17 /4* η * s1* c1p3  +  3 /2*  s1* c1p3  
		  dAdχsxDN(90) =  7 /12* δ* η *c2β* c2* c1p3 *s1   +  35 /6* δ* c2β* c2* c1p3 *s1   +  79 /12* δ* η *c2β* c1p3 *s1   -  13 /6* δ *c2β* c1p3* s1   -  1 /4* δ* η* c2* c1p3* s1*   -  5 /2* δ* c2* c1p3 *s1*   -  17 /4* δ *η *c1p3* s1+  3 /2* δ* c1p3* s1
		  dAdχsxDN(91) = − 1/3*η*s2β*c2*s1p4 − 10/3* s2β *c2*s1p4 + 19/6 *η*s2β *s1p4− 7/3 *s2β*s1p4 
		  dAdχsxDN(92) = 0
		  dAdχsxDN(93) = 3/4* s2p3 + 1/4 *c2β* s2p3 − 3/8 *η*s2p3 − 1/8 *η*c2β *s2p3
		  dAdχsxDN(94) = 10/3*cβ*sβ*c2*s2p2 + 1/3 *η*cβ* sβ* c2*s2p2 
		  dAdχsxDN(95) = 1/2* η*s2β* c2*s2p2 − s2β *c2*s2p2
		  dAdχsxDN(96) = − 11/16 *c2β* s2 − 3/4 *s2p3 − 7/16 *c2β* s6 + 11/32 *η*c2β* s2 + 3/8* η*s2p3 + 7/32 *η*c2β* s6
		  dAdχsxDN(97) = 0
		  dAdχsxDN(98) = 0
		  dAdχsxDN(99) = 0
		  dAdχsxDN(100) = 0
		  dAdχsxDN(101) = 0
		  dAdχsxDN(102) = 0
		  dAdχsxDN(103) = 0
		  dAdχsxDN(104) = 0
		  dAdχsxDN(105) = 0
		  dAdχsxDN(106) = 0
		  dAdχsxDN(107) = 0
		  dAdχsxDN(108) = 0
		  dAdχsxDN(109) = 0
		  dAdχsxDN(110) = 0
		  dAdχsxDN(111) = 0
		  dAdχsxDN(112) = 0
		  dAdχsxDN(113) = 0
		  dAdχsxDN(114) = 0
		  dAdχsxDN(115) = 0
		  dAdχsxDN(116) = 0
		  dAdχsxDN(117) = 0
		  dAdχsxDN(118) = 0
		  dAdχsxDN(119) = 0
		  dAdχsxDN(120) = 0
		  dAdχsxDN(121) = 0
		  dAdχsxDN(122) = 0
		  dAdχsxDN(123) = 0
		  dAdχsxDN(124) = 0
		  dAdχsxDN(125) = 0
		  dAdχsxDN(126) = 0
		  dAdχsxDN(127) = 0
		  dAdχsxDN(128) = 0
		  dAdχsxDN(129) = 0
		  dAdχsxDN(130) = 0
		  dAdχsxDN(131) = 0
		  dAdχsxDN(132) = 0
		  dAdχsxDN(133) = 0
		  dAdχsxDN(134) = 0
		  dAdχsxDN(135) = 0
		  dAdχsxDN(136) = 0
		  dAdχsxDN(137) = 0
		  dAdχsxDN(138) = 0
		  dAdχsxDN(139) = 0
		  dAdχsxDN(140) = 0
		  dAdχsxDN(141) = 0
		  dAdχsxDN(142) = 0
		  dAdχsxDN(143) = 0
		  dAdχsxDN(144) = 0
		  dAdχsxDN(145) = 0
		  dAdχsxDN(146) = 0
		  dAdχsxDN(147) = 0
		  dAdχsxDN(148) = 0
		  dAdχsxDN(149) = 0
		  dAdχsxDN(150) = 0
		  dAdχsxDN(151) = 0
		  dAdχsxDN(152) = 0
		  dAdχsxDN(153) = 0
		  dAdχsxDN(154) = 0
		  dAdχsxDN(155) = 0
		  dAdχsxDN(156) = 0
		  dAdχsxDN(157) = 0
		  dAdχsxDN(158) = 0
		  dAdχsxDN(159) = 0
		  dAdχsxDN(160) = 0
		  dAdχsxDN(161) = 0
		  dAdχsxDN(162) = 0
		  dAdχsxDN(163) = 0
		  dAdχsxDN(164) = 0
		  dAdχsxDN(165) = 0
		  dAdχsxDN(166) = 0
		  dAdχsxDN(167) = 0
		  dAdχsxDN(168) = 0
		  dAdχsxDN(169) = 0
		  dAdχsxDN(170) = − 1/2 *δ*cβ2 *c2 + 1/2* δ*cβ2 
		  dAdχsxDN(171) = δ*cβ* sβ* s2 
		  dAdχsxDN(172) = 1/2 *δ*cβ2* c2 + 1/2* δ*cβ2
		  dAdχsxDN(173) = 0
		  dAdχsxDN(174) = 0
		  dAdχsxDN(175) = 0
		  dAdχsxDN(176) = 0
		  dAdχsxDN(177) = 0
		  dAdχsxDN(178) = 0
		  dAdχsxDN(179) = 0
		  dAdχsxDN(180) = 0
		  dAdχsxDN(181) = 0
		  dAdχsxDN(182) = 0
		  dAdχsxDN(183) = 0
		  dAdχsxDN(184) = 0
		  dAdχsxDN(185) = 0
		  dAdχsxDN(186) = 0
		  dAdχsxDN(187) = 0
		  dAdχsxDN(188) = 0
		  dAdχsxDN(189) = 0
		  dAdχsxDN(190) = 0
		  dAdχsxDN(191) = 0
		  dAdχsxDN(192) = 0
		  dAdχsxDN(193) = 0
		  dAdχsxDN(194) = 0
		  dAdχsxDN(195) = 0
		  dAdχsxDN(196) = 0
		  dAdχsxDN(197) = 0
		  dAdχsxDN(198) = 0
		  dAdχsxDN(199) = 0
		  dAdχsxDN(200) = 0
		  dAdχsxDN(201) = 0
		  dAdχsxDN(202) = 0
		  dAdχsxDN(203) = 0
		  dAdχsxDN(204) = 0
		  dAdχsxDN(205) = 0
		  dAdχsxDN(206) = 0
		  dAdχsxDN(207) = 0
		  dAdχsxDN(208) = 0
		  dAdχsxDN(209) = 0
		  dAdχsxDN(210) = 0
		  dAdχsxDN(211) = 0
		  dAdχsxDN(212) = 0
		  dAdχsxDN(213) = 0
		  dAdχsxDN(214) = 0
		  dAdχsxDN(215) = 0
		  dAdχsxDN(216) = 0
		  dAdχsxDN(217) = 3/4 *η*cβ* s2 + 1/4 *η*cβ *c4*s2 − 3/2 *cβ* s2 − 1/2* cβ* c4*s2
		  dAdχsxDN(218) = η*sβ *s2p2*c2 − 2sβ* s2p2*c2
		  dAdχsxDN(219) = −2*η*cβ *s2p3 + 4*cβ *s2p3
		  dAdχsxDN(220) = − 1/3 *η*cβ *s1p3*c2*c1 − 10/3 *cβ *s1p3*c2*c1+4*η*c3β *s1p3*c1 − 5/3 *η*cβ *s1p3*c1 − 2/3 *cβ *s1p3*c1
		  dAdχsxDN(221) = − 2/3* η*sβ *s1p4*c2 − 20/3 *sβ *s1p4*c2+η*s3β *s1p4 + 10/3 *η*sβ *s1p4 − 14/3 *sβ *s1p4
		  dAdχsxDN(222) = 2/3 *η*cβ *c1*s1p5 + 20/3 *cβ *c1*s5
		  dAdχsxDN(223) = 3*η*sβ *c2β *s2p2 − 7/6 *η*sβ *s2p2 + 1/3 *sβ *s2
		  dAdχsxDN(224) = 1/3 *η*cβ *c2*c1p3*s1 + 10/3 *cβ *c2*c1p3*s1 + 4*η*c3β *c1p3*s1− 5/3 *η*cβ *c1p3*s1 − 2/3 *cβ *c1p3*s1
		  dAdχsxDN(225) = − 2/3 *η*sβ *c2*c1p4 − 20/3 *sβ* c2*c1p4−η*s3β* c1p4 − 10/3 *η*sβ *c1p4 + 14/3 *sβ *c4
		  dAdχsxDN(226) = 20/3* cβ* c1p5*s1 + 2/3 *η*cβ* c1p5*s1
		  dAdχsxDN(227) = 0
		  dAdχsxDN(228) = 0
		  dAdχsxDN(229) = 0
		  dAdχsxDN(230) = 0
		  dAdχsxDN(231) = 0
		  dAdχsxDN(232) = 0
		  dAdχsxDN(233) = 0
		  dAdχsxDN(234) = 0
		  dAdχsxDN(235) = 0
		  dAdχsxDN(236) = 0
		  dAdχsxDN(237) = 0
		  dAdχsxDN(238) = 0
		  dAdχsxDN(239) = 0
		  dAdχsxDN(240) = 0
		  dAdχsxDN(241) = 0
		  dAdχsxDN(242) = 0
		  dAdχsxDN(243) = 0
		  dAdχsxDN(244) = 0
		  dAdχsxDN(245) = 0
		  dAdχsxDN(246) = 0
		  dAdχsxDN(247) = 0
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDAdχsyDN()
		  // load the constant parameters
		  Var ιDN As Double  =self.ιDN 
		  Var β As Double  = self.β 
		  Var c2 As Double  = Trig.c2
		  Var s2 As Double = Trig.s2
		  Var c1 As Double = Trig.c1
		  Var s1   As Double = Trig.s1
		  Var c3   As Double = Trig.c3
		  Var s3  As Double  = Trig.s3
		  Var c4   As Double = Trig.c4
		  Var s4  As Double  = Trig.s4
		  Var c5  As Double  = Trig.c5
		  Var s5   As Double = Trig.s5
		  Var c6 As Double   = Trig.c6
		  Var s6   As Double = Trig.s6
		  Var c7   As Double = Trig.c7
		  Var s7 As Double   = Trig.s7
		  Var c8  As Double  = Trig.c8
		  Var s8  As Double  = Trig.s8
		  Var c9  As Double  = Trig.c9
		  Var s9   As Double = Trig.s9
		  Var c10  As Double  = Trig.c10
		  Var s10  As Double  = Trig.s10
		  Var c1p2  As Double  = Trig.c1p2
		  Var c1p3  As Double  = Trig.c1p3   
		  Var c1p4  As Double  =Trig. c1p4   
		  Var c1p5 As Double  = Trig.c1p5   
		  Var c1p6 As Double   = Trig.c1p6   
		  Var c1p7  As Double  = Trig.c1p7 
		  Var c1p8 As Double   = Trig.c1p8  
		  Var c1p9   As Double = Trig.c1p9  
		  Var c1p10  As Double  = Trig.c1p10
		  Var s1p2   As Double = Trig.s1p2   
		  Var s1p3  As Double  = Trig.s1p3   
		  Var s1p4As Double    = Trig.s1p4   
		  Var s1p5   As Double = Trig.s1p5  
		  Var s1p6 As Double   = Trig.s1p6   
		  Var s1p7 As Double   = Trig.s1p7   
		  Var s1p8  As Double  = Trig.s1p8   
		  Var s1p9 As Double   = Trig.s1p9
		  Var s1p10 As Double  = Trig.s1p10
		  Var c2p2  As Double  = Trig.c2p2
		  Var c2p3   As Double = Trig.c2p3
		  Var c2p4 As Double  = Trig.c2p4
		  Var s2p2  As Double  = Trig.s2p2
		  Var s2p3  As Double  = Trig.s2p3
		  Var s2p4 As Double   = Trig.s2p4
		  Var s2p5 As Double   = Trig.s2p5
		  
		  // Define local β trig functions
		  Var cβ  As Double  = Trig.cβ
		  Var sβ  As Double  = Trig.sβ
		  Var c2β As Double   = Trig.c2β
		  Var s2β  As Double  = Trig.s2β
		  Var c3β As Double   = Trig.c3β
		  Var s3β  As Double  = Trig.s3β
		  Var c4β As Double   = Trig.c4β
		  Var s4β  As Double  = Trig.s4β
		  Var c5β As Double   = Trig.c5β
		  Var s5β As Double   = Trig.s5β
		  Var cβ2  As Double  = Trig.cβ2
		  Var cβ3 As Double   = Trig.cβ3
		  Var sβ2  As Double  = Trig.sβ2
		  Var sβ3  As Double  = Trig.sβ3
		  
		  
		  //Calculate the dAdχsyDN
		  dAdχsyDN(0) = 0
		  dAdχsyDN(1) = 0
		  dAdχsyDN(2) = 0
		  dAdχsyDN(3) = 0
		  dAdχsyDN(4) = 0
		  dAdχsyDN(5) = 0
		  dAdχsyDN(6) = 0
		  dAdχsyDN(7) = 0
		  dAdχsyDN(8) = 0
		  dAdχsyDN(9) = 0
		  dAdχsyDN(10) = 0
		  dAdχsyDN(11) = 0
		  dAdχsyDN(12) = 0
		  dAdχsyDN(13) = 0
		  dAdχsyDN(14) = 0
		  dAdχsyDN(15) = 0
		  dAdχsyDN(16) = 0
		  dAdχsyDN(17) = 0
		  dAdχsyDN(18) = 0
		  dAdχsyDN(19) = 0
		  dAdχsyDN(20) = 0
		  dAdχsyDN(21) = 0
		  dAdχsyDN(22) = 0
		  dAdχsyDN(23) = 0
		  dAdχsyDN(24) = 0
		  dAdχsyDN(25) = 0
		  dAdχsyDN(26) = 0
		  dAdχsyDN(27) = 0
		  dAdχsyDN(28) = 0
		  dAdχsyDN(29) = 0
		  dAdχsyDN(30) = 0
		  dAdχsyDN(31) = 0
		  dAdχsyDN(32) = 0
		  dAdχsyDN(33) = 0
		  dAdχsyDN(34) = 0
		  dAdχsyDN(35) = 0
		  dAdχsyDN(36) = 0
		  dAdχsyDN(37) = 0
		  dAdχsyDN(38) = 0
		  dAdχsyDN(39) = 0
		  dAdχsyDN(40) = 0
		  dAdχsyDN(41) = 0
		  dAdχsyDN(42) = 0
		  dAdχsyDN(43) = 0
		  dAdχsyDN(44) = −δ*cβ *s1p2
		  dAdχsyDN(45) = −δ*sβ *s2
		  dAdχsyDN(46) = −δ*cβ *c1p2
		  dAdχsyDN(47) = 0
		  dAdχsyDN(48) = 0
		  dAdχsyDN(49) = 0
		  dAdχsyDN(50) = 0
		  dAdχsyDN(51) = 0
		  dAdχsyDN(52) = 0
		  dAdχsyDN(53) = 0
		  dAdχsyDN(54) = 0
		  dAdχsyDN(55) = 0
		  dAdχsyDN(56) = 0
		  dAdχsyDN(57) = 0
		  dAdχsyDN(58) = 0
		  dAdχsyDN(59) = 0
		  dAdχsyDN(60) = 0
		  dAdχsyDN(61) = 0
		  dAdχsyDN(62) = 0
		  dAdχsyDN(63) = 0
		  dAdχsyDN(64) = 0
		  dAdχsyDN(65) = 0
		  dAdχsyDN(66) = 0
		  dAdχsyDN(67) = 0
		  dAdχsyDN(68) = 0
		  dAdχsyDN(69) = 0
		  dAdχsyDN(70) = 0
		  dAdχsyDN(71) = 0
		  dAdχsyDN(72) = 0
		  dAdχsyDN(73) = 0
		  dAdχsyDN(74) = 0
		  dAdχsyDN(75) = 0
		  dAdχsyDN(76) = 0
		  dAdχsyDN(77) = 0
		  dAdχsyDN(78) = 0
		  dAdχsyDN(79) = 0
		  dAdχsyDN(80) = 0
		  dAdχsyDN(81) = 0
		  dAdχsyDN(82) = 0
		  dAdχsyDN(83) = 0
		  dAdχsyDN(84) = 0
		  dAdχsyDN(85) = 0
		  dAdχsyDN(86) = 0
		  dAdχsyDN(87) = 0
		  dAdχsyDN(88) = 0
		  dAdχsyDN(89) = 0
		  dAdχsyDN(90) = 0
		  dAdχsyDN(91) = 0
		  dAdχsyDN(92) = 0
		  dAdχsyDN(93) = 0
		  dAdχsyDN(94) = 0
		  dAdχsyDN(95) = 0
		  dAdχsyDN(96) = 0
		  dAdχsyDN(97) = 15/8 *s2 − 3/8* c2β* s2 + 9/8 *c4*s2 − 5/8 *c2β* c4*s2 − 15/16 *η*s2+ 3/16 *η*c2β* s2 − 9/16* η*c4*s2 + 5/16 *η*c2β* c4*s2
		  dAdχsyDN(98) = η*cβ* sβ* c2*s2p2 − 2*cβ* sβ* c2*s2p2
		  dAdχsyDN(99) = 3/4 *s2p3 + 1/4 *c2β* s2p3 − 3/8 *η*s2p3 − 1/8 *η*c2β* s2p3
		  dAdχsyDN(100) = − 5/12 *η*c2β *c2*c1*s1p3− 25/6 *c2β* c2*c1*s1p3 − 31/12 *η*c2β *c1*s1p3 + 55/12 *c2β *c1*s1p3+ 3/4 *η*c2*c1*s1p3 + 15/2* c2*c1*s1p3 + 1/4 *η*c1*s1p3
		  dAdχsyDN(101) = − 7/3 *s2β *s1p4 − 10/3 *s2β* c1p4*c2 − 5/6 *η*s2β *s1p4 − 1/3* η*s2β *c2*s1p4
		  dAdχsyDN(102) = 5*c1*s1p5 + 5/3 *c2β *c1*s1p5 + 1/2 *η*c1*s1p5 + 1/6* η*c2β* c1*s1p5
		  dAdχsyDN(103) = − 1/3 *cβ *sβ *s2p2 − 11/6 *η*cβ *sβ *s2p2
		  dAdχsyDN(104) = 1/4* η*c1p3*s1 − 31/12 *η*c2β* c1p3*s1 − 3/4 *η*c1p3*c2*s1 + 5/12* η*c2β* c1p3*c2*s1
		  + 5/2* c1p3*s1 − 11/6 *c2β *c1p3*s1 − 15/2* c1p3*c2*s1 + 25/6* c2β* c1p3*c2*s1
		  dAdχsyDN(105) = 7/3 *s2β *c1p4 − 10/3 *s2β *c1p4*c2 + 5/6 *η*s2β *c1p4 − 1/3 *η*s2β *c1p4*c2
		  dAdχsyDN(106) = 1/2* η*c1p5*s1 + 1/6 *η*c2β* c1p5*s1 + 5*c1p5*s1 + 5/3 *c2β* c1p5*s1
		  dAdχsyDN(107) = 0
		  dAdχsyDN(108) = 0
		  dAdχsyDN(109) = 0
		  dAdχsyDN(110) = 0
		  dAdχsyDN(111) = 0
		  dAdχsyDN(112) = 0
		  dAdχsyDN(113) = 0
		  dAdχsyDN(114) = 0
		  dAdχsyDN(115) = 0
		  dAdχsyDN(116) = 0
		  dAdχsyDN(117) = 0
		  dAdχsyDN(118) = 0
		  dAdχsyDN(119) = 0
		  dAdχsyDN(120) = 0
		  dAdχsyDN(121) = 0
		  dAdχsyDN(122) = 0
		  dAdχsyDN(123) = 0
		  dAdχsyDN(124) = 0
		  dAdχsyDN(125) = 0
		  dAdχsyDN(126) = 0
		  dAdχsyDN(127) = 0
		  dAdχsyDN(128) = 0
		  dAdχsyDN(129) = 0
		  dAdχsyDN(130) = 0
		  dAdχsyDN(131) = 0
		  dAdχsyDN(132) = 0
		  dAdχsyDN(133) = 0
		  dAdχsyDN(134) = 0
		  dAdχsyDN(135) = 0
		  dAdχsyDN(136) = 0
		  dAdχsyDN(137) = 0
		  dAdχsyDN(138) = 0
		  dAdχsyDN(139) = 0
		  dAdχsyDN(140) = 0
		  dAdχsyDN(141) = 0
		  dAdχsyDN(142) = 0
		  dAdχsyDN(143) = 0
		  dAdχsyDN(144) = 0
		  dAdχsyDN(145) = 0
		  dAdχsyDN(146) = 0
		  dAdχsyDN(147) = 0
		  dAdχsyDN(148) = 0
		  dAdχsyDN(149) = 0
		  dAdχsyDN(150) = 0
		  dAdχsyDN(151) = 0
		  dAdχsyDN(152) = 0
		  dAdχsyDN(153) = 0
		  dAdχsyDN(154) = 0
		  dAdχsyDN(155) = 0
		  dAdχsyDN(156) = 0
		  dAdχsyDN(157) = 0
		  dAdχsyDN(158) = 0
		  dAdχsyDN(159) = 0
		  dAdχsyDN(160) = 0
		  dAdχsyDN(161) = 0
		  dAdχsyDN(162) = 0
		  dAdχsyDN(163) = 0
		  dAdχsyDN(164) = 0
		  dAdχsyDN(165) = 0
		  dAdχsyDN(166) = 0
		  dAdχsyDN(167) = 0
		  dAdχsyDN(168) = 1/2* δ + 1/2 *δ*c2
		  dAdχsyDN(169) = δ*s1p2
		  dAdχsyDN(170) = 0
		  dAdχsyDN(171) = 0
		  dAdχsyDN(172) = 0
		  dAdχsyDN(173) = 0
		  dAdχsyDN(174) = 0
		  dAdχsyDN(175) = 0
		  dAdχsyDN(176) = 0
		  dAdχsyDN(177) = 0
		  dAdχsyDN(178) = 0
		  dAdχsyDN(179) = 0
		  dAdχsyDN(180) = 0
		  dAdχsyDN(181) = 0
		  dAdχsyDN(182) = 0
		  dAdχsyDN(183) = 0
		  dAdχsyDN(184) = 0
		  dAdχsyDN(185) = 0
		  dAdχsyDN(186) = 0
		  dAdχsyDN(187) = 0
		  dAdχsyDN(188) = 0
		  dAdχsyDN(189) = 0
		  dAdχsyDN(190) = 0
		  dAdχsyDN(191) = 0
		  dAdχsyDN(192) = 0
		  dAdχsyDN(193) = 0
		  dAdχsyDN(194) = 0
		  dAdχsyDN(195) = 0
		  dAdχsyDN(196) = 0
		  dAdχsyDN(197) = 0
		  dAdχsyDN(198) = 0
		  dAdχsyDN(199) = 0
		  dAdχsyDN(200) = 0
		  dAdχsyDN(201) = 0
		  dAdχsyDN(202) = 0
		  dAdχsyDN(203) = 0
		  dAdχsyDN(204) = 0
		  dAdχsyDN(205) = 0
		  dAdχsyDN(206) = −η*sβ *c2p3 + 2*sβ* c2p3
		  dAdχsyDN(207) = 2/3* η*sβ* c1p4*c2 + 20/3 *sβ *c2*c1p4 − 5/3 *η*sβ* c1p4 − 14/3 *sβ *c1p4
		  dAdχsyDN(208) = − 2/3 *η*cβ *s1*c1p5 − 20/3* cβ* s1*c1p5
		  dAdχsyDN(209) = 1/3 *η*cβ *s1*c2*c1p3 + 10/3 *cβ* s1*c2*c1p3 + 7/3* η*cβ *s1*c1p3 − 2/3 *cβ *s1*c1p3
		  dAdχsyDN(210) = − 1/3 *η*cβ* s1p3*c2*c1 − 10/3 *cβ *s1p3*c2*c1 + 7/3 *η*cβ *s1p3*c1 − 2/3* cβ *s1p3*c1
		  dAdχsyDN(211) = 2/3 *η*sβ *c2*s1p4 + 20/3 *sβ *c2*s1p4 + 5/3 *η*sβ *s1p4 + 14/3* sβ* s1p4
		  dAdχsyDN(212) = − 20/3 *cβ *c1*s1p5− 2/3 *η*cβ* c1*s1p5
		  dAdχsyDN(213) = −η*sβ *c2*s2p2 + 2*sβ *c2*s2p2
		  dAdχsyDN(214) = 1/3 *η*sβ *s2p2*c2 + 10/3 *sβ* s2p2*c2
		  dAdχsyDN(215) = 1/2 *η*s2p3*cβ − s2p3*cβ
		  dAdχsyDN(216) = 5/8 *η*cβ *s2 + 1/8* η*cβ* s6 − 5/4* cβ* s2 − 1/4 *cβ *s6 
		  dAdχsyDN(217) = 0
		  dAdχsyDN(218) = 0
		  dAdχsyDN(219) = 0
		  dAdχsyDN(220) = 0
		  dAdχsyDN(221) = 0
		  dAdχsyDN(222) = 0
		  dAdχsyDN(223) = 0
		  dAdχsyDN(224) = 0
		  dAdχsyDN(225) = 0
		  dAdχsyDN(226) = 0
		  dAdχsyDN(227) = 0
		  dAdχsyDN(228) = 0
		  dAdχsyDN(229) = 0
		  dAdχsyDN(230) = 0
		  dAdχsyDN(231) = 0
		  dAdχsyDN(232) = 0
		  dAdχsyDN(233) = 0
		  dAdχsyDN(234) = 0
		  dAdχsyDN(235) = 0
		  dAdχsyDN(236) = 0
		  dAdχsyDN(237) = 0
		  dAdχsyDN(238) = 0
		  dAdχsyDN(239) = 0
		  dAdχsyDN(240) = 0
		  dAdχsyDN(241) = 0
		  dAdχsyDN(242) = 0
		  dAdχsyDN(243) = 0
		  dAdχsyDN(244) = 0
		  dAdχsyDN(245) = 0
		  dAdχsyDN(246) = 0
		  dAdχsyDN(247) = 0
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDAdχszDN()
		  // load the constant parameters
		  Var ιDN As Double  =self.ιDN 
		  Var β As Double  = self.β 
		  Var c2 As Double  = Trig.c2
		  Var s2 As Double = Trig.s2
		  Var c1 As Double = Trig.c1
		  Var s1   As Double = Trig.s1
		  Var c3   As Double = Trig.c3
		  Var s3  As Double  = Trig.s3
		  Var c4   As Double = Trig.c4
		  Var s4  As Double  = Trig.s4
		  Var c5  As Double  = Trig.c5
		  Var s5   As Double = Trig.s5
		  Var c6 As Double   = Trig.c6
		  Var s6   As Double = Trig.s6
		  Var c7   As Double = Trig.c7
		  Var s7 As Double   = Trig.s7
		  Var c8  As Double  = Trig.c8
		  Var s8  As Double  = Trig.s8
		  Var c9  As Double  = Trig.c9
		  Var s9   As Double = Trig.s9
		  Var c10  As Double  = Trig.c10
		  Var s10  As Double  = Trig.s10
		  Var c1p2  As Double  = Trig.c1p2
		  Var c1p3  As Double  = Trig.c1p3   
		  Var c1p4  As Double  =Trig. c1p4   
		  Var c1p5 As Double  = Trig.c1p5   
		  Var c1p6 As Double   = Trig.c1p6   
		  Var c1p7  As Double  = Trig.c1p7 
		  Var c1p8 As Double   = Trig.c1p8  
		  Var c1p9   As Double = Trig.c1p9  
		  Var c1p10  As Double  = Trig.c1p10
		  Var s1p2   As Double = Trig.s1p2   
		  Var s1p3  As Double  = Trig.s1p3   
		  Var s1p4As Double    = Trig.s1p4   
		  Var s1p5   As Double = Trig.s1p5  
		  Var s1p6 As Double   = Trig.s1p6   
		  Var s1p7 As Double   = Trig.s1p7   
		  Var s1p8  As Double  = Trig.s1p8   
		  Var s1p9 As Double   = Trig.s1p
		  Var s1p10 As Double  = Trig.s1p10
		  Var c2p2  As Double  = Trig.c2p2
		  Var c2p3   As Double = Trig.c2p3
		  Var c2p4 As Double  = Trig.c2p4
		  Var s2p2  As Double  = Trig.s2p2
		  Var s2p3  As Double  = Trig.s2p3
		  Var s2p4 As Double   = Trig.s2p4
		  Var s2p5 As Double   = Trig.s2p5
		  
		  // Define local β trig functions
		  Var cβ  As Double  = Trig.cβ
		  Var sβ  As Double  = Trig.sβ
		  Var c2β As Double   = Trig.c2β
		  Var s2β  As Double  = Trig.s2β
		  Var c3β As Double   = Trig.c3β
		  Var s3β  As Double  = Trig.s3β
		  Var c4β As Double   = Trig.c4β
		  Var s4β  As Double  = Trig.s4β
		  Var c5β As Double   = Trig.c5β
		  Var s5β As Double   = Trig.s5β
		  Var cβ2  As Double  = Trig.cβ2
		  Var cβ3 As Double   = Trig.cβ3
		  Var sβ2  As Double  = Trig.sβ2
		  Var sβ3  As Double  = Trig.sβ3
		  
		  
		  //Calculate the dAdχszDN
		  dAdχszDN(0) = 0
		  dAdχszDN(1) = 0
		  dAdχszDN(2) = 0
		  dAdχszDN(3) = 0
		  dAdχszDN(4) = 0
		  dAdχszDN(5) = 0
		  dAdχszDN(6) = 0
		  dAdχszDN(7) = 0
		  dAdχszDN(8) = 0
		  dAdχszDN(9) = 0
		  dAdχszDN(10) = 0
		  dAdχszDN(11) = 0
		  dAdχszDN(12) = 0
		  dAdχszDN(13) = 0
		  dAdχszDN(14) = 0
		  dAdχszDN(15) = 0
		  dAdχszDN(16) = 0
		  dAdχszDN(17) = 0
		  dAdχszDN(18) = 0
		  dAdχszDN(19) = 0
		  dAdχszDN(20) = 0
		  dAdχszDN(21) = 0
		  dAdχszDN(22) = 0
		  dAdχszDN(23) = 0
		  dAdχszDN(24) = 0
		  dAdχszDN(25) = 0
		  dAdχszDN(26) = 0
		  dAdχszDN(27) = 0
		  dAdχszDN(28) = 0
		  dAdχszDN(29) = 0
		  dAdχszDN(30) = 0
		  dAdχszDN(31) = 0
		  dAdχszDN(32) = 0
		  dAdχszDN(33) = 0
		  dAdχszDN(34) = 0
		  dAdχszDN(35) = 0
		  dAdχszDN(36) = 0
		  dAdχszDN(37) = 0
		  dAdχszDN(38) = 0
		  dAdχszDN(39) = 0
		  dAdχszDN(40) = 0
		  dAdχszDN(41) = 0
		  dAdχszDN(42) = −δ*sβ *c1p2
		  dAdχszDN(43) = −δ*sβ *s1p2
		  dAdχszDN(44) = 0
		  dAdχszDN(45) = 0
		  dAdχszDN(46) = 0
		  dAdχszDN(47) = 0
		  dAdχszDN(48) = 0
		  dAdχszDN(49) = 0
		  dAdχszDN(50) = 0
		  dAdχszDN(51) = 0
		  dAdχszDN(52) = 0
		  dAdχszDN(53) = 0
		  dAdχszDN(54) = 0
		  dAdχszDN(55) = 0
		  dAdχszDN(56) = 0
		  dAdχszDN(57) = 0
		  dAdχszDN(58) = 0
		  dAdχszDN(59) = 0
		  dAdχszDN(60) = 0
		  dAdχszDN(61) = 0
		  dAdχszDN(62) = 0
		  dAdχszDN(63) = 0
		  dAdχszDN(64) = 0
		  dAdχszDN(65) = 0
		  dAdχszDN(66) = 0
		  dAdχszDN(67) = 0
		  dAdχszDN(68) = 0
		  dAdχszDN(69) = 0
		  dAdχszDN(70) = 0
		  dAdχszDN(71) = 0
		  dAdχszDN(72) = 0
		  dAdχszDN(73) = 0
		  dAdχszDN(74) = 0
		  dAdχszDN(75) = 0
		  dAdχszDN(76) = 0
		  dAdχszDN(77) = 0
		  dAdχszDN(78) = 0
		  dAdχszDN(79) = 0
		  dAdχszDN(80) = 0
		  dAdχszDN(81) = 0
		  dAdχszDN(82) = 0
		  dAdχszDN(83) = 0
		  dAdχszDN(84) = 0
		  dAdχszDN(85) = 0
		  dAdχszDN(86) = 1/6 *η*c2β* c1p4*c2 + 5/3 *c2β* c1p4*c4 − 7/2* η*c2β *c1p4 − c2β* c1p4+ 1/2* η*c1p4*c2 + 5*c1p4*c4 − 5/2 *η*c1p4 − 3*c1p4
		  dAdχszDN(87) = 0
		  dAdχszDN(88) = 0
		  dAdχszDN(89) =  2/3 *η*s2β *s1*c1p3*c2 + 20/3* s2β* s1*c1p3*c2 − 7*η*s2β* s1*c1p3 − 2*s2β *s1*c3
		  dAdχszDN(90) = 2/3 *δ*η*s2β *c2*c1p3*s1 + 20/3* δ*s2β *c2*c1p3*s1 − 7*δ*η*s2β* c1p3*s1 − 2*δ*s2β* c1p3*s1
		  dAdχszDN(91) = 1/6 *η*c2β *c2*s1p4 + 5/3 *c2β *c2*s1p4 + 7/2 *η*c2β *s1p4 + c2β* s1p4+ 1/2 *η*c2*s1p4 + 5*c2*s1p4 + 5/2* η*s1p4 + 3*s1p4
		  dAdχszDN(92) = −3*sβ2* c2*s2p2 + 3/2* η*sβ2 *c2*s2p2
		  dAdχszDN(93) = 0
		  dAdχszDN(94) = 5*sβ2 *c2*s2p2 + 1/2 *η*sβ2 *c2*s2p2
		  dAdχszDN(95) = 3/2 *c2*s2p2 + 1/2 *c2β* c2*s2p2 − 3/4 *η*c2*s2p2 − 1/4* η*c2β *c2*s2p2
		  dAdχszDN(96) = 1/2 *s2β *s2 − 1/2 *s2β *s6 − 1/4* η*s2β* s2 + 1/4* η*s2β *s6
		  dAdχszDN(97) = 0
		  dAdχszDN(98) = 0
		  dAdχszDN(99) = 0
		  dAdχszDN(100) = 0
		  dAdχszDN(101) = 0
		  dAdχszDN(102) = 0
		  dAdχszDN(103) = 0
		  dAdχszDN(104) = 0
		  dAdχszDN(105) = 0
		  dAdχszDN(106) = 0
		  dAdχszDN(107) = 0
		  dAdχszDN(108) = 0
		  dAdχszDN(109) = 0
		  dAdχszDN(110) = 0
		  dAdχszDN(111) = 0
		  dAdχszDN(112) = 0
		  dAdχszDN(113) = 0
		  dAdχszDN(114) = 0
		  dAdχszDN(115) = 0
		  dAdχszDN(116) = 0
		  dAdχszDN(117) = 0
		  dAdχszDN(118) = 0
		  dAdχszDN(119) = 0
		  dAdχszDN(120) = 0
		  dAdχszDN(121) = 0
		  dAdχszDN(122) = 0
		  dAdχszDN(123) = 0
		  dAdχszDN(124) = 0
		  dAdχszDN(125) = 0
		  dAdχszDN(126) = 0
		  dAdχszDN(127) = 0
		  dAdχszDN(128) = 0
		  dAdχszDN(129) = 0
		  dAdχszDN(130) = 0
		  dAdχszDN(131) = 0
		  dAdχszDN(132) = 0
		  dAdχszDN(133) = 0
		  dAdχszDN(134) = 0
		  dAdχszDN(135) = 0
		  dAdχszDN(136) = 0
		  dAdχszDN(137) = 0
		  dAdχszDN(138) = 0
		  dAdχszDN(139) = 0
		  dAdχszDN(140) = 0
		  dAdχszDN(141) = 0
		  dAdχszDN(142) = 0
		  dAdχszDN(143) = 0
		  dAdχszDN(144) = 0
		  dAdχszDN(145) = 0
		  dAdχszDN(146) = 0
		  dAdχszDN(147) = 0
		  dAdχszDN(148) = 0
		  dAdχszDN(149) = 0
		  dAdχszDN(150) = 0
		  dAdχszDN(151) = 0
		  dAdχszDN(152) = 0
		  dAdχszDN(153) = 0
		  dAdχszDN(154) = 0
		  dAdχszDN(155) = 0
		  dAdχszDN(156) = 0
		  dAdχszDN(157) = 0
		  dAdχszDN(158) = 0
		  dAdχszDN(159) = 0
		  dAdχszDN(160) = 0
		  dAdχszDN(161) = 0
		  dAdχszDN(162) = 0
		  dAdχszDN(163) = 0
		  dAdχszDN(164) = 0
		  dAdχszDN(165) = 0
		  dAdχszDN(166) = 0
		  dAdχszDN(167) = 0
		  dAdχszDN(168) = 0
		  dAdχszDN(169) = 0
		  dAdχszDN(170) = 1/2* δ*cβ* sβ* c2 − 1/2 *δ*cβ* sβ
		  dAdχszDN(171) = −δ*sβ2 *s2 
		  dAdχszDN(172) = − 1/2 *δ*cβ *sβ *c2 − 1/2 *δ*cβ* sβ 
		  dAdχszDN(173) = 0
		  dAdχszDN(174) = 0
		  dAdχszDN(175) = 0
		  dAdχszDN(176) = 0
		  dAdχszDN(177) = 0
		  dAdχszDN(178) = 0
		  dAdχszDN(179) = 0
		  dAdχszDN(180) = 0
		  dAdχszDN(181) = 0
		  dAdχszDN(182) = 0
		  dAdχszDN(183) = 0
		  dAdχszDN(184) = 0
		  dAdχszDN(185) = 0
		  dAdχszDN(186) = 0
		  dAdχszDN(187) = 0
		  dAdχszDN(188) = 0
		  dAdχszDN(189) = 0
		  dAdχszDN(190) = 0
		  dAdχszDN(191) = 0
		  dAdχszDN(192) = 0
		  dAdχszDN(193) = 0
		  dAdχszDN(194) = 0
		  dAdχszDN(195) = 0
		  dAdχszDN(196) = 0
		  dAdχszDN(197) = 0
		  dAdχszDN(198) = 0
		  dAdχszDN(199) = 0
		  dAdχszDN(200) = 0
		  dAdχszDN(201) = 0
		  dAdχszDN(202) = 0
		  dAdχszDN(203) = 0
		  dAdχszDN(204) = 0
		  dAdχszDN(205) = 0
		  dAdχszDN(206) = 0
		  dAdχszDN(207) = 0
		  dAdχszDN(208) = 0
		  dAdχszDN(209) = 0
		  dAdχszDN(210) = 0
		  dAdχszDN(211) = 0
		  dAdχszDN(212) = 0
		  dAdχszDN(213) = 0
		  dAdχszDN(214) = 0
		  dAdχszDN(215) = 0
		  dAdχszDN(216) = 0
		  dAdχszDN(217) = η*sβ* c4*s2 − 2*sβ *c4*s2
		  dAdχszDN(218) = −η*cβ *s2p2*c2 + 2*cβ* s2p2*c2
		  dAdχszDN(219) = 0
		  dAdχszDN(220) = − 4/3 *η*sβ* s1p3*c2*c1 − 40/3 *sβ* s1p3*c2*c1 − 4*η*s3β* s1p3*c1 − 2*η*sβ *s1p3*c1 − 4*sβ *s1p3*c1
		  dAdχszDN(221) = 2/3* η*cβ *c2*s1p4 + 20/3 *cβ* s1p4*c2 + η*c3β* s1p4+5*η*cβ *s1p4 + 4*cβ* s1p4
		  dAdχszDN(222) = 0
		  dAdχszDN(223) = −6*η*sβ2 *cβ* s2p2 
		  dAdχszDN(224) = + 4/3 *η*sβ *s1*c2*c1p3 + 40/3* sβ* s1*c2*c1p3 − 4*η*s3β *s1*c1p3 − 2*η*sβ *s1*c1p3 − 4*sβ *s1*c1p3
		  dAdχszDN(225) = 2/3 *η*cβ *c2*c1p4 + 20/3* cβ *c2*c1p4 − η*c3β* c1p4−5*η*cβ* c1p4 − 4*cβ *c4
		  dAdχszDN(226) = 0
		  dAdχszDN(227) = 0
		  dAdχszDN(228) = 0
		  dAdχszDN(229) = 0
		  dAdχszDN(230) = 0
		  dAdχszDN(231) = 0
		  dAdχszDN(232) = 0
		  dAdχszDN(233) = 0
		  dAdχszDN(234) = 0
		  dAdχszDN(235) = 0
		  dAdχszDN(236) = 0
		  dAdχszDN(237) = 0
		  dAdχszDN(238) = 0
		  dAdχszDN(239) = 0
		  dAdχszDN(240) = 0
		  dAdχszDN(241) = 0
		  dAdχszDN(242) = 0
		  dAdχszDN(243) = 0
		  dAdχszDN(244) = 0
		  dAdχszDN(245) = 0
		  dAdχszDN(246) = 0
		  dAdχszDN(247) = 0
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateV()
		  Var γE As Double = 0.577215664901533
		  Var χ1 As Double = SourceEvolverBase.χ1
		  Var χ2 As Double = SourceEvolverBase.χ2
		  Var χ1ι As Double = SourceEvolverBase.χ1 * cos( θ1)
		  Var χ2ι As Double = SourceEvolverBase.χ2 * cos( θ2)
		  Var b6 As Double = − 1712/315
		  Var β3 As Double = (113/48 (1 + δ)^2 + 25/4*η)*χ1ι + (113/48*(1 − δ)^2 + 25/4*η)*χ2ι
		  Var β5 As Double = ( (31319/4032 − 1159/96*η)*(1 + δ)^2 + 809/84*η − 281/8*η^2)*χ1ι + ((31319/4032 − 1159/96*η*(1 − δ)^2) + 809/84*η − 281/8*η^2)*χ2ι
		  Var β6 =  (75/8*(1 + δ)^2 + 151/6*η)*π*χ1ι + (75/8*(1 − δ)^2 + 151/6*η)*π*χ2ι
		  Var β7 As Double= ( (130325/3024 − 796069/8064*η + 100019/3456*η^2)*(1 + δ)^2+ 1195759/18144*η − 257023/1008*η^2 + 2903/32*η^3) *χ1ι + (  (130325/3024 − 796 069/8064*η + 100019/3456*η^2) *(1 − δ)^2 + 1195759/18144*η − 257023/1008*η^2 + 2903/32*η^3)* χ2ι
		  Var σ4 As Double= 247/48*χ1*χ2 − 721/48*χ1ι*χ2ι +  (233/384*χ1^2 − 719/384*χ1ι^2) *(1 + δ)^2 +  (233/384*χ2^2 − 719/384*χ2ι^2) *(1 − δ)^2
		  Var a0 As Double  = 96/5*η
		  Var a2 As Double = − 743/336 − 11/4*η
		  Var a3 As Double = 4*π − β3
		  Var a4 As Double = 34103/18144 + 13661/2016*η + 59/18*η^2 − σ4
		  Var a5 As Double = − 4159/672*π − 189/8*π*η − β5
		  Var a6 As Double = 16447322263/139708800 + 16/3*π^2 − 856/105*ln(16) − 1712/105*γE − β6 + 451/48*π^2 − 56198689/217728*η + 541/896*η^2 − 5605/2592*η^3
		  Var a7 As Double = − 4415/4032*π + 358675/6048*π*η + 91495/1512*π*η^2 − β7
		  Var c2 As Double = a2/6
		  Var c3 As Double = − a3/5
		  Var c4 As Double = − a4/4 + 5/24*a2^2
		  Var c5 As Double = − a5/3 + 3/5*a3*a2
		  Var c6 As Double = − a6/2 − 3/4*b6 + 23/24*a4*a2 + 12/25*a3^2 − 67/144*a2^3
		  Var c7 As Double = −a7 + 2*a5*a2 + 2*a4*a3 − 3*a3*a2^2
		  Var ζ  As Double =  ( 5/ (256*(τc − τ ) ) )^0.125
		  v = ζ *( 1 + c2*ζ^2 + c3*ζ^3 + c4*ζ^4 + c5*ζ^5 + c6*ζ^6 + c7*ζ^7 − 3/2*b6*ζ^6*ln ζ )
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
		  Var fN As Double =  VDN*VDN*VDN/(2*Parameters.π*Parameters.GM)*Parameters.OneOver1PlusZ
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
		  εForβ = 1.0e-6
		  CosβPlus = Cos(P.β+εForβ)
		  SinβPlus = Sin(P.β+εForβ)
		  CosβMinus = Cos(P.β-εForβ)
		  SinβMinus = Sin(P.β-εForβ)
		  IDεForβ = 0.5/εForβ
		  δ = P.δ
		  η = 0.25*(1.0 - δ*δ)
		  VeSinΘ = P.Ve*Sin(P.Θ)
		  VeCosΘ = P.Ve*Cos(P.Θ)
		  ΨDN = P.λ0
		  ΨrDN = P.λ0
		  ΨDP = ΨDN
		  ΨrDP = ΨrDN
		  DΨrDΘDN = 0.0  // These derivatives are zero at time = 0
		  DΨrDΦDN = 0.0
		  DΨrDΘDP = 0.0
		  DΨrDΦDP = 0.0
		  
		  // Initialize the Noise class
		  Noise = New NoiseClass(Parameters.ΔT)
		  
		  // Set up the base case
		  SourceEvolverBase = New SourceEvolverClass(P)
		  // We need the following for calculating the z-derivative
		  α0 = SourceEvolverBase.αN
		  ι0 = SourceEvolverBase.ιN
		  χax0 = SourceEvolverBase.χaXN
		  χay0 = SourceEvolverBase.χaYN
		  χaz0 = SourceEvolverBase.χaZN
		  χsx0 = SourceEvolverBase.χsXN
		  χsy0 = SourceEvolverBase.χsYN
		  χsz0 = SourceEvolverBase.χsZN
		  
		  // Set up the parameters needed for ammplitude derivatives
		  
		  Trig = WaveAmplitudesTrigFunctions.updateTrig(ιDN, P.β)
		  
		  // Set up source evolvers where the value of δ is tweaked
		  Var ε As Double = 1.0e-6
		  SourceEvolverδMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.delta), -ε))
		  SourceEvolverδPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.delta), ε))
		  IDεForδ = 0.5/ε
		  
		  // Set up source evolvers where the value of v0 is adjusted
		  ε = 1.0e-6
		  SourceEvolverV0Minus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.v0), -ε))
		  SourceEvolverV0Plus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.v0), ε))
		  IDεForV0 = 0.5/ε
		  
		  // Set up source evolvers where the value of χ10x is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ10xMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10x), -ε))
		  SourceEvolverχ10xPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10x), ε))
		  IDεForχ10x = 0.5/ε
		  
		  // Set up source evolvers where the value of χ10y is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ10yMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10y), -ε))
		  SourceEvolverχ10yPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10y), ε))
		  IDεForχ10y = 0.5/ε
		  
		  // Set up source evolvers where the value of χ10z is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ10zMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10z), -ε))
		  SourceEvolverχ10zPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi10z), ε))
		  IDεForχ10z = 0.5/ε
		  
		  // Set up source evolvers where the value of χ20x is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ20xMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20x), -ε))
		  SourceEvolverχ20xPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20x), ε))
		  IDεForχ20x = 0.5/ε
		  
		  // Set up source evolvers where the value of χ20y is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ20yMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20y), -ε))
		  SourceEvolverχ20yPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20y), ε))
		  IDεForχ20y = 0.5/ε
		  
		  // Set up source evolvers where the value of χ20z is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ20zMinus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20z), -ε))
		  SourceEvolverχ20zPlus = New SourceEvolverClass(Tweak(Integer(CaseInfoClass.Param.chi20z), ε))
		  IDεForχ20z = 0.5/ε
		  
		  // Calculate derivative of Z with respect to lnR
		  DZDlnR = P.R*P.DZDR
		  
		  // Get the value of the detector time step (sampling interval)
		  DτrD = P.ΔT/P.GM
		  // do a trial step to get a value of DτIdeal.
		  SourceBestDτr = 1.0e300 // Initialize this to be something huge
		  // Note that DτIdeal is passed by reference, so each case has an opportunity to
		  // tweak its value. This is necessary because the base case may have no spin,
		  // while some side cases might have a spin that requires a certain step size.
		  // A first argument of zero indicates a trial step here.
		  SourceEvolverBase.DoStep(-1.0, DτrD, SourceBestDτr)
		  If P.SolveFor(Integer(CaseInfoClass.Param.delta)) Then
		    SourceEvolverδMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverδPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		  End If
		  If P.SolveFor(Integer(CaseInfoClass.Param.V0)) Then
		    SourceEvolverV0Minus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverV0Plus.DoStep(-1.0, DτrD, SourceBestDτr)
		  End If
		  If P.SolveForχ1 Then
		    SourceEvolverχ10xMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverχ10xPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverχ10yMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverχ10yPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverχ10zMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverχ10zPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		  End If
		  If P.SolveForχ2 Then
		    SourceEvolverχ20xMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverχ20xPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverχ20yMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverχ20yPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverχ20zMinus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverχ20zPlus.DoStep(-1.0, DτrD, SourceBestDτr)
		  End If
		  SourceBestDτr = SourceBestDτr*(1.0 + P.Z)  // get this step size in the solar system frame
		  
		  // Now set up the actual first time step
		  // The ratio of the real future step will be some power of two of the detector sample step size.
		  // Compute that power of two
		  Var NewStepPower as Integer = Floor(Log(SourceBestDτr/DτrD)/Log(2))
		  StepPowerF = NewStepPower // initalize the CurrentStepPower
		  StepPowerP = NewStepPower
		  DτrSF = DτrD*2^StepPowerF  // and initialize DτrSF (the time interval between the present and future source steps)
		  DτrSP = 0.0 // This will indicate a first step
		  SourceNow = 0
		  SourcePast = 0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DidDetectorStepOK(StepN As Integer) As Boolean
		  // This method will execute as many steps of the source evolution code as necessary to stay ahead of
		  // (or at least in step with) steps of the main program.
		  
		  Var oKToContinue As Boolean = True
		  Var stepUnitPower As Integer  = 10
		  DetectorNow = StepN*2^stepUnitPower
		  τrDP = τrDN // store previous step time
		  τrDN = StepN*DτrD // this is the time of the current sample step
		  While SourceNow < detectorNow
		    SourcePast = SourceNow
		    DoSourceStep
		    SourceNow = SourceNow + 2^(StepPowerP + stepUnitPower)
		    If StepPowerF < -10 Then
		      oKToContinue = False
		      Exit
		    End If
		  Wend
		  If oKToContinue Then AssembleDerivatives
		  Return oKToContinue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSourceStep()
		  // This method performs a source step
		  // Do the base case and side case steps
		  SourceEvolverBase.DoStep(DτrSP, DτrSF, SourceBestDτr)
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.delta)) Then
		    SourceEvolverδMinus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverδPlus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		  End If
		  If Parameters.SolveFor(Integer(CaseInfoClass.Param.V0)) Then
		    SourceEvolverV0Minus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverV0Plus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		  End If
		  If Parameters.SolveForχ1 Then
		    SourceEvolverχ10xMinus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverχ10xPlus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverχ10yMinus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverχ10yPlus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverχ10zMinus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverχ10zPlus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		  End If
		  If Parameters.SolveForχ2 Then
		    SourceEvolverχ20xMinus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverχ20xPlus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverχ20yMinus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverχ20yPlus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverχ20zMinus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverχ20zPlus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		  End If
		  SourceBestDτr = SourceBestDτr*(1.0+Parameters.Z) // correct to solar system time
		  // Make the source step just completed the present step
		  StepPowerP = StepPowerF
		  DτrSP = DτrSF
		  // This chooses the next source step to be a multiple or fraction of a power of 2 
		  // times the detector (sample) step
		  // Compute that power of two
		  Var NewStepPower as Integer = Floor(Log(SourceBestDτr*(1.0+Parameters.Z)/DτrD)/Log(2))
		  If NewStepPower > StepPowerP Then NewStepPower = StepPowerP // This power should never increase
		  If NewStepPower < StepPowerP Then // if the new step is smaller
		    StepPowerF = NewStepPower // this will be the step power for the next step
		    DτrSF = DτrD*2^StepPowerF
		    // note that if the power is NOT smaller, everything will remain the same
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetDataAtDetectorStep(PE As SourceEvolverClass)
		  If DetectorNow = SourceNow Then // if we are getting information about the current step,
		    VDN = PE.VN
		    ιDN = PE.ιN
		    αDN = PE.αN
		    ΨDN = PE.ΨN
		    χaxDN = PE.χaxN
		    χayDN = PE.χayN
		    χazDN = PE.χazN
		    χsxDN = PE.χsxN
		    χsyDN = PE.χsyN
		    χszDN = PE.χszN
		    AlphaDotDN = PE.AlphaDotPublic
		  Else // if we are interpolating between the past and present source steps
		    // NOTE: This should only happen when StepPowerP > 0 and when DetectorNow > SourcePast
		    // so we will insert a debugging test for this condition
		    #If DebugBuild Then
		      If Not StepPowerP > 0 or Not DetectorNow > SourcePast Then Break
		    #Endif
		    // Get the interpolated values and return them
		    Var stepRatio As Double = (DetectorNow-SourcePast)/(SourceNow-SourcePast)
		    Var oneMinusRatio As Double = 1.0 - stepRatio
		    VDN = oneMinusRatio*PE.VPold + stepRatio*PE.VN
		    ιDN = oneMinusRatio*PE.ιP  + stepRatio*PE.ιN
		    αDN = oneMinusRatio*PE.αPold + stepRatio*PE.αN
		    ΨDN = oneMinusRatio*PE.ΨP + stepRatio*PE.ΨN
		    χaxDN = oneMinusRatio*PE.χaxP + stepRatio*PE.χaXN
		    χayDN = oneMinusRatio*PE.χayP + stepRatio*PE.χaYN
		    χazDN = oneMinusRatio*PE.χazP + stepRatio*PE.χazN
		    χsxDN = oneMinusRatio*PE.χsxP + stepRatio*PE.χsxN
		    χsyDN = oneMinusRatio*PE.χsyP + stepRatio*PE.χsyN
		    χszDN = oneMinusRatio*PE.χszP + stepRatio*PE.χszN
		    
		    AlphaDotDN = PE.AlphaDotPublic
		  End If
		End Sub
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

	#tag Method, Flags = &h0
		Function Tweak(TheItem As Integer, ε As Double) As CaseInfoClass
		  Var P As CaseInfoClass = Parameters.Clone
		  Select Case TheItem
		  Case Integer(CaseInfoClass.Param.delta)
		    P.δ = δ + ε
		  Case Integer(CaseInfoClass.Param.V0)
		    P.V0 = P.V0*(1.0+ε)
		  Case Integer(CaseInfoClass.Param.chi10x)
		    P.χ10x = P.χ10x + ε
		  Case Integer(CaseInfoClass.Param.chi10y)
		    P.χ10y = P.χ10y + ε
		  Case Integer(CaseInfoClass.Param.chi10z)
		    P.χ10z = P.χ10z + ε
		  Case Integer(CaseInfoClass.Param.chi20x)
		    P.χ20x = P.χ20x + ε
		  Case Integer(CaseInfoClass.Param.chi20y)
		    P.χ20y = P.χ20y + ε
		  Case Integer(CaseInfoClass.Param.chi20z)
		    P.χ20z = P.χ20z + ε
		  End Select
		  Return P
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		A(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		AlphaDotDN As Double
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
		CosβMinus As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CosβPlus As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dAdι(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dAdβ(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dAdδ(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dAdχaxDN(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dAdχayDN(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dAdχazDN(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dAdχsxDN(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dAdχsyDN(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dAdχszDN(247) As Double
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
		DτrD As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DτrSF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DτrSP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΘDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΘDP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΦDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΦDP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		HP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		HX As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForχ20z As Double
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
		SinβMinus As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SinβPlus As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sn(5) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceBestDτr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverBase As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverV0Minus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverV0Plus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverδMinus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverδPlus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ10xMinus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ10xPlus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ10yMinus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ10yPlus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ10zMinus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ10zPlus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ20xMinus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ20xPlus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ20yMinus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ20yPlus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ20zMinus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverχ20zPlus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceNow As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		SourcePast As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		StepPowerF As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		StepPowerP As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Trig As WaveAmplitudesTrigFunctions
	#tag EndProperty

	#tag Property, Flags = &h0
		v As Double
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
		ι0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ιDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		α0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		δ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		εForβ As Double
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
		τrDP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χax0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaxDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χay0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χayDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaz0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χazDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsx0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsxDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsy0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsyDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsz0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χszDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨDP As Double
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
			Name="DτrD"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="StepPowerF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="StepPowerP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
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
			Name="αDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
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
			Name="CosβPlus"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CosβMinus"
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
			Name="SinβMinus"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SinβPlus"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IDεForβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IDεForδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IDεForV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IDεForχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IDεForχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IDεForχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IDεForχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IDεForχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IDεForχ20z"
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
			Name="SourceBestDτr"
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
			Name="α0"
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
			Name="χax0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χay0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaz0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsx0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsy0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsz0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="εForβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
		#tag ViewProperty
			Name="DetectorNow"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SourceNow"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SourcePast"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
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
			Name="τrDP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΘDP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΦDP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
			Name="ΨDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨDP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DτrSF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DτrSP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AlphaDotDN"
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
	#tag EndViewBehavior
End Class
#tag EndClass
