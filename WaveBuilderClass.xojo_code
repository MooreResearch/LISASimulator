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
		  
		  // Calculate the derivative with respect to ψ (this is the easy one!)
		  If Parameters.SolveForψ Then
		    DHDq(Integer(Item.ψ)) = 2.0*(-fx*HP + fp*HX)
		  Else
		    DHDq(Integer(Item.ψ)) = 0.0
		  End If
		  
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
		  
		  // in the case of λ0, DΨrDλ0 = 1, so the following is the correct total derivative.
		  If Parameters.SolveForλ0 Then
		    DHDq(Integer(Item.λ0)) = dHDΨr
		  Else
		    DHDq(Integer(Item.λ0)) = 0.0
		  End If
		  
		  // We can also use the above items to calculate the derivative with respect to Θ
		  If Parameters.SolveForΘ Then
		    DHDq(Integer(Item.Θ)) = dfp1dΘ*hpBase + dfp2dΘ*hpBase + dfx1dΘ*hxBase + dfx2dΘ*hxBase _
		    + dHDΨr*DΨrDΘDN
		  Else
		    DHDq(Integer(Item.Θ)) = 0.0
		  End If
		  
		  // and the derivative with respect to Φ
		  If Parameters.SolveForΦ Then
		    DHDq(Integer(Item.Φ)) = dfp1dΦ*hpBase + dfp2dΦ*hpBase + dfx1dΦ*hxBase + dfx2dΦ*hxBase _
		    + dHDΨr*DΨrDΦDN
		  Else
		    DHDq(Integer(Item.Φ)) = 0.0
		  End If
		  
		  // Now we will start calculating derivatives that involve derivatives of the wave amplitudes
		  
		  Var originalValue As Double
		  Var originalValue2 As Double
		  Var hPlus As Double
		  If Parameters.SolveForβ Then
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
		    DHDq(Integer(Item.β)) = (hPlus - fp*HP - fx*HX)*IDεForβ  // This gives us the complete β-derivative
		  Else
		    DHDq(Integer(Item.β)) = 0.0
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
		  Var dδDq As Double
		  Var dχaxDq As Double
		  Var dχayDq As Double
		  Var dχazDq As Double
		  Var dχsxDq As Double
		  Var dχsyDq As Double
		  Var dχszDq As Double
		  
		  If Parameters.SolveForΛ Then
		    // Calculate the derivative with respect to q = lnΛ
		    dαDq = -(αDN - α0)*Parameters.IVOnePlusZ*DZDlnΛ
		    dΨrDq = -(ΨrDN - Parameters.λ0)*Parameters.IVOnePlusZ*DZDlnΛ
		    dVDq = -(VDN - Parameters.V0)*Parameters.IVOnePlusZ*DZDlnΛ
		    dιDq = -(ιDN - ι0)*Parameters.IVOnePlusZ*DZDlnΛ
		    dχaxDq = -(χaxDN - χax0)*Parameters.IVOnePlusZ*DZDlnΛ
		    dχayDq = -(χayDN - χay0)*Parameters.IVOnePlusZ*DZDlnΛ
		    dχazDq = -(χazDN - χaz0)*Parameters.IVOnePlusZ*DZDlnΛ
		    dχsxDq = -(χsxDN - χsx0)*Parameters.IVOnePlusZ*DZDlnΛ
		    dχsyDq = -(χsyDN - χsy0)*Parameters.IVOnePlusZ*DZDlnΛ
		    dχszDq = -(χszDN - χsz0)*Parameters.IVOnePlusZ*DZDlnΛ
		    
		    // Now, we put it all together (The first term is actually the derivative of h0 with respect to lnΛ).
		    DHDq(Integer(Item.Λ)) = -hBase + dHDα*dαDq + dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq+ dHDχsz*dχszDq
		  Else
		    DHDq(Integer(Item.Λ)) = 0.0
		  End If
		  
		  If Parameters.SolveForF0 Then
		    // Calculate the derivative with respect to q = lnF0
		    GetDataAtDetectorStep(SourceEvolverF0Plus)
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
		    GetDataAtDetectorStep(SourceEvolverF0Minus)
		    dαDq = (αPlus - αDN)*IDεForF0
		    dΨrDq = (ΨrPlus - ΨrDN)*IDεForF0
		    dιDq = (ιPlus - ιDN)*IDεForF0
		    dVDq = (VPlus - VDN)*IDεForF0
		    dχaxDq = (χaxPlus - χaxDN)*IDεForF0
		    dχayDq = (χayPlus - χayDN)*IDεForF0
		    dχazDq = (χazPlus - χazDN)*IDεForF0
		    dχsxDq = (χsxPlus - χsxDN)*IDεForF0
		    dχsyDq = (χsyPlus - χsyDN)*IDεForF0
		    dχszDq = (χszPlus - χszDN)*IDεForF0
		    // Put it all together
		    DHDq(Integer(Item.F0)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(Item.F0)) = 0.0
		  End If
		  
		  If Parameters.SolveForM1 Then
		    // Calculate the derivative with respect to q = lnM1
		    GetDataAtDetectorStep(SourceEvolverM1Plus)
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
		    GetDataAtDetectorStep(SourceEvolverM1Minus)
		    dαDq = (αPlus - αDN)*IDεForM1
		    dΨrDq = (ΨrPlus - ΨrDN)*IDεForM1
		    dVDq = (VPlus - VDN)*IDεForM1
		    dιDq = (ιPlus - ιDN)*IDεForM1
		    dχaxDq = (χaxPlus - χaxDN)*IDεForM1
		    dχayDq = (χayPlus - χayDN)*IDεForM1
		    dχazDq = (χazPlus - χazDN)*IDεForM1
		    dχsxDq = (χsxPlus - χsxDN)*IDεForM1
		    dχsyDq = (χsyPlus - χsyDN)*IDεForM1
		    dχszDq = (χszPlus - χszDN)*IDεForM1
		    dδDq = 2.0*η  // this can be calculated analytically
		    // Put it all together
		    DHDq(Integer(Item.M1)) = hbase*Parameters.M1/(Parameters.M1 + Parameters.M2)_
		    + dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq + dHDδ*dδDq
		  Else
		    DHDq(Integer(Item.M1)) = 0.0
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
		  
		  If Parameters.SolveForM2 Then
		    // Calculate the derivative with respect to q = lnM2
		    GetDataAtDetectorStep(SourceEvolverM2Plus)
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
		    GetDataAtDetectorStep(SourceEvolverM2Minus)
		    dαDq = (αPlus - αDN)*IDεForM2
		    dΨrDq = (ΨrPlus - ΨrDN)*IDεForM2
		    dVDq = (VPlus - VDN)*IDεForM2
		    dιDq = (ιPlus - ιDN)*IDεForM2
		    dχaxDq = (χaxPlus - χaxDN)*IDεForM2
		    dχayDq = (χayPlus - χayDN)*IDεForM2
		    dχazDq = (χazPlus - χazDN)*IDεForM2
		    dχsxDq = (χsxPlus - χsxDN)*IDεForM2
		    dχsyDq = (χsyPlus - χsyDN)*IDεForM2
		    dχszDq = (χszPlus - χszDN)*IDεForM2
		    // Put it all together
		    DHDq(Integer(Item.M2)) =  hbase*Parameters.M2/(Parameters.M1 + Parameters.M2)_
		    + dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq - dHDδ*dδDq
		  Else
		    DHDq(Integer(Item.M2)) = 0.0
		  End If
		  
		  If Parameters.SolveForχ10x Then
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
		    DHDq(Integer(Item.χ10x)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(Item.χ10x)) = 0.0
		  End If
		  
		  If Parameters.SolveForχ10y Then
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
		    DHDq(Integer(Item.χ10y)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(Item.χ10y)) = 0.0
		  End If
		  
		  If Parameters.SolveForχ10z Then
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
		    DHDq(Integer(Item.χ10z)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(Item.χ10z)) = 0.0
		  End If
		  
		  If Parameters.SolveForχ20x Then
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
		    DHDq(Integer(Item.χ20x)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(Item.χ20x)) = 0.0
		  End If
		  
		  If Parameters.SolveForχ20y Then
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
		    DHDq(Integer(Item.χ20y)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(Item.χ20y)) = 0.0
		  End If
		  
		  If Parameters.SolveForχ20z Then
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
		    DHDq(Integer(Item.χ20z)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		    + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		    + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  Else
		    DHDq(Integer(Item.χ20z)) = 0.0
		  End If
		  
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
		  A(132) = 4.0*sβ*c1*s1p3
		  A(133) = -2.0*cβ*s1p4
		  A(134) =  -4.0*sβ*c1p3*s1
		  A(135) =  -2.0*cβ*c1p4
		  
		  If Parameters.PNOrder > 0 Then
		    
		    // Amplitude factors for H1P
		    
		    A(5) = δ*c1p6*(-45/32*sβ -9/32*s3β)
		    A(6) = δ*c1p2*(-175/256*sβ + c2*(87/64*sβ - 5/64*s3β) + c4*(-5/256*sβ + 15/256*s3β) + 13/256*s3β)
		    A(7) =  δ*(s1p2)*(175/256*sβ+c2*(87/64*sβ-5/64*s3β)+c4*(5/256*sβ-15/256*s3β)-13/256*s3β)
		    A(8) = δ*c1p4*s1p2*(-5/32*sβ-1/32*s3β)
		    A(9) = δ*c1p4*s1p2*(-45/32*sβ +135/32*s3β)
		    A(10) = δ*c1p2*s1p4*(45/32*sβ - 135/32*s3β)
		    A(11) = δ*c1p2*s1p4*(5/32*sβ + 1/32*s3β)
		    A(12) = δ*s1p6*sβ*(27/16 + 9/16*c2β)
		    A(13) = δ*s2p3*cβ*s2β
		    A(14) = δ*((-85/256*cβ-1/128*cβ*c2β-1/32*cβ*c2β*c2-3/128*cβ*c2β*c4)*s2-11/64*cβ*s4-1/256*cβ*s6)
		    A(15) = δ*((45/256*cβ+81/128*cβ*c2β+27/32*cβ*c2β*c2+27/128*cβ*c2β*c4)*s2+9/64*cβ*s4+9/256*cβ*s6)
		    A(16) = δ*((1/256*cβ*c2β - 85/256*cβ)*s2 + (11/64*cβ + 1/64*cβ*c2β)*s4 - (1/256*cβ+3/256*cβ*c2β)*s6)
		    A(17) = δ*((45/256*cβ + 135/256*cβ*c2β)*s2 - (9/64*cβ + 27/64*cβ*c2β)*s4 + (9/256*cβ+27/256*cβ*c2β)*s6)
		    A(18) = δ*(1/64*cβ*sβ2*s2 + 5/64*cβ*sβ2*s6)
		    
		    
		    '1 return Parameters.δ*(C1^6)*(-45/32*Sβ-9/32*S3β)
		    '2 return Parameters.δ*(C1^2)*(-175/256*Sβ+C2*(87/64*Sβ-5/64*S3β)+C4*(-5/256*Sβ+15/256*S3β)+13/256*S3β)
		    '3 return Parameters.δ*(S1^2)*(175/256*Sβ+C2*(87/64*Sβ-5/64*S3β)+C4*(5/256*Sβ-15/256*S3β)-13/256*S3β)
		    '4 return Parameters.δ*(C1^4)*(S1^2)*(-5/32*Sβ-S3β/32)
		    '5 return Parameters.δ*(C1^4)*(S1^2)*(-45/32*Sβ+S3β*135/32)
		    '6 return Parameters.δ*(C1^2)*(S1^4)*(45/32*Sβ-S3β*135/32)
		    '7 return Parameters.δ*(C1^2)*(S1^4)*(5/32*Sβ+S3β/32)
		    '8 return Parameters.δ*(S1^6)*Sβ*(27/16+9/16*C2β)
		    '9 return Parameters.δ*(45/16)*(S2^3)*(Cβ)*(Sβ^2)
		    '10 return Parameters.δ*((-85/256*Cβ-Cβ*C2β/128-Cβ*C2β*C2/32-3/128*Cβ*C2β*C4)*S2-11/64*Cβ*S4-Cβ*S6/256)
		    '11 return Parameters.δ*((45/256*Cβ+Cβ*C2β*81/128+Cβ*C2β*C2*27/32+27/128*Cβ*C2β*C4)*S2+9/64*Cβ*S4+Cβ*S6*9/256)
		    '12 return Parameters.δ*((-85/256*Cβ+Cβ*C2β*1/256)*S2+(11/64*Cβ+1/64*Cβ*C2β)*S4-(1/256*Cβ+3/256*Cβ*C2β)*S6)
		    '13 return Parameters.δ*((45/256*Cβ+Cβ*C2β*135/256)*S2-(9/64*Cβ+27/64*Cβ*C2β)*S4+(9/256*Cβ+27/256*Cβ*C2β)*S6)
		    '14 return Parameters.δ*(1/64*Cβ*Sβ^2*S2+5/64*Cβ*Sβ*S6)
		    
		    // Ampitude factors for H1X
		    
		    A(136) = -δ*(45/8*c1p2*s2β*s1p4)
		    A(137) = δ*(9/2*c2β*c1*s1p5)
		    A(138) = δ*(9/8*s2β*s1p6)
		    A(139) = δ*(-1/64*cβ*sβ + 43/128*cβ*c2*sβ - 23/128*c4*s2β + 5/256*c6*s2β)
		    A(140) = δ*((-1-1/4*c2β)*c1 + 1/4*c2β*c1*c2)*s1p3
		    A(141) = δ*(1/8*c1p2*s2β*s1p4)
		    A(142) = δ*(1/2*sβ2*s4)
		    A(143) = δ*(1/64*cβ*sβ + 43/128*cβ*c2*sβ + 23/128*c4*sβ2 + 5/256*c6*s2β)
		    A(144) = δ*((-1-1/4*c2β)*c1p3 - 1/4*c2β*c1p3*c2)*s1
		    A(145) = -δ*(1/8*c1p4*s2β*s1p2)
		    A(146) = δ*(45/8*c1p4*s2β*s1p2)
		    A(147) = δ*(9/2*c2β*c1p5*s1)
		    A(148) = -δ*(9/8*c1p6*s2β)
		    
		    '1 return Parameters.δ*(-45/8)*C1^2*S2β*S1^4
		    '2 return Parameters.δ*(9/2)*C2β*C1*S1^5
		    '3 return Parameters.δ*(9/8)*S2β*S1^6
		    '4 return Parameters.δ*(-1/64*Cβ*Sβ+43/128*Cβ*C2*Sβ-23/128*C4*S2β+5/256*C6*S2β)
		    '5 return Parameters.δ*((-1-C2β/4)*C1+1/4*C2β*C1*C2)*S1^3 
		    '6 return Parameters.δ*(1/8)*C1^2*S2β*S1^4
		    '7 return Parameters.δ*(1/2)*Sβ^2*S4
		    '8 return Parameters.δ*(Cβ*Sβ/64+43/128*Cβ*C2*Sβ+23/128*C4*S2β+5/256*C6*S2β)
		    '9 return Parameters.δ*S1*((-1-C2β/4)*C1^3-1/4*C2β*C1^3*C2)
		    '10 return Parameters.δ*(-1/8)*(C1^4*S2β*S1^2)
		    '11 return Parameters.δ*(45/8)*(C1^4)*(S2β)*(S1^2)
		    '12 return Parameters.δ*(9/2)*C2β*C1^5*S1
		    '13 return Parameters.δ*(-9/8)*C1^6*S2β
		    
		  End If
		  If Parameters.PNOrder > 1 Then
		    
		    // Amplitude factors for H2P
		    '1 return (59/16+5/2*C2β-3/16*C4β+(5/24-11/6*C2β+7/24*C4β)*C2-(5/48+1/12*C2β+7/48*C4β)*C4)*C1^4+
		    '(-25/16-13/3*C2β+9/16*C4β+(-5/8+11/2*C2β-7/8*C4β)*C2+(5/16+1/4*C2β+7/16*C4β)*C4)*η*C1^4
		    '2 return (6+2*C2β)*η*C1^8*Sβ^2-(2+2/3*C2β)*C1^8*Sβ^2
		    '3 return 32*(1/3-η)*Cβ^3*C1^7*Sβ*S1
		    '4 return ((1/6*C2β-5/6)*S2β-2/3*Cβ^2*C2*S2β+η*((5/2-1/2*C2β)*S2β+2*Cβ^2*C2*S2β))*C1^5*S1
		    '5 return (-(10/3+8/3*C2β+14/3*C4β)+η*(10+8*C2β+14*C4β))*C1^6*S1^2
		    '6 return 1/2*(-(1+1/3*C2β)+η*(3+C2β))*C1^6*Sβ^2*S1^2
		    '7 return (8/3-56/3*C2β+η*(56*C2β-8))*C1^5*S2β*S1^3
		    '8 return η*(C1*(16/3*S2β+31/4*C2*S2β+1/4*C4*S2β-19/16*S4β)-7/8*C3*S4β-7/16*C5*S4β)*S1^3
		    '+(C1*(-6*S2β-31/12*C2*S2β-1/12*C4*S2β+19/48*S4β)+7/24*C3*S4β+7/48*C5*S4β)*S1^3
		    '9 return (59/16+5/2*C2β-3/16*C4β-(5/24-11/6*C2β+7/24*C4β)*C2-(5/48+1/12*C2β+7/48*C4β)*C4)*S1^4
		    '+η*(-25/16-13/3*C2β+9/16*C4β+(5/8-11/2*C2β+7/8*C4β)*C2+(5/16+1/4*C2β+7/16*C4β)*C4)*S1^4
		    '10 return (56/3*C2β-8/3+η*(8-56*C2β))*C1^3*S2β*S1^5
		    '11 return ((5/6-1/6*C2β)*S2β-2/3*Cβ^2*C2*S2β+η*((-5/2+1/2*C2β)*S2β+2*Cβ^2*C2*S2β))*C1*S1^5
		    '12 return (-(10/3+8/3*C2β+14/3*C4β)+η*(10+8*C2β+14*C4β))*C1^2*S1^6
		    '13 return (-(1/2+1/6*C2β)+η*(3/2+1/2*C2β))*C1^2*Sβ^2*S1^6
		    '14 return 32*(η-1/3)*Cβ^3*C1*Sβ*S1^7
		    '15 return (η*(6+2*C2β)-(2+2/3*C2β))*Sβ^2*S1^8
		    '16 return 1/32*(1/3*(349-25*C2β)*Sβ^2-(25+35*C2β)*C4*Sβ^2)+η*((25*C2β-45)*Sβ^2+(25+35*C2β)*C4*Sβ^2)*S2^2
		    '17 return 1/4*(η*(25+35*C2β)-1/3*(25-35*C2β))*Sβ^2*S2^4
		    '18 return C1^3*(6*S2β-31/12*C2*S2β+1/12*C4*S2β-19/48*S4β)*S1+7/24*C1^3*S4β*S3-7/48*C1^3*S4β*S5
		    '+η*(C1^3*(-16/3*S2β+31/4*C2*S2β-1/4*C4*S2β+19/16*S4β)*S1-7/8*C1^3*S4β*S3+7/16*C1^3*S4β*S5)
		    
		    '1 Return χax*Cβ*C1^2-χaz*C1^2*Sβ
		    '2 Return χax*(Cβ/2-Cβ*C2/2)-χaz*Sβ*S1^2
		    '3 Return -χay*Cβ*S1^2
		    '4 Return -χay*Sβ*S2
		    '5 Return -χay*Cβ*C1^2
		    '6 Return Parameters.δ*(χsx*Cβ*C1^2-χsz*C1^2*Sβ)
		    '7 Return Parameters.δ*(χsx*(Cβ/2-Cβ*C2/2)-χsz*Sβ*S1^2)
		    '8 Return -Parameters.δ*(χsy*Cβ*S1^2)
		    '9 Return Parameters.δ*(χsy*Sβ*S2)
		    '10 Return -Parameters.δ*(χsy*Cβ*S1^2)
		    
		    // Ampitude factors for H2X
		    '1 Return (4*Sβ+28/3*S3β-η*(12*Sβ+28*S3β))*C1^3*S1^5
		    '2 Return (η*(4*Cβ+28*C3β-(4/3*Cβ+28/3*C3β)))*C1^2*S1^6
		    '3 Return ((4/3*Sβ-4*S3β)+η*(-4*Sβ+12*S3β))*C1*S1^7
		    '4 Return (8*η-8/3)*Cβ*Sβ*S1^8
		    '5 Return C1*(-79/8*Sβ+C2*(3/4*Sβ-19/12*S3β)+C4*(Sβ/8+7/24*S3β)-3/8*S3β)*S1^3
		    '+η*C1*(103/24*Sβ-C4*(3/8*Sβ+7/8*S3β)+9/8*S3β+C2*(-9/4*Sβ+19/4*S3β))*S1^3
		    '6 Return (47/8*Cβ+C3β/8+(7/6*Cβ+C3β/6)*C2-(Cβ/24+7/24*C3β)*C4+η*(-119/24*Cβ
		    '-3/8*C3β-(7/2*Cβ+C3β/2)*C2+(Cβ/8+7/8*C3β)*C4))*S1^4
		    '7 Return (4/3*Sβ-(1/3+C2β)*C2*Sβ+η*(-4*Sβ+(1+3*C2β)*C2*Sβ))*C1*S1^5
		    '8 Return (2*η-2/3)*Cβ*C1^2*Sβ^2*S1^6
		    '9 Return (15/2*η-5/2)*Cβ*C2*Sβ^2*S2^2
		    '10 Return C1^3*(79/8*Sβ+C2*(3/4*Sβ-19/12*S3β)-C4*(Sβ/8+7/24*S3β)+3/8*S3β)*S1
		    '+η*C1^3*(-103/24*Sβ+C4*(3/8*Sβ+7/8*S3β)-9/8*S3β+C2*(-9/4*Sβ+19/4*S3β))*S1
		    '11 Return C1^4*(47/8*Cβ+C3β/8-(7/6*Cβ+C3β/6)*C2-(Cβ/24+7/24*C3β)*C4)+η*C1^4*(-119/24*Cβ
		    '-3/8*C3β+(7/2*Cβ+C3β/2)*C2+(Cβ/8+7/8*C3β)*C4)
		    '12 Return (-4/3*Sβ-(1/3+C2β)*C2*Sβ+η*(4*Sβ+(1+3*C2β)*C2*Sβ))*C1^5*S1
		    '13 Return (2*η-2/3)*Cβ*C1^6*Sβ^2*S1^2
		    '14 Return (η*(12*Sβ+28*S3β)-(4*Sβ+28/3*S3β))*C1^5*S1^3
		    '15 Return (η*(4*Cβ+28*C3β)-(4/3*Cβ+28/3*C3β))*C1^6*S1^2
		    '16 Return (8/3+8*C2β-η*(8+24*C2β))*C1^7*Sβ*S1
		    '17 Return (8*η-8/3)*Cβ*C1^8*Sβ^2
		    
		    '1 Return χay*(1/2+C2/2)
		    '2 Return χay*S1^2
		    '3 Return χax*(Cβ^2/2-Cβ^2*C2/2)+χaz*(-Cβ*Sβ/2+Cβ*C2*Sβ/2)
		    '4 Return χax*Cβ*Sβ*S2-χaz*Sβ^2*S2
		    '5 Return χax*(Cβ^2/2+Cβ^2*C2/2)+χaz*(-Cβ*Sβ/2-Cβ*C2*Sβ/2)
		    '6 Return Parameters.δ*(χsy*(1/2+C2/2))
		    '7 Return Parameters.δ*(χsy*S1^2)
		    '8 Return Parameters.δ*(χsx*(Cβ^2/2-Cβ^2*C2/2)+χsz*(-Cβ*Sβ/2+Cβ*C2*Sβ/2))
		    '9 Return Parameters.δ*(χsx*Cβ*Sβ*S2-χsz*Sβ^2*S2)
		    '10 Return Parameters.δ*(χsx*(Cβ^2/2+Cβ^2*C2/2)+χsz*(-Cβ*Sβ/2-Cβ*C2*Sβ/2))
		    
		  End If
		  
		  If Parameters.PNOrder > 2 then
		    
		    // Ampitude factors for H3P
		    '1 Return -(3*π+π*C2β)*C1^4
		    '2 Return -4*π*C1^3*S2β*S1
		    '3 Return 4*π*C1*S2β*S1^3
		    '4 Return -(3*π+π*C2β)*S1^4
		    '5 Return -3*π*Sβ^2*S2^2
		    '6 Return Parameters.δ*(η*(625/128+625/384*C2β)-(625/256+625/768*C2β))*C1^10*Sβ^3
		    '7 Return Parameters.δ*(η*C1^2*(-7449/16384*Sβ-331/32768*S3β+C4*(337/12288*Sβ-47/8192*S3β-21/8192*S5β)
		    '+C8*(7/49152*Sβ+7/32768*S3β-35/32768*S5β)+C6*(-59/6144*Cβ-91/4096*S3β+7/4096*S5β)+C2*(1873/2048*Sβ
		    '+19/4096*S3β+35/12288*S5β)-155/98304*S5β)+C1^2*(43723/98304*Sβ-9653/65536*S3β+C2*(-10675/12288*Sβ
		    '+1901/8192*S3β-35/24576*S5β)+C6*(59/12288*Sβ+91/8192*S3β-7/8192*S5β)+C8*(-7/98304*Sβ-7/65536*S3β
		    '+35/65536*S5β)+C4*(1103/24576*Sβ-2833/16384*S3β+21/16384*S5β)+155/196608*S5β))
		    '8 Return Parameters.δ*(C1^6*(39249/8192*Sβ+38331/16384*S3β-C4*(1701/8192*Sβ+3159/16384*S3β+3645/16384*S5β)
		    '+C2*(2403/2048*Sβ-6399/4096*S3β+2187/4096*S5β)-5751/16384*S5β)+η*C1^6*(-4689/4096*Sβ-24507/8192*S3β
		    '+C2*(-2403/1024*Sβ+6399/2048*S3β-2187/2048*S5β)+C4*(1701/4096*Sβ+3159/8192*S3β+3645/8192*S5β)+5751/8192*S5β))
		    '9 Return Parameters.δ*((11875/768*Cβ+3125/768*C3β-η*(11875/384*Cβ+3125/384*C3β))*C1^9*Sβ^2*S1)
		    '10 Return Parameters.δ*(((-351/256*Cβ+243/256*Cβ*C2β)*Sβ^2-(567/256*Cβ+405/256*Cβ*C2β)*C2*Sβ^2
		    '+η*((351/128*Cβ-243/128*Cβ*C2β)*Sβ^2+(567/128*Cβ+405/128*Cβ*C2β)*C2*Sβ^2))*C1^7*S1)
		    '11 Return Parameters.δ*((η*(243/128+81/128*C2β)-(243/256+81/256*C2β))*C1^8*Sβ^3*S1^2)
		    '12 Return Parameters.δ*((-43723/98304*Sβ+9653/65536*S3β+C2*(-10675/12288*Sβ+1901/8192*S3β-35/24576*S5β)
		    '+C4*(-1103/24576*Sβ+2833/16384*S3β-21/16384*S5β)+C6*(59/12288*Sβ+91/8192*S3β-7/8192*S5β)
		    '+C8*(7/98304*Sβ+7/65536*S3β-35/65536*S5β)-155/196608*S5β)*S1^2+η*(7449/16384*Sβ+331/32768*S3β
		    '+C8*(-7/49152*Sβ-7/32768*S3β+35/32768*S5β)+C6*(-59/6144*Sβ-91/4096*S3β+7/4096*S5β)+C4*(-337/12288*Sβ
		    '+47/8192*S3β+21/8192*S5β)+C2*(1873/2048*Sβ+19/4096*S3β+35/12288*S5β)+155/98304*S5β)*S1^2)
		    '13 Return Parameters.δ*(C1^4*(1675/4096*Sβ+825/8192*S3β-C4*(7/4096*Sβ+13/8192*S3β+15/8192*S5β)
		    '+C2*(27/1024*Sβ-151/2048*S3β+3/2048*S5β)-13/8192*S5β)*S1^2+η*C1^4*(245/2048*Sβ-57/4096*S3β
		    '+C2*(-27/512*Sβ+151/1024*S3β-3/1024*S5β)+C4*(7/2048*Sβ+13/4096*S3β+15/4096*S5β)+13/4096*S5β)*S1^2)
		    '14 Return Parameters.δ*((η*(4375/512*Sβ+8125/1024*S3β+9375/1024*S5β)-(4375/1024*Sβ+8125/2048*S3β+9375/2048*S5β))*C1^8*S1^2)
		    '15 Return Parameters.δ*(C1^4*(20475/4096*Sβ-149391/8192*S3β+C2*(2187/1024*Sβ+10017/2048*S3β-1701/2048*S5β)
		    '+7371/8192*S5β+C4*(-567/4096*Sβ-1701/8192*S3β+8505/8192*S5β))*S1^2+η*C1^4*(-3195/2048*Sβ+45711/4096*S3β
		    '+C4*(567/2048*Sβ+1701/4096*S3β-8505/4096*S5β)-7371/4096*S5β+C2*(-2187/512*Sβ-10017/1024*S3β+1701/1024*S5β))*S1^2)
		    '16 Return Parameters.δ*((4375/384*Cβ+625/256*C3β+3125/256*C5β-η*(4375/192*Cβ+625/128*C3β+3125/128*C5β))*C1^7*S1^3)
		    '17 Return Parameters.δ*(C1^5*((-37/384*Cβ+1/384*Cβ*C2β)*Sβ^2-(7/384*Cβ+5/384*Cβ*C2β)*C2*Sβ^2)*S1^3
		    '+η*C1^5*((37/192*Cβ-1/192*Cβ*C2β)*Sβ^2+(7/192*Cβ+5/192*Cβ*C2β)*C2*Sβ^2)*S1^3)
		    '18 Return Parameters.δ*((η*(1/64+1/192*C2β)-(1/128+1/384*C2β))*C1^6*Sβ^3*S1^4)
		    '19 Return Parameters.δ*(η*C1^2*(-245/2048*Sβ+57/4096*S3β-C4*(7/2048*Sβ+13/4096*S3β+15/4096*S5β)
		    '+C2*(-27/512*Sβ+151/1024*S3β-3/1024*S5β)-13/4096*S5β)*S1^4+C1^2*(-1675/4096*Sβ-825/8192*S3β+C2*(27/1024*Sβ
		    '-151/2048*S3β+3/2048*S5β)+C4/4096*(7*Sβ+13*S3β+15*S5β)*S1^4))
		    '20 Return Parameters.δ*(4375*η*C1^6*(1/768*Sβ+1/512*S3β-5/512*S5β)*S1^4+4375*C1^6*(-1/1536*Sβ-1/1024*S3β+5/1024*S5β)*S1^4)
		    '21 Return Parameters.δ*(C1^2*(-20475/4096*Sβ+149391/8192*S3β+C4/4096*(567*Sβ+1701/2*S3β-8505/2*S5β)
		    '+C2/2048*(4374*Sβ+10017*S3β-1701*S5β)-7371/8192*S5β)*S1^4+η*C1^2*(3195/2048*Sβ-45711/4096*S3β
		    '+7371/4096*S5β+C2*(-2187/512*Sβ-10017/1024*S3β+1701/1024*S5β)+C4*(-567/2048*Sβ-1701/4096*S3β+8505/4096*S5β))*S1^4)
		    '22 Return Parameters.δ*(η*C1^3*((37/192*Cβ-Cβ*C2β/192)*Sβ^2-(7/192+5/192*Cβ*C2β)*C2*Sβ^2)*S1^5
		    '+C1^3*((-37/384*Cβ+Cβ*C2β/384)*Sβ^2+(7/384*Cβ+5/384*Cβ*C2β)*C2*Sβ^2)*S1^5)
		    '23 Return Parameters.δ*((1/128+1/384*C2β-η*(1/64+1/192*C2β))*C1^4*Sβ^3*S1^6)
		    '24 Return Parameters.δ*(η*((14067/4096+4689/1024*C2β-5751/4096*C4β)*Sβ+(-297/1024+1053/256*C2β
		    '-2187/1024*C4β)*C2*Sβ-(5103/4096+1701/1024*C2β+3645/4096*C4β)*C4*Sβ)*S1^6+((-55539/8192-8145/2048*C2β
		    '+5751/8192*C4β)*Sβ+(297/2048-1053/512*C2β+2187/2048*C4β)*C2*Sβ+(5103/8192+1701/2048*C2β+3645/8192*C4β)*C4*Sβ)*S1^6)
		    '25 Return Parameters.δ*(C1^4*(4375/1536*Sβ+4375/1024*S3β-21875/1024*S5β)*S1^6+η*C1^4*(-4375/768*Sβ-4375/512*S3β+21875/512*S5β)*S1^6)
		    '26 Return Parameters.δ*((4375/384*Cβ+625/256*C3β+3125/256*C5β-η*(4375/192*Cβ+625/128*C3β+3125/128*C5β))*C1^3*S1^7)
		    '27 Return Parameters.δ*(η*C1*((351/128*Cβ-243/128*Cβ*C2β)*Sβ^2-(567/128*Cβ+405/128*Cβ*C2β)*C2*Sβ^2)*S1^7
		    '+C1*((-351/256*Cβ+243/256*Cβ*C2β)*Sβ^2+(567/256*Cβ+405/256*Cβ*C2β)*C2*Sβ^2)*S1^7)
		    '28 Return Parameters.δ*((243/256-81/256*C2β-η*(243/128+81/128*C2β))*C1^2*Sβ^3*S1^8)
		    '29 Return Parameters.δ*((4375/1024*Sβ+8125/2048*S3β+9375/2048*S5β-η*(4375/512*Sβ+8125/1024*S3β+9375/1024*S5β))*C1^2*S1^8)
		    '30 Return Parameters.δ*((11875/768*Cβ+3125/768*C3β-η*(11875/384*Cβ+3125/384*C3β))*C1*Sβ^2*S1^9)
		    '31 Return Parameters.δ*((625/256+625/768*C2β-η*(625/128+625/384*C2β))*Sβ^3*S1^10)
		    '32 Return Parameters.δ*(η*((10197/2048*Cβ-3969/2048*Cβ*C2β)*Sβ^2-(1701/2048*Cβ+5103/2048*Cβ*C2β)*C4*Sβ^2)*S2^3
		    '+((-44757/4096*Cβ+3969/4096*Cβ*C2β)*Sβ^2+(1701/4096*Cβ+5103/4096*Cβ*C2β)*C4*Sβ^2)*S2^3)
		    '33 Return Parameters.δ*((21875/4096*Cβ+13125/4096*C3β-η*(21875/2048*Cβ+13125/2048*C3β))*Sβ^2*S2^5)
		    '34 Return Parameters.δ*((-37071/16384*Cβ*C2β+Cβ*(-7641/8192+567/32768*C4β)-(10917/8192*Cβ+2835/1024*Cβ*C2β)*C2
		    '+(-10089/16384*Cβ+135/8192*Cβ*C2β)*C4+513/8192*Cβ*C6+5167/32768*Cβ*C8)*S2-81/8192*Cβ*C4β*S4
		    '+1053/65536*Cβ*C4β*S6+(2565/32768*C3β+729/32768*C5β)*S8+(243/131072*C3β+1215/131072*C5β)*S10
		    '+η*((5967/8192*Cβ*C2β+Cβ*(2457/4096-567/16384*C4β)+(4005/4096*Cβ+243/512*Cβ*C2β)*C2+(6633/8192*Cβ
		    '-5319/4096*Cβ*C2β)*C4-513/4096*Cβ*C6-567/16384*Cβ*C8)*S2+81/4096*Cβ*C4β*S4-1053/32768*Cβ*C4β*S6
		    '-(2565/16384*C3β+729/16384*C5β)*S8-(243/65536*C3β+1215/65536*C5β)*S10))
		    '35 Return Parameters.δ*((-18603/8192*Cβ*C2β+Cβ*(-20475/32768+567/32768*C4β))*S2+(2835/2048*Cβ*C2β
		    '+Cβ*(5715/8192+81/8192*C4β))*S4+(135/16384*Cβ*C2β+Cβ*(-20745/65536+1053/65536*C4β))*S6-(513/16384*Cβ
		    '+2565/32768*C3β+729/32768*C5β)*S8+(567/65536*Cβ+243/131072*C3β+1215/131072*C5β)*S10+η*((5643/4096*Cβ*C2β
		    '+Cβ*(3195/16384-567/16384*C4β))*S6+(513/8192*Cβ+2565/16384*C3β+729/16384*C5β)*S8-(567/32768*Cβ+243/65536*C3β+1215/65536*C5β)*S10))
		    '36 Return Parameters.δ*((319/24576*Cβ*C2β+Cβ*(871/4096+C4β/49152)+(933/4096*Cβ+133/1536*Cβ*C2β)*C2+(625/24576*Cβ
		    '+211/4096*Cβ*C2β)*C4-11/12288*Cβ*C6-7/49152*Cβ*C8)*S2-Cβ*C4β*S4/12288+Cβ*C4β*S6/32768-(45/16384*C3β
		    '+C5β/16384)*S8-(C3β/65536+5*C5β/65536)*S10+η*((257/12288*Cβ*C2β-Cβ*(1493/6144+C4β/24576)+(-1391/6144+11/768*Cβ*C2β)*C2
		    '+(-49/12288*Cβ+77/2048*Cβ*C2β)*C4+11/6144*Cβ*C6+7/24576*Cβ*C8)*S2+Cβ*C4β*S4/6144-Cβ*C4β*S6/16384+(45/8192*C3β
		    '+C5β/8192)*S8+(C3β/32768+5/32768*C5β)*S10))
		    '37 Return Parameters.δ*((-157/12288*Cβ*C2β+Cβ*(9287/49152+C4β/49152))*S2+(-133/3072*Cβ*C2β+Cβ*(-1405/12288+C4β/12288))*S4
		    '+(211/8192*Cβ*C2β+Cβ*(419/32768+C4β/32768))*S6+(11/24576*Cβ+45/16384*C3β+C5β/16384)*S8-(7/98304*Cβ
		    '+C3β/65536+5*C5β/65536)*S10+η*((13/6144*Cβ*C2β-Cβ*(5923/24576+C4β/24576))*S2+(-11/1536*Cβ*C2β
		    '+Cβ*(701/6144-C4β/6144))*S4+(77/4096*Cβ*C2β-Cβ*(35/16384+C4β/16384))*S6-(11/12288*Cβ+45/8192*C3β+C5β/8192)*S8
		    '+(7/49152*Cβ+C3β/32768+5/32768*C5β)*S10))
		    '38 Return Parameters.δ*((-341/8192*Cβ+Cβ*C2β/8192)*Sβ^2*S2+(-3411/16384*Cβ+7/16384*Cβ*C2β)*Sβ^2*S6+(35/32768*Cβ
		    '+21/32768*C3β)*Sβ^2*S10+η*((-43/4096*Cβ-Cβ*C2β/4096)*Sβ^2*S2+(-429/8192*Cβ+7/8192*Cβ*C2β)*Sβ^2*S6
		    '+(-35/16384*Cβ-21/16384*C3β)*Sβ^2*S10))
		    
		    '1 Return χsx*(2*Cβ*C2^2*Sβ-η*Cβ*C2^3*Sβ)
		    '2 Return χsz*(η*C1^4*(-5/2-7/2*C2β+(1/2+C2β/6)*C2)+C1^4*(-3-C2β+(5+5/3*C2β)*C4))
		    '+χsx*(C1^4*(7/3*S2β-10/3*C2*S2β)-η*C1^4*(19/6*S2β+1/3*C2*S2β))
		    '3 Return χsx*(η*(1/2+C2β/6)*C1^5*S1+(5+5/3*C2β)*C1^5*S1)
		    '4 Return χsx*(η*(1/2+C2β/6)*C1*S1^5+(5+5/3*C2β)*C1*S1^5)
		    '5 Return χsx*(η*C1^3*(-17/4+79/12*C2β+(-1/4+7/12*C2β)*C2)*S1+C1^3*(3/2-13/6*C2β+(-5/2+35/6*C2β)*C2)*S1)
		    '+χsz*(η*C1^3*(-7*S2β+2/3*C2*S2β)*S1+C1^3*(-2*S2β+20/3*C2*S2β)*S1)
		    '6 Return χsx*(C1*(3/2-13/6*C2β+(5/2-35/6*C2β)*C2)*S1^3+η*C1*(-17/4+79/12*C2β+(1/4-7/12*C2β)*C2)*S1^3)
		    '+χsz*(-C1*(2*S2β+20/3*C2*S2β)*S1^3-η*C1*(7*S2β+2/3*C2*S2β)*S1^3)
		    '7 Return χsz*(η*(5/2+7/2*C2β+(1/2+C2β/6)*C2)*S1^4+(3+C2β+(5+5/3*C2β)*C2)*S1^4)+χsx*(-(7/3*S2β+10/3*C2*S2β)*S1^4
		    '+η*(19/6*S2β-1/3*C2*S2β)*S1^4)
		    '8 Return χsz*(-3+3/2*η)*C2*Sβ^2*S2^2
		    '9 Return χsx*(3/4+C2β/4-η*(3/8+C2β/8))*S2^3
		    '10 Return χsx*(10/3+1/3*η)*Cβ*C2*Sβ*S2^2+χsz*(5+η/2)*C2*Sβ^2*S2^2
		    '11 Return χsz*(3/2+C2β/2-η*(3/4+C2β/4))*C2*S2^2+χsx*(η/2-1)*C2*S2β*S2^2
		    '12 Return χsx*(-11/16*C2β*S2-3/4*S2^3-7/16*C2β*S6+η*(11/32*C2β*S2+3/8*S2^3+7/32*C2β*S6))
		    '+χsz*(S2β*S2/2-S2β*S6/2+η*(-1/4*S2β*S2+1/4*S2β*S6))
		    '13 Return χsy*((15/8-3/8*C2β+(9/8-5/8*C2β)*C4)*S2+η*(-15/16+3/16*C2β+(-9/16+5/16*C2β)*C4)*S2)
		    '14 Return χsy*(η-2)*Cβ*C2*Sβ*S2^2
		    '15 Return χsy*(3/4+C2β/4-η*(3/8+C2β/8))*S2^3
		    '16 Return χsy*(C1*(5/2-11/6*C2β+(15/2-25/6*C2β)*C2)*S1^3+η*C1*(1/4-31/12*C2β+(3/4-5/12*C2β)*C2)*S1^3)
		    '17 Return χsy*(-(7/3*S2β+10/3*C2*S2β)*S1^4-η*(5/6*S2β+1/3*C2*S2β)*S1^4)
		    '18 Return χsy*(5+5/3*C2β+η*(1/2+C2β/6))*C1*S1^5
		    '19 Return -χsy*(1/3+11/6*η)*Cβ*Sβ*S2^2
		    '20 Return χsy*(η*C1^3*(1/4-31/12*C2β)+(-3/4+5/12*C2β)*C2)*S1+C1^3*(5/2-11/6*C2β+(-15/2+25/6*C2β)*C2*S1)
		    '21 Return χsy*(C1^4*(7/3*S2β-10/3*C2*S2β)+η*C1^4*(5/6*S2β-1/3*C2*S2β))
		    '22 Return χsy*(η*(1/2+C2β/6)+5+5/3*C2β)*C1^5*S1
		    '23 Return 2*Parameters.δ*χax*Cβ*C2^3*Sβ
		    '24 Return Parameters.δ*(χaz*C1^4*(-3-C2β+(5+5/3*C2β)*C2)+χax*C1^4*(7/3*S2β-10/3*C2*S2β))
		    '25 Return Parameters.δ*χax*(5+5/3*C2β)*C1^5*S1
		    '26 Return Parameters.δ*χax*(5+5/3*C2β)*C1*S1^5
		    '27 Return Parameters.δ*(χax*(3/2-13/6*C2β+(-5/2+35/6*C2β)*C2)+χaz*(-2*S2β+20/3*C2*S2β))*C1^3*S1
		    '28 Return Parameters.δ*(χax*(3/2-13/6*C2β+(5/2-35/6*C2β)*C2)+χaz*(-2*S2β-20/3*C2*S2β))*C1*S1^3
		    '29 Return Parameters.δ*(χaz*(3+C2β+(5+5/3*C2β)*C2)*S1^4-χax*(7/2*S2β+10/3*C2*S2β)*S1^4)
		    '30 Return -3*Parameters.δ*χaz*C2*Sβ^2*S2^2
		    '31 Return Parameters.δ*χax*(3/4+C2β/4)*S2^3
		    '32 Return Parameters.δ*(10/3*χax*Cβ*C2*Sβ*S2^2+5*χaz*C2*Sβ^2*S2^2)
		    '33 Return Parameters.δ*χaz*(3/2*C2β/2)*C2*S2^2-χax*C2*S2β*S2^2
		    '34 Return Parameters.δ*(χax*(-11/16*C2β*S2-3/4*S2^3-7/16*C2β*S6)+χaz*(S2β*S2/2-S2β*S6/2))
		    '35 Return Parameters.δ*(χay*(15/8-3/8*C2β+(9/8-5/8*C2β)*C4)*S2)
		    '36 Return -2*Parameters.δ*χay*Cβ*C2*Sβ*S2^2
		    '37 Return Parameters.δ*χay*(3/4+C2β/4)*S2^3
		    '38 Return Parameters.δ*χay*C1*(5/2-11/6*C2β+(15/2-25/6*C2β)*C2)*S1^3
		    '39 Return Parameters.δ*χay*(-7/3*S2β-10/3*C2*S2β)*S1^4
		    '40 Return Parameters.δ*χay*(5+5/3*C2β)*C1*S1^5
		    '41 Return -1/3*Parameters.δ*χay*Cβ*Sβ*S2^2
		    '42 Return Parameters.δ*χay*C1^3*(5/2-11/6*C2β+(-15/2+25/6*C2β)*C2)*S1
		    '43 Return Parameters.δ*χay*C1^4*(7/3*S2β-10/3*C2*S2β)
		    '44 Return Parameters.δ*χay*(5+5/3*C2β)*C1^5*S1
		    
		    //Amplitude factors for H3X
		    '1 Return 8*π*C1*Sβ*S1^3
		    '2 Return -4*π*Cβ*S1^4
		    '3 Return -8*π*C1^3*Sβ*S1
		    '4 Return -4*π*Cβ*C1^4
		    '5 Return Parameters.δ*(C1^4*(-4375/384*S2β-4375/256*S4β)*S1^6+η*C1^4*(4375/192*S2β+4375/128*S4β)*S1^6)
		    '6 Return Parameters.δ*(625/96*C2β+625/32*C4β-η*(625/48*C2β+625/16*C4β))*C1^3*S1^7
		    '7 Return Parameters.δ*(-625/256*S2β+5625/512*S4β+η*(625/48*S2β-5625/256*S4β))*C1^2*S1^8
		    '8 Return Parameters.δ*(625/96+625/48*C2β-η*(625/48+625/24*C2β))*C1*Sβ^2*S1^9
		    '9 Return Parameters.δ*(625/192-625/96*η)*Cβ*Sβ^3*S1^10
		    '10 Return Parameters.δ*(η*C1^2*(-4923/512*S2β+C2*(459/128*S2β-2079/256*S4β)-945/1024*S4β
		    '+C4*(567/512*S2β+1701/1024*S4β))*S1^4+C1^2*(22203/1024*S2β-C4*(567/1024*S2β+1701/2048*S4β)+945/2048*S4β
		    '+C2*(-459/256*S2β+2079/512*S4β))*S1^4)
		    '11 Return Parameters.δ*(η*C1*(27/16+1233/128*C2β+27/128*C4β+(27/8+27/16*C2β+27/16*C4β)*C2
		    '-(81/128*C2β+243/128*C4β)*C4)*S1^5+C1*(-27/32-4689/256*C2β-27/256*C4β-(27/16+27/32*C2β
		    '+27/32*C4β)*C2+(81/256*C2β+243/256*C4β)*C4)*S1^5)
		    '12 Return Parameters.δ*(η*((4761/1024-1377/1024*C2β)*S2β+(837/256-621/256*C2β)*C2*S2β+(243/1024-2187/1024*C2β)*C4*S2β)*S1^6
		    '+((-11673/2048+1377/2048*C2β)*S2β+(-837/512+621/512*C2β)*C2*S2β+(-243/2048+2187/2048*C2β)*C4*S2β)*S1^6)
		    '13 Return Parameters.δ*(η*C1*((81/32-27/16*C2β)*Sβ^2-(81/32+81/16*C2β)*C2*Sβ^2)*S1^7+C1*((-81/64+27/32*C2β)*Sβ^2
		    '+(81/64+81/32*C2β)*C2*Sβ^2)*S1^7)
		    '14 Return Parameters.δ*(81/64-81/32*η)*Cβ*C1^2*Sβ^3*S1^8
		    '15 Return Parameters.δ*(683/16384*Cβ*Sβ+(557/4096-11/12288*C2β)*C4*S2β+(-1719/32768+91/32768*C2β)*C6*S2β
		    '-1/16384*Cβ*S3β+C2*(-10511/49152*Cβ*Sβ+173/49152*Cβ*S3β)+η*(85/8192*Cβ*Sβ+(-679/6144+11/6144*C2β)*C4*S2β
		    '-(201/16384+91/16384*C2β)*C6*S2β+1/8192*Cβ*S3β+C2*(6031/24576*Cβ*Sβ-173/24576*Cβ*S3β)
		    '-C10*(7/49152*S2β+7/32768*S4β)+C8*(-37/24576*S2β+91/16384*S4β))+C8*(37/49152*S2β-91/32768*S4β)+C10*(7/98304*S2β+7/65536*S4β))
		    '16 Return Parameters.δ*(η*(19/512*C4β*C3+9/512*C4β*C5+C1*(-11/16-35/128*C2β+79/1536*C4β+(1/32-37/256*C2β)*C2
		    '+(1/32+3/128*C2β)*C4-1/768*C2β*C6)-1/512*C4β*C7)*S1^3+(-19/1024*C4β*C3-9/1024*C4β*C5+C1*(19/32-23/768*C2β
		    '-79/3072*C4β-(1/64+347/512*C2β)*C2-(1/64+3/256*C2β)*C4+1/1536*C2β*C6)+1/1024*C4β*C7)*S1^3)
		    '17 Return Parameters.δ*(C1^2*(-355/1024*S2β-C2*(13/256*S2β+11/512*S4β)+C4*(-1/1024*S2β+9/2048*S4β)
		    '-5/2048*S4β)*S1^4+η*C1^2*(-29/512*S2β+C4*(1/512*S2β-9/1024*S4β)+C2*(13/128*S2β+11/256*S4β)+5/1024*S4β)*S1^4)
		    '18 Return Parameters.δ*(η*C1^3*((7/48+1/24*C2β)*Sβ^2-(1/48+1/24*C2β)*C2*Sβ^2)*S1^5+C1^3*(-(7/96+1/48*C2β)*Sβ^2
		    '+(1/96+1/48*C2β)*C2*Sβ^2)*S1^5)
		    '19 Return Parameters.δ*(1/96-1/48*η)*Cβ*C1^4*Sβ^3*S1^6
		    '20 Return Parameters.δ*((-77/256+1/256*Cβ)*Sβ^2*S4+(5/512+7/512*C2β)*Sβ^2*S8+η*((45/128-1/128*C2β)*Sβ^2*S4
		    '-(5/256-7/256*C2β)*Sβ^2*S8))
		    '21 Return Parameters.δ* (135/64+189/64*C2β-η*(135/32+189/32*C2β))*C2*Sβ^2*S2^3
		    '22 Return Parameters.δ* (-683/16384*Cβ*Sβ+(-557/4096+11/12288*C2β)*C4*S2β+(-1719/32768+91/32768*C2β)*C6*S2β
		    '+Cβ*Sβ/16384+C2*(-10511/49152*Cβ*Sβ+173/49152*Cβ*S3β)+η*(-85/8192*(Cβ)*Sβ+(679/6144-11/6144*C2β)*C4*S2β
		    '-(201/16384+91/16384*C2β)*C6*S2β-Cβ*S3β/8192+C2*(6031/24576*Cβ*Sβ-173/24576*Cβ*S3β)+C8*(37/24576*S2β-91/16384*S4β)
		    '-C10*(7/49152*S2β+7/32768*S4β))+C10*(7/98304*S2β+7/65536*S4β)+C8*(-37/49152*S2β+91/32768*S4β))
		    '23 Return Parameters.δ*(C1^3*(19/32-23/768*C2β-79/3072*C4β+(1/64+347/512*C2β)*C2-(1/64+3/256*C2β)*C4-C2β*C6/1536)*S1
		    '+19*C4β*C1^3*S3/1024-9*C4β*C1^3*S5/1024-C4β*C1^3*S7/1024+η*(C1^3*(-11/16-35/128*C2β+79/1536*C4β
		    '+(-1/32+37/256*C2β)*C2+(1/32+3/128*C2β)*C4+1/768*C2β*C6)*S1-19/512*C4β*C1^3*S3+9/512*C4β*C1^3*S5+1/512*C4β*C1^3*S7))
		    '24 Return Parameters.δ*(η*C1^4*(4923/512*S2β+C4*(567/1024*S2β+1701/2048*S4β)-945/2048*S4β+C2*(-459/256*S2β+2079/512*S4β))*S1^2)
		    '25 Return Parameters.δ*(η*C1^5*(27/16+1233/128*C2β+27/128*C4β-(27/8+27/16*C2β+27/16*C4β)*C2-(81/128*C2β+243/128*C4β)*C4)*S1
		    '+C1^5*(-27/32-4689/256*C2β-27/256*C4β+(27/16+27/32*C2β+27/32*C4β)*C2+(81/256*C2β+243/256*C4β)*C4)*S1)
		    '26 Return Parameters.δ*(C1^6*(11673/2048*S2β+C4*(243/2048*S2β-2187/4096*S4β)+C2*(-837/512*S2β+621/1024*S4β)-1377/4096*S4β)
		    '+η*C1^6*(-4761/1024*S2β+C2*(837/256*S2β-621/512*S4β)+1377/2048*S4β+C4*(-243/1024*S2β+2187/2048*S4β)))
		    '27 Return Parameters.δ*(C1^7*((-81/64+27/32*C2β)*Sβ^2-(81/64+81/32*C2β)*C2*Sβ^2)*S1+η*C1^7*((81/32-27/16*C2β)*Sβ^2
		    '+(81/32+81/16*C2β)*C2*Sβ^2)*S1)
		    '28 Return Parameters.δ*(81/32*η-81/64)*Cβ*C1^8*Sβ^3*S1^2
		    '29 Return Parameters.δ*(4375/384*S2β+4375/256*S4β-η*(4375/192*S2β+4375/128*S4β))*C1^6*S1^4
		    '30 Return Parameters.δ*(625/96*C2β+625/32*C4β-η*(625/48*C2β+625/16*C4β))*C1^7*S1^3
		    '31 Return Parameters.δ*(625/256*S2β-5625/512*S4β+η*(-625/128*S2β+5625/256*S4β))*C1^8*S1^2
		    '32 Return Parameters.δ*(625/96+625/48*C2β-η*(625/48+625/24*C2β))*C1^9*Sβ^2*S1
		    '33 Return Parameters.δ*(625/96*η-625/192)*Cβ*C1^10*Sβ^3
		    
		    '1 Return χsy*(2*C2^3*Sβ-η*C2^3*Sβ)
		    '2 Return χsy*(η*C1^4*(-5*Sβ/3 + 2*C2*Sβ/3)+C1^4*(-14*Sβ/3+20*C2*Sβ/3))
		    '3 Return χsy*(-20/3*Cβ*C1^5*S1-2/3*η*Cβ*C1^5*S1)
		    '4 Return χsy*(η*C1^3*(7*Cβ/3+Cβ*C2/3)*S1+C1^3*(-2*Cβ/3+10*Cβ*C2/3)*S1)
		    '5 Return χsy*(η*C1*(7*Cβ/3 - Cβ*C2/3)*S1^3 + C1*(-2*Cβ/3-10*Cβ*C2/3)*S1^3)
		    '6 Return χsy*(η*(5*Sβ/3+2*C2*Sβ/3)*S1^4+(14*Sβ/3+20*C2*Sβ/3)*S1^4)
		    '7 Return χsy*(-20/3*Cβ*C1*S1^5-2/3*η*Cβ*C1*S1^5)
		    '8 Return χsy*(2*C2*Sβ*S2^2-η*C2*Sβ*S2^2)
		    '9 Return χsy*(10/3*C2*Sβ*S2^2+1/3*η*C2*Sβ*S2^2)
		    '10 Return χsy*(-Cβ*S2^3+1/2*η*Cβ*S2^3)
		    '11 Return χsy*(-5/4*Cβ*S2-1/4*Cβ*S6+η*(5*Cβ*S2/8+1/8*Cβ*S6))
		    '12 Return χsx*((-3*Cβ/2-1/2*Cβ*C4)*S2+η*(3*Cβ+1/4*Cβ*C4)*S2)+χsz*(-2*C4*Sβ*S2+η*C4*Sβ*S2)
		    '13 Return χsz*(2*Cβ*C2*S2^2-η*Cβ*C2*S2^2)+χsx*(-2*C2*Sβ*S2^2+η*C2*Sβ*S2^2)
		    '14 Return χsx*(Cβ*S2^3-1/2*η*Cβ*S2^3)
		    '15 Return χsx*(C1*(-2*Cβ/3-10*Cβ*C2/3)*S1^3+η*C1*(-5/3*Cβ+4*C3β-Cβ*C2/3)*S1^3)
		    '+χsz*(C1*(-4*Sβ-40*C2*Sβ/3)*S1^3+η*C1*(-2*Sβ-4*C2*Sβ/3-4*S3β)*S1^3)
		    '16 Return χsz*(η*(5*Cβ+C3β+2*Cβ*C2/3)*S1^4+(4*Cβ+20*Cβ*C2/3)*S1^4)
		    '+χsx*((-14*Sβ/3-20*C2*Sβ/3)*S1^4+η*(10*Sβ/3-2*C2*Sβ/3+S3β)*S1^4)
		    '17 Return χsx*(20/3*Cβ*C1*S1^5+2/3*η*Cβ*C1*S1^5)
		    '18 Return -6*β*χsz*Cβ*Sβ^2*S2^2 + χsx*(1/3*Sβ*S2^2+η*(-7/6+3*C2β)*Sβ*S2^2)
		    '19 Return χsx*(η*C1^3*(-5*Cβ/3+4*C3β+Cβ*C2/3)*S1+C1^3*(-2*Cβ/3+10*Cβ*C2/3)*S1)
		    '+χsz*(C1^3*(-4*Sβ+40*C2*Sβ/3)*S1+η*C1^3*(-2*Sβ+4*C2*Sβ/3-4*S3β)*S1)
		    '20 Return χsz*(η*C1^4*(-5*Cβ-C3β+2*Cβ*C2/3)+C1^4*(-4*Cβ+20*Cβ*C2/3))
		    '+χsx*(C1^4*(14*Sβ/3-20*C2*Sβ/3)+η*C1^4*(-10*Sβ/3-2*C2*Sβ/3-S3β))
		    '21 Return χsx*(20/3*Cβ*C1^5*S1+2/3*η*Cβ*C1^5*S1)
		    '22 Return Parameters.δ*(2*χay*C2^3*Sβ)
		    '23 Return Parameters.δ*(χay*C1^4*(-14/3*Sβ+20/3*C2*Sβ))
		    '24 Return Parameters.δ*(χay*C1^3*(-2/3*Cβ+10/3*Cβ*C2)*S1)
		    '25 Return Parameters.δ*(-20/3*χay*Cβ*C1^5*S1)
		    '26 Return Parameters.δ*(χay*C1*(-2/3*Cβ-10/3*Cβ*C2)*S1^3)
		    '27 Return Parameters.δ*(χay*(14/3*Sβ+20/3*C2*Sβ)*S1^4)
		    '29 Return Parameters.δ*(2*χay*C2*Sβ*S2^2)
		    '30 Return Parameters.δ*(10/3*χay*C2*Sβ*S2^2)
		    '31 Return Parameters.δ*(-χay*Cβ*S1^3)
		    '32 Return Parameters.δ*(χay*(-5/4*Cβ*S2-1/4*Cβ*S6))
		    '33 Return Parameters.δ*(χax*(-3/2*Cβ-Cβ*C4/2)*S2-2*χaz*C4*Sβ*S2)
		    '34 Return Parameters.δ*(2*χaz*Cβ*C2*S2^2-2*χax*C2*Sβ*S2^2)
		    '35 Return Parameters.δ*(χax*Cβ*S2^3)
		    '36 Return Parameters.δ*(χax*C1*(-2/3*Cβ-10/3*Cβ*C2)*S1^3+χaz*C1*(-4*Sβ-40/3*C2*Sβ)*S1^3)
		    '37 Return Parameters.δ*(χaz*(4*Cβ+20/3*Cβ*C2)*S1^4-χax*(14/3*Sβ+20/3*C2*Sβ)*S1^4)
		    '38 Return Parameters.δ*(20/3*χax*Cβ*C1*S1^5)
		    '39 Return Parameters.δ*(1/3*χax*Sβ*S2^2)
		    '40 Return Parameters.δ*(χax*C1^3*(-2/3*Cβ+10/3*Cβ*C2)*S1+χaz*C1^3*(-4*Sβ+40/3*C2*Sβ)*S1)
		    '41 Return Parameters.δ*(χaz*C1^4*(-4*Cβ+20/3*Cβ*C2)+χax*C1^4*(14/3*Sβ-20/3*C2*Sβ))
		    '42 Return Parameters.δ*(20/3*χax*Cβ*C1^5*S1)
		    
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateWaveFactors()
		  // Calculate signal-to-noise rations
		  // This is the value of the observed orbital frequency in Hz
		  Var fN As Double =  VDN*VDN*VDN/(2*Parameters.π*Parameters.GM)*Parameters.IVOnePlusZ
		  //  get the noise at various frequencies
		  // The following set of variables contains ratios that we will use to enhance derivatives of harmonics at higher frequencies
		  // to reflect how they may be better or more poorly received by the detector than the fundamental harmonic
		  Var sn20string As String = Format(Sn20, "+0.00e+00")
		  Var snfnstring As String = Format(Sqrt(Noise.GetNoise(2*fN)), "+0.00e+00")
		  Var snratio1 As Double = Sn20/Sqrt(Noise.GetNoise(fN))
		  Var snratio2 As Double = Sn20/Sqrt(Noise.GetNoise(2*fN))
		  Var snratio3 As Double = Sn20/Sqrt(Noise.GetNoise(3*fN))
		  Var snratio4 As Double = Sn20/Sqrt(Noise.GetNoise(4*fN))
		  Var snratio5 As Double = Sn20/Sqrt(Noise.GetNoise(5*fN))
		  
		  // Calculate the received wave phase
		  
		  // Calculate basic angle multiples for the phase Ψ
		  // (The noise adjustment assumes that the orbital motion will dominate in the total wave phase,
		  // which should be an excellent approximation).
		  CosApΨ(0,1) = Cos(ΨrDN)*snratio1
		  SinApΨ(0,1) = Sin(ΨrDN)*snratio1
		  CosApΨ(0,2) = (CosApΨ(0,1)*CosApΨ(0,1) - SinApΨ(0,1)*SinApΨ(0,1))*snratio2
		  SinApΨ(0,2)  = (2*CosApΨ(0,1)*SinApΨ(0,1))*snratio2
		  CosApΨ(0,3) = (CosApΨ(0,2)*CosApΨ(0,1) - SinApΨ(0,2)*SinApΨ(0,1))*snratio3
		  SinApΨ(0,3)  = (SinApΨ(0,2)*CosApΨ(0,1) + CosApΨ(0,2)*SinApΨ(0,1))*snratio3
		  CosApΨ(0,4) = (CosApΨ(0,3)*CosApΨ(0,1) - SinApΨ(0,3)*SinApΨ(0,1))*snratio4
		  SinApΨ(0,4)  = (SinApΨ(0,3)*CosApΨ(0,1) + CosApΨ(0,3)*SinApΨ(0,1))*snratio4
		  CosApΨ(0,5) = (CosApΨ(0,4)*CosApΨ(0,1) - SinApΨ(0,4)*SinApΨ(0,1))*snratio5
		  SinApΨ(0,5)  = (SinApΨ(0,4)*CosApΨ(0,1) + CosApΨ(0,4)*SinApΨ(0,1))*snratio5
		  
		  // Calculate basic angle multiples for the phase α
		  CosApΨ(1,0) = Cos(αDN)
		  SinApΨ(1,0) = Sin(αDN)
		  CosApΨ(2,0) = CosApΨ(1,0)*CosApΨ(1,0) - SinApΨ(1,0)*SinApΨ(1,1)
		  SinApΨ(2,0)  = 2*CosApΨ(1,0)*SinApΨ(1,0)
		  CosApΨ(3,0) = CosApΨ(2,0)*CosApΨ(1,0) - SinApΨ(2,0)*SinApΨ(1,0)
		  SinApΨ(3,0)  = SinApΨ(2,0)*CosApΨ(1,0) + CosApΨ(2,0)*SinApΨ(1,0)
		  CosApΨ(4,0) = CosApΨ(3,0)*CosApΨ(1,0) - SinApΨ(3,0)*SinApΨ(1,0)
		  SinApΨ(4,0)  = SinApΨ(3,0)*CosApΨ(1,0) + CosApΨ(3,0)*SinApΨ(1,0)
		  CosApΨ(5,0) = CosApΨ(4,0)*CosApΨ(1,0) - SinApΨ(4,0)*SinApΨ(1,0)
		  SinApΨ(5,0)  = SinApΨ(4,0)*CosApΨ(1,0) + CosApΨ(4,0)*SinApΨ(1,0)
		  
		  // Now basically calculate all possible combinations
		  For j As Integer = 1 to 5
		    For k As Integer = 1 to 5
		      CosApΨ(j,k) = CosApΨ(j,0)*CosApΨ(0,k) - SinApΨ(j,0)*SinApΨ(0,k)
		      CosAmΨ(j,k) = CosApΨ(j,0)*CosApΨ(0,k) + SinApΨ(j,0)*SinApΨ(0,k)
		      SinApΨ(j,k)  = SinApΨ(j,0)*CosApΨ(0,k) + CosApΨ(j,0)*SinApΨ(0,k)
		      SinApΨ(j,k)  = SinApΨ(j,0)*CosApΨ(0,k) - CosApΨ(j,0)*SinApΨ(0,k)
		    Next
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
		  
		  // Factors for H2P
		  
		  // Factors for H3P
		  
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
		  
		  // Factors for H1X
		  
		  // Factors for H2X
		  
		  // Factors for H3X
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(P As CaseParametersClass)
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
		  Sn20 = P.Sn20
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
		  
		  // Set up source evolvers where the value of M1 is tweaked
		  Var ε As Double = 1.0e-6
		  SourceEvolverM1Minus = New SourceEvolverClass(Tweak(Item.M1, -ε))
		  SourceEvolverM1Plus = New SourceEvolverClass(Tweak(Item.M1, ε))
		  IDεForM1 = 0.5/ε
		  
		  // Set up source evolvers where the value of M2 is tweaked
		  ε = 1.0e-6
		  SourceEvolverM2Minus = New SourceEvolverClass(Tweak(Item.M2, -ε))
		  SourceEvolverM2Plus = New SourceEvolverClass(Tweak(Item.M2, ε))
		  IDεForM2 = 0.5/ε
		  
		  // Set up source evolvers where the value of F0 is adjusted
		  ε = 1.0e-6
		  SourceEvolverF0Minus = New SourceEvolverClass(Tweak(Item.F0, -ε))
		  SourceEvolverF0Plus = New SourceEvolverClass(Tweak(Item.F0, ε))
		  IDεForF0 = 0.5/ε
		  
		  // Set up source evolvers where the value of χ10x is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ10xMinus = New SourceEvolverClass(Tweak(Item.χ10x, -ε))
		  SourceEvolverχ10xPlus = New SourceEvolverClass(Tweak(Item.χ10x, ε))
		  IDεForχ10x = 0.5/ε
		  
		  // Set up source evolvers where the value of χ10y is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ10yMinus = New SourceEvolverClass(Tweak(Item.χ10y, -ε))
		  SourceEvolverχ10yPlus = New SourceEvolverClass(Tweak(Item.χ10y, ε))
		  IDεForχ10y = 0.5/ε
		  
		  // Set up source evolvers where the value of χ10z is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ10zMinus = New SourceEvolverClass(Tweak(Item.χ10z, -ε))
		  SourceEvolverχ10zPlus = New SourceEvolverClass(Tweak(Item.χ10z, ε))
		  IDεForχ10z = 0.5/ε
		  
		  // Set up source evolvers where the value of χ20x is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ20xMinus = New SourceEvolverClass(Tweak(Item.χ20x, -ε))
		  SourceEvolverχ20xPlus = New SourceEvolverClass(Tweak(Item.χ20x, ε))
		  IDεForχ20x = 0.5/ε
		  
		  // Set up source evolvers where the value of χ20y is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ20yMinus = New SourceEvolverClass(Tweak(Item.χ20y, -ε))
		  SourceEvolverχ20yPlus = New SourceEvolverClass(Tweak(Item.χ20y, ε))
		  IDεForχ20y = 0.5/ε
		  
		  // Set up source evolvers where the value of χ20z is adjusted
		  ε = 1.0e-6
		  SourceEvolverχ20zMinus = New SourceEvolverClass(Tweak(Item.χ20z, -ε))
		  SourceEvolverχ20zPlus = New SourceEvolverClass(Tweak(Item.χ20z, ε))
		  IDεForχ20z = 0.5/ε
		  
		  // Calculate derivative of Z with respect to Λ
		  Var universe As New UniverseClass
		  Var εForΛ As Double = 1.0e-6
		  Var rInSeconds As Double = P.R*(1.0 + εForΛ)
		  Var zpε As Double = universe.GetZFrom(rInSeconds)
		  rInSeconds = P.R*(1.0 - εForΛ)
		  Var zmε As Double = universe.GetZFrom(rInSeconds)
		  DZDlnΛ = (zpε - zmε)/(2.0*εForΛ)
		  DτrD = P.ΔT/P.GM  // Get the value of the detector time step (sampling interval)
		  // do a trial step to get a value of DτIdeal.
		  SourceBestDτr = 1.0e300 // Initialize this to be something huge
		  // Note that DτIdeal is passed by reference, so each case has an opportunity to
		  // tweak its value. This is necessary because the base case may have no spin,
		  // while some side cases might have a spin that requires a certain step size.
		  // A first argument of zero indicates a trial step here.
		  SourceEvolverBase.DoStep(-1.0, DτrD, SourceBestDτr)
		  If P.SolveForM1 Then 
		    SourceEvolverM1Minus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverM1Plus.DoStep(-1.0, DτrD, SourceBestDτr)
		  End If
		  If P.SolveForM2 Then
		    SourceEvolverM2Minus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverM2Plus.DoStep(-1.0, DτrD, SourceBestDτr)
		  End If
		  If P.SolveForF0 Then
		    SourceEvolverF0Minus.DoStep(-1.0, DτrD, SourceBestDτr)
		    SourceEvolverF0Plus.DoStep(-1.0, DτrD, SourceBestDτr)
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
		  If Parameters.SolveForM1 Then
		    SourceEvolverM1Minus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverM1Plus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		  End If
		  If Parameters.SolveForM2 Then
		    SourceEvolverM2Minus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverM2Plus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		  End If
		  If Parameters.SolveForF0 Then
		    SourceEvolverF0Minus.DoStep(DτrSP, DτrSF, SourceBestDτr)
		    SourceEvolverF0Plus.DoStep(DτrSP, DτrSF, SourceBestDτr)
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
		  Var h0 As Double = 0.5*(1.0 - Parameters.δ*Parameters.δ)*Parameters.GM/(Parameters.Λ*Parameters.R0)
		  HP = h0*HP
		  HX = h0*HX
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Tweak(TheItem As Item, ε As Double) As CaseParametersClass
		  Var P As CaseParametersClass = Parameters.Clone
		  Select Case TheItem
		  Case Item.M1
		    P.M1 = P.M1*(1.0 + ε)
		    P.δ = (P.M1 - P.M2)/(P.M1 + P.M2)
		  Case Item.M2
		    P.M1 = P.M1*(1.0 + ε)
		    P.δ = (P.M1 - P.M2)/(P.M1 + P.M2)
		  Case Item.F0
		    P.F0 = P.F0*(1.0 + ε)
		    P.V0 = Pow(P.GM*P.F0*2*P.π*(1.0+P.Z)/1000,1/3)
		  Case Item.χ10x
		    P.χ10x = P.χ10x + ε
		  Case Item.χ10y
		    P.χ10y = P.χ10y + ε
		  Case Item.χ10z
		    P.χ10z = P.χ10z + ε
		  Case Item.χ20x
		    P.χ20x = P.χ20x + ε
		  Case Item.χ20y
		    P.χ20y = P.χ20y + ε
		  Case Item.χ20z
		    P.χ20z = P.χ20z + ε
		  End Select
		  Return P
		End Function
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
		CosβMinus As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CosβPlus As Double
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
		DZDlnΛ As Double
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
		IDεForF0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForM1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForM2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IDεForβ As Double
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
		Parameters As CaseParametersClass
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
		Sn20 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceBestDτr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverBase As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverF0Minus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverF0Plus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverM1Minus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverM1Plus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverM2Minus As SourceEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SourceEvolverM2Plus As SourceEvolverClass
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


	#tag Enum, Name = Item, Flags = &h0
		M1
		  M2
		  F0
		  Λ
		  β
		  ψ
		  λ0
		  Θ
		  Φ
		  χ10x
		  χ10y
		  χ10z
		  χ20x
		  χ20y
		χ20z
	#tag EndEnum


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
			Name="Sn20"
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
			Name="DZDlnΛ"
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
			Name="IDεForM1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IDεForM2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IDεForF0"
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
	#tag EndViewBehavior
End Class
#tag EndClass
