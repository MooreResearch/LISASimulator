#tag Class
Protected Class EvolverClass
	#tag Method, Flags = &h0
		Sub AssembleDerivatives()
		  // These constants help us build the detector functions
		  Static cos2ψ As Double = Cos(2*Parameters.ψ)
		  Static sin2ψ As Double = Sin(2*Parameters.ψ)
		  Static σ1 As Double = 1.5*Parameters.π + Parameters.ρ0
		  Static σ2 As Double = (4/3)*Parameters.π + σ1
		  
		  Static dpc1 As Double = 3.0*Sqrt(3.0)/128.0
		  Static dpc2 As Double = Sqrt(3.0)*Cos(2.0*Parameters.Θ)/128.0
		  Static dpc3 As Double = 3.0*Sin(2.0*Parameters.Θ)/32.0
		  Static dxc1 As Double = Sqrt(3.0)*Cos(Parameters.Θ)/32.0
		  Static dxc2 As Double = 3.0*Sin(Parameters.Θ)/32.0
		  Static dpc2dΘ As Double = -Sqrt(3.0)*Sin(2.0*Parameters.Θ)/64.0
		  Static dpc3dΘ As Double = 3.0*Cos(2.0*Parameters.Θ)/16.0
		  Static dxc1dΘ As Double = -Sqrt(3.0)*Sin(Parameters.Θ)/32.0
		  Static dxc2dΘ As Double = 3.0*Cos(Parameters.Θ)/32.0
		  
		  Static sΘ As Double = Sin(Parameters.Θ)
		  Static cΘ As Double = Cos(Parameters.Θ)
		  Static s2Θ As Double = 2*sΘ*cΘ
		  Static c2Θ As Double = cΘ*cΘ - sΘ*sΘ
		  
		  // Now start calculating detector functions
		  Var ρ As Double = Parameters.GMΩe*N*Dτr
		  Var s210 As Double = Sin(2.0*ρ - σ1)
		  Var s012 As Double = Sin(σ1 - 2.0*Parameters.Φ)
		  Var s412 As Double = Sin(4.0*ρ - σ1 - 2.0*Parameters.Φ)
		  Var s311 As Double = Sin(3.0*ρ - σ1 - Parameters.Φ)
		  Var s111 As Double = Sin(ρ - σ1 - Parameters.Φ)
		  Var c210 As Double = Cos(2.0*ρ - σ1)
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
		    c210 = Cos(2.0*ρ - σ2)
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
		  
		  GetDataAtMainStep(PhaseEvolverBase)
		  CalculateWaveFactors
		  CalculateAmplitudes
		  SumSourceH(W)
		  
		  // Calculate the derivative with respect to ψ (this is the easy one!)
		  DHDq(Integer(Item.ψ)) = 2.0*(-fx*HP + fp*HX)
		  
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
		  DHDq(Integer(Item.λ0)) = dHDΨr
		  
		  // We can also use the above items to calculate the derivative with respect to Θ
		  DHDq(Integer(Item.Θ)) = dfp1dΘ*hpBase + dfp2dΘ*hpBase + dfx1dΘ*hxBase + dfx2dΘ*hxBase _
		  + dHDΨr*DΨrDΘMN
		  
		  // and the derivative with respect to Φ
		  DHDq(Integer(Item.Φ)) = dfp1dΦ*hpBase + dfp2dΦ*hpBase + dfx1dΦ*hxBase + dfx2dΦ*hxBase _
		  + dHDΨr*DΨrDΦMN
		  
		  // Now we will start calculating derivatives that involve derivatives of the wave amplitudes
		  // First, calculate the derivative with respect to β
		  Var originalValue As Double = Cosβ // Store these values for safekeeping
		  Var originalValue2 As Double = Sinβ
		  Cosβ = CosβPlus  // Reset their values to the plus tweaked versions
		  Sinβ = SinβPlus
		  CalculateAmplitudes  // calculate amplitudes using these tweaked values
		  SumSourceH(W)  // and calculate the waves with these amplitudes
		  Var hPlus As Double = fp*HP + fx*HX  // save the results for later
		  Cosβ = CosβMinus  // now reset the values of Cosβ, Sinβ to the minus tweaked version
		  Sinβ = SinβMinus
		  CalculateAmplitudes // calculate the amplitudes using these tweaked values
		  SumSourceH(W) // and calculate the waves
		  Cosβ = originalValue  // restore the original values of of Cosβ, Sinβ, so that no harm is done
		  Sinβ = originalValue2
		  DHDq(Integer(Item.β)) = (hPlus - fp*HP - fx*HX)*IDεForβ  // This gives us the complete β-derivative
		  
		  // Most of the remaining parameters require all or nearly all the following amplitude derivatives
		  // because varying the parameters have either implicit or explicit effects on the quantities that
		  // the wave amplitudes depend on.
		  
		  // Calculate amplitude derivative with respect to ι
		  Var ε As Double = 1.0e-5
		  originalValue = ιMN
		  ιMN = ιMN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  ιMN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  ιMN = originalValue
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
		  originalValue = χaxMN
		  χaxMN = χaxMN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χaxMN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χaxMN = originalValue
		  Var dHDχax As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to χay
		  ε = 1.0e-5
		  originalValue = χayMN
		  χayMN = χayMN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χayMN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χayMN = originalValue
		  Var dHDχay As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to χaz
		  ε = 1.0e-5
		  originalValue = χazMN
		  χazMN = χazMN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χazMN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χazMN = originalValue
		  Var dHDχaz As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to χsx
		  ε = 1.0e-5
		  originalValue = χsxMN
		  χsxMN = χsxMN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χsxMN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χsxMN = originalValue
		  Var dHDχsx As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to χsy
		  ε = 1.0e-5
		  originalValue = χsyMN
		  χsyMN = χsyMN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χsyMN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χsyMN = originalValue
		  Var dHDχsy As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate amplitude derivative with respect to χsz
		  ε = 1.0e-5
		  originalValue = χszMN
		  χszMN = χszMN + ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  hPlus = fp*HP + fx*HX
		  χszMN = originalValue - ε
		  CalculateAmplitudes
		  SumSourceH(W)
		  χszMN = originalValue
		  Var dHDχsz As Double = (hPlus - fp*HP - fx*HX)/(2.0*ε)
		  
		  // Calculate the derivative with respect to q = lnΛ
		  Var dαDq As Double = (αMN - α0)*Parameters.IVOnePlusZ*DZDlnΛ
		  Var dΨrDq As Double = (ΨrMN - Parameters.λ0)*Parameters.IVOnePlusZ*DZDlnΛ
		  Var dVDq As Double = (VMN - Parameters.V0)*Parameters.IVOnePlusZ*DZDlnΛ
		  Var dιDq As Double = (ιMN - ι0)*Parameters.IVOnePlusZ*DZDlnΛ
		  Var dδDq As Double // We'll need this later
		  Var dχaxDq As Double = (χaxMN - χax0)*Parameters.IVOnePlusZ*DZDlnΛ
		  Var dχayDq As Double = (χayMN - χay0)*Parameters.IVOnePlusZ*DZDlnΛ
		  Var dχazDq As Double = (χazMN - χaz0)*Parameters.IVOnePlusZ*DZDlnΛ
		  Var dχsxDq As Double = (χsxMN - χsx0)*Parameters.IVOnePlusZ*DZDlnΛ
		  Var dχsyDq As Double = (χsyMN - χsy0)*Parameters.IVOnePlusZ*DZDlnΛ
		  Var dχszDq As Double = (χszMN - χsz0)*Parameters.IVOnePlusZ*DZDlnΛ
		  
		  // Now, we put it all together (The first term is actually the derivative of h0 with respect to lnΛ).
		  DHDq(Integer(Item.Λ)) = -hBase + dHDα*dαDq + dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  + dHDχsx*dχsxDq + dHDχsy*dχsyDq+ dHDχsz*dχszDq
		  
		  // Calculate the derivative with respect to q = lnF0
		  GetDataAtMainStep(PhaseEvolverF0Plus)
		  Var ιPlus As Double = ιMN
		  Var αPlus As Double = αMN
		  Var ΨrPlus As Double = ΨrMN
		  Var VPlus As Double = VMN
		  Var χaxPlus As Double = χaxMN
		  Var χayPlus As Double = χayMN
		  Var χazPlus As Double = χazMN
		  Var χsxPlus As Double = χsxMN
		  Var χsyPlus As Double = χsyMN
		  Var χszPlus As Double = χszMN
		  GetDataAtMainStep(PhaseEvolverF0Minus)
		  dαDq = (αPlus - αMN)*IDεForF0
		  dΨrDq = (ΨrPlus - ΨrMN)*IDεForF0
		  dιDq = (ιPlus - ιMN)*IDεForF0
		  dVDq = (VPlus - VMN)*IDεForF0
		  dχaxDq = (χaxPlus - χaxMN)*IDεForF0
		  dχayDq = (χayPlus - χayMN)*IDεForF0
		  dχazDq = (χazPlus - χazMN)*IDεForF0
		  dχsxDq = (χsxPlus - χsxMN)*IDεForF0
		  dχsyDq = (χsyPlus - χsyMN)*IDεForF0
		  dχszDq = (χszPlus - χszMN)*IDεForF0
		  // Put it all together
		  DHDq(Integer(Item.F0)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  
		  // Calculate the derivative with respect to q = lnM1
		  GetDataAtMainStep(PhaseEvolverM1Plus)
		  ιPlus = ιMN
		  αPlus = αMN
		  ΨrPlus = ΨrMN
		  VPlus = VMN
		  χaxPlus = χaxMN
		  χayPlus = χayMN
		  χazPlus = χazMN
		  χsxPlus = χsxMN
		  χsyPlus = χsyMN
		  χszPlus = χszMN
		  GetDataAtMainStep(PhaseEvolverM1Minus)
		  dαDq = (αPlus - αMN)*IDεForM1
		  dΨrDq = (ΨrPlus - ΨrMN)*IDεForM1
		  dVDq = (VPlus - VMN)*IDεForM1
		  dιDq = (ιPlus - ιMN)*IDεForM1
		  dχaxDq = (χaxPlus - χaxMN)*IDεForM1
		  dχayDq = (χayPlus - χayMN)*IDεForM1
		  dχazDq = (χazPlus - χazMN)*IDεForM1
		  dχsxDq = (χsxPlus - χsxMN)*IDεForM1
		  dχsyDq = (χsyPlus - χsyMN)*IDεForM1
		  dχszDq = (χszPlus - χszMN)*IDεForM1
		  dδDq = 2.0*η  // this can be calculated analytically
		  // Put it all together
		  DHDq(Integer(Item.M1)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq + dHDδ*dδDq
		  
		  // Calculate the derivative with respect to q = lnM2
		  GetDataAtMainStep(PhaseEvolverM2Plus)
		  ιPlus = ιMN
		  αPlus = αMN
		  ΨrPlus = ΨrMN
		  VPlus = VMN
		  χaxPlus = χaxMN
		  χayPlus = χayMN
		  χazPlus = χazMN
		  χsxPlus = χsxMN
		  χsyPlus = χsyMN
		  χszPlus = χszMN
		  GetDataAtMainStep(PhaseEvolverM2Minus)
		  dαDq = (αPlus - αMN)*IDεForM2
		  dΨrDq = (ΨrPlus - ΨrMN)*IDεForM2
		  dVDq = (VPlus - VMN)*IDεForM2
		  dιDq = (ιPlus - ιMN)*IDεForM2
		  dχaxDq = (χaxPlus - χaxMN)*IDεForM2
		  dχayDq = (χayPlus - χayMN)*IDεForM2
		  dχazDq = (χazPlus - χazMN)*IDεForM2
		  dχsxDq = (χsxPlus - χsxMN)*IDεForM2
		  dχsyDq = (χsyPlus - χsyMN)*IDεForM2
		  dχszDq = (χszPlus - χszMN)*IDεForM2
		  // Put it all together
		  DHDq(Integer(Item.M2)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq + dHDδ*dδDq
		  
		  // Calculate the derivative with respect to q = χ10x
		  GetDataAtMainStep(PhaseEvolverχ10xPlus)
		  ιPlus = ιMN
		  αPlus = αMN
		  ΨrPlus = ΨrMN
		  VPlus = VMN
		  χaxPlus = χaxMN
		  χayPlus = χayMN
		  χazPlus = χazMN
		  χsxPlus = χsxMN
		  χsyPlus = χsyMN
		  χszPlus = χszMN
		  GetDataAtMainStep(PhaseEvolverχ10xMinus)
		  dαDq = (αPlus - αMN)*IDεForχ10x
		  dΨrDq = (ΨrPlus - ΨrMN)*IDεForχ10x
		  dVDq = (VPlus - VMN)*IDεForχ10x
		  dιDq = (ιPlus - ιMN)*IDεForχ10x
		  dχaxDq = (χaxPlus - χaxMN)*IDεForχ10x
		  dχayDq = (χayPlus - χayMN)*IDεForχ10x
		  dχazDq = (χazPlus - χazMN)*IDεForχ10x
		  dχsxDq = (χsxPlus - χsxMN)*IDεForχ10x
		  dχsyDq = (χsyPlus - χsyMN)*IDεForχ10x
		  dχszDq = (χszPlus - χszMN)*IDεForχ10x
		  // Put it all together
		  DHDq(Integer(Item.χ10x)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  
		  // Calculate the derivative with respect to q = χ10y
		  GetDataAtMainStep(PhaseEvolverχ10yPlus)
		  ιPlus = ιMN
		  αPlus = αMN
		  ΨrPlus = ΨrMN
		  VPlus = VMN
		  χaxPlus = χaxMN
		  χayPlus = χayMN
		  χazPlus = χazMN
		  χsxPlus = χsxMN
		  χsyPlus = χsyMN
		  χszPlus = χszMN
		  GetDataAtMainStep(PhaseEvolverχ10yMinus)
		  dαDq = (αPlus - αMN)*IDεForχ10y
		  dΨrDq = (ΨrPlus - ΨrMN)*IDεForχ10y
		  dVDq = (VPlus - VMN)*IDεForχ10y
		  dιDq = (ιPlus - ιMN)*IDεForχ10y
		  dχaxDq = (χaxPlus - χaxMN)*IDεForχ10y
		  dχayDq = (χayPlus - χayMN)*IDεForχ10y
		  dχazDq = (χazPlus - χazMN)*IDεForχ10y
		  dχsxDq = (χsxPlus - χsxMN)*IDεForχ10y
		  dχsyDq = (χsyPlus - χsyMN)*IDεForχ10y
		  dχszDq = (χszPlus - χszMN)*IDεForχ10y
		  // Put it all together
		  DHDq(Integer(Item.χ10y)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  
		  // Calculate the derivative with respect to q = χ10z
		  GetDataAtMainStep(PhaseEvolverχ10zPlus)
		  ιPlus = ιMN
		  αPlus = αMN
		  ΨrPlus = ΨrMN
		  VPlus = VMN
		  χaxPlus = χaxMN
		  χayPlus = χayMN
		  χazPlus = χazMN
		  χsxPlus = χsxMN
		  χsyPlus = χsyMN
		  χszPlus = χszMN
		  GetDataAtMainStep(PhaseEvolverχ10zMinus)
		  dαDq = (αPlus - αMN)*IDεForχ10z
		  dΨrDq = (ΨrPlus - ΨrMN)*IDεForχ10z
		  dVDq = (VPlus - VMN)*IDεForχ10z
		  dιDq = (ιPlus - ιMN)*IDεForχ10z
		  dχaxDq = (χaxPlus - χaxMN)*IDεForχ10z
		  dχayDq = (χayPlus - χayMN)*IDεForχ10z
		  dχazDq = (χazPlus - χazMN)*IDεForχ10z
		  dχsxDq = (χsxPlus - χsxMN)*IDεForχ10z
		  dχsyDq = (χsyPlus - χsyMN)*IDεForχ10z
		  dχszDq = (χszPlus - χszMN)*IDεForχ10z
		  // Put it all together
		  DHDq(Integer(Item.χ10z)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  
		  // Calculate the derivative with respect to q = χ20x
		  GetDataAtMainStep(PhaseEvolverχ20xPlus)
		  ιPlus = ιMN
		  αPlus = αMN
		  ΨrPlus = ΨrMN
		  VPlus = VMN
		  χaxPlus = χaxMN
		  χayPlus = χayMN
		  χazPlus = χazMN
		  χsxPlus = χsxMN
		  χsyPlus = χsyMN
		  χszPlus = χszMN
		  GetDataAtMainStep(PhaseEvolverχ20xMinus)
		  dαDq = (αPlus - αMN)*IDεForχ20x
		  dΨrDq = (ΨrPlus - ΨrMN)*IDεForχ20x
		  dVDq = (VPlus - VMN)*IDεForχ20x
		  dιDq = (ιPlus - ιMN)*IDεForχ20x
		  dχaxDq = (χaxPlus - χaxMN)*IDεForχ20x
		  dχayDq = (χayPlus - χayMN)*IDεForχ20x
		  dχazDq = (χazPlus - χazMN)*IDεForχ20x
		  dχsxDq = (χsxPlus - χsxMN)*IDεForχ20x
		  dχsyDq = (χsyPlus - χsyMN)*IDεForχ20x
		  dχszDq = (χszPlus - χszMN)*IDεForχ20x
		  // Put it all together
		  DHDq(Integer(Item.χ20x)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  
		  // Calculate the derivative with respect to q = χ20y
		  GetDataAtMainStep(PhaseEvolverχ20yPlus)
		  ιPlus = ιMN
		  αPlus = αMN
		  ΨrPlus = ΨrMN
		  VPlus = VMN
		  χaxPlus = χaxMN
		  χayPlus = χayMN
		  χazPlus = χazMN
		  χsxPlus = χsxMN
		  χsyPlus = χsyMN
		  χszPlus = χszMN
		  GetDataAtMainStep(PhaseEvolverχ20yMinus)
		  dαDq = (αPlus - αMN)*IDεForχ20y
		  dΨrDq = (ΨrPlus - ΨrMN)*IDεForχ20y
		  dVDq = (VPlus - VMN)*IDεForχ20y
		  dιDq = (ιPlus - ιMN)*IDεForχ20y
		  dχaxDq = (χaxPlus - χaxMN)*IDεForχ20y
		  dχayDq = (χayPlus - χayMN)*IDεForχ20y
		  dχazDq = (χazPlus - χazMN)*IDεForχ20y
		  dχsxDq = (χsxPlus - χsxMN)*IDεForχ20y
		  dχsyDq = (χsyPlus - χsyMN)*IDεForχ20y
		  dχszDq = (χszPlus - χszMN)*IDεForχ20y
		  // Put it all together
		  DHDq(Integer(Item.χ20y)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  
		  // Calculate the derivative with respect to q = χ20z
		  GetDataAtMainStep(PhaseEvolverχ20zPlus)
		  ιPlus = ιMN
		  αPlus = αMN
		  ΨrPlus = ΨrMN
		  VPlus = VMN
		  χaxPlus = χaxMN
		  χayPlus = χayMN
		  χazPlus = χazMN
		  χsxPlus = χsxMN
		  χsyPlus = χsyMN
		  χszPlus = χszMN
		  GetDataAtMainStep(PhaseEvolverχ20zMinus)
		  dαDq = (αPlus - αMN)*IDεForχ20z
		  dΨrDq = (ΨrPlus - ΨrMN)*IDεForχ20z
		  dVDq = (VPlus - VMN)*IDεForχ20z
		  dιDq = (ιPlus - ιMN)*IDεForχ20z
		  dχaxDq = (χaxPlus - χaxMN)*IDεForχ20z
		  dχayDq = (χayPlus - χayMN)*IDεForχ20z
		  dχazDq = (χazPlus - χazMN)*IDεForχ20z
		  dχsxDq = (χsxPlus - χsxMN)*IDεForχ20z
		  dχsyDq = (χsyPlus - χsyMN)*IDεForχ20z
		  dχszDq = (χszPlus - χszMN)*IDεForχ20z
		  // Put it all together
		  DHDq(Integer(Item.χ20z)) = dHDα*dαDq+ dHDΨr*dΨrDq + dHDV*dVDq + dHDι*dιDq _
		  + dHDχax*dχaxDq + dHDχay*dχayDq + dHDχaz*dχazDq _
		  + dHDχsx*dχsxDq + dHDχsy*dχsyDq + dHDχsz*dχszDq
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateAmplitudes()
		  // Now calculate all wave amplitudes
		  
		  // Calculate some useful trig functions of angle ι
		  Var c2 As Double = Cos(ιMN)
		  Var s2 As Double = Sin(ιMN)
		  Var c1 As Double = Cos(0.5*ιMN)
		  Var s1 As Double = Sin(0.5*ιMN)
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
		  Var fN As Double =  VMN*VMN*VMN/(2*Parameters.π*Parameters.GM)*Parameters.IVOnePlusZ
		  //  get the noise at various frequencies
		  // The following set of variables contains ratios that we will use to enhance derivatives of harmonics at higher frequencies
		  // to reflect how they may be better or more poorly received by the detector than the fundamental harmonic
		  Var snratio1 As Double = sn20/Sqrt(Noise.GetNoise(fN))
		  Var snratio2 As Double = sn20/Sqrt(Noise.GetNoise(2*fN))
		  Var snratio3 As Double = sn20/Sqrt(Noise.GetNoise(3*fN))
		  Var snratio4 As Double = sn20/Sqrt(Noise.GetNoise(4*fN))
		  Var snratio5 As Double = sn20/Sqrt(Noise.GetNoise(5*fN))
		  
		  // Calculate the received wave phase
		  
		  // Calculate basic angle multiples for the phase Ψ
		  // (The noise adjustment assumes that the orbital motion will dominate in the total wave phase,
		  // which should be an excellent approximation).
		  CosApΨ(0,1) = Cos(ΨrMN)*snratio1
		  SinApΨ(0,1) = Sin(ΨrMN)*snratio1
		  CosApΨ(0,2) = (CosApΨ(0,1)*CosApΨ(0,1) - SinApΨ(0,1)*SinApΨ(0,1))*snratio2
		  SinApΨ(0,2)  = (2*CosApΨ(0,1)*SinApΨ(0,1))*snratio2
		  CosApΨ(0,3) = (CosApΨ(0,2)*CosApΨ(0,1) - SinApΨ(0,2)*SinApΨ(0,1))*snratio3
		  SinApΨ(0,3)  = (SinApΨ(0,2)*CosApΨ(0,1) + CosApΨ(0,2)*SinApΨ(0,1))*snratio3
		  CosApΨ(0,4) = (CosApΨ(0,3)*CosApΨ(0,1) - SinApΨ(0,3)*SinApΨ(0,1))*snratio4
		  SinApΨ(0,4)  = (SinApΨ(0,3)*CosApΨ(0,1) + CosApΨ(0,3)*SinApΨ(0,1))*snratio4
		  CosApΨ(0,5) = (CosApΨ(0,4)*CosApΨ(0,1) - SinApΨ(0,4)*SinApΨ(0,1))*snratio5
		  SinApΨ(0,5)  = (SinApΨ(0,4)*CosApΨ(0,1) + CosApΨ(0,4)*SinApΨ(0,1))*snratio5
		  
		  // Calculate basic angle multiples for the phase α
		  CosApΨ(1,0) = Cos(αMN)
		  SinApΨ(1,0) = Sin(αMN)
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
		  W(1) = CosApΨ(1,2)   // cos(2α + Ψ)
		  W(2) = CosAmΨ(1,2)  // cos(α - 2Ψ)
		  W(3) =  CosAmΨ(2,2) // cos(2α - 2Ψ)
		  W(4) = CosApΨ(0,2)  // cos(2Ψ)
		  
		  DWDα(0) = -2.0*SinApΨ(2,2)  // derivative of cos(2α + 2Ψ) with respect to α
		  DWDα(1) = -2.0*SinApΨ(1,2)   // derivqtive of cos(2α + Ψ)
		  DWDα(2) = -SinAmΨ(1,2)  // derivative of cos(α - 2Ψ)
		  DWDα(3) =  -2.0*SinAmΨ(2,2) // derivative of cos(2α - 2Ψ)
		  DWDα(4) = 0.0  // derivative of cos(2Ψ)
		  
		  DWDΨ(0) = -2.0*SinApΨ(2,2)  // derivative of cos(2α + 2Ψ) with respect to Ψ
		  DWDΨ(1) = -SinApΨ(1,2)   // derivqtive of cos(2α + Ψ)
		  DWDΨ(2) = 2.0*SinAmΨ(1,2)  // derivative of cos(α - 2Ψ)
		  DWDΨ(3) =  2.0*SinAmΨ(2,2) // derivative of cos(2α - 2Ψ)
		  DWDΨ(4) = -SinAmΨ(0,2) // derivative of cos(2Ψ)
		  
		  // Factors for H1P
		  W(5) = CosApΨ(3,3)  // cos(3α + 3Ψ)
		  W(6) = CosApΨ(1,1)  // cos(α + Ψ)
		  W(7) = CosAmΨ(1,2)   // cos(α - 2Ψ)
		  W(8) = CosApΨ(3,1)   // cos(3α + Ψ)
		  W(9) = CosApΨ(2,2)   // cos(2α + 2Ψ)
		  W(10) = CosAmΨ(1,3)   // cos(α - 3Ψ)
		  W(11) = CosAmΨ(3,1)   // cos(3α - Ψ)
		  W(12) = CosAmΨ(3,3)  // cos(3α - 3Ψ)
		  W(13) = CosAmΨ(0,3)   // cos(3Ψ)
		  W(14) = CosApΨ(2,1)   // cos(2α + Ψ)
		  W(15) = CosApΨ(2,3)   // cos(2α + 3Ψ)
		  W(16) = CosAmΨ(2,1)   // cos(2α  - Ψ)
		  W(17) = CosAmΨ(2,3)   // cos(2α - 3Ψ)
		  W(18) = CosApΨ(0,1)  // cos(Ψ)
		  
		  // Factors for H2P
		  
		  // Factors for H3P
		  
		  // Factors for H0X
		  W(129) = SinApΨ(1,2)
		  W(130) = SinApΨ(2,2)
		  W(131) = SinAmΨ(1,2)
		  W(132) = SinAmΨ(2,2)
		  
		  DWDα(129) = CosApΨ(1,2)
		  DWDα(130) = 2.0*CosApΨ(2,2)
		  DWDα(131) = CosAmΨ(1,2)
		  DWDα(132) = 2.0*CosAmΨ(2,2)
		  
		  DWDΨ(129) = 2.0*CosApΨ(1,2)
		  DWDΨ(130) = 2.0*CosApΨ(2,2)
		  DWDΨ(131) = -2.0*CosAmΨ(1,2)
		  DWDΨ(132) = -2.0*CosAmΨ(2,2)
		  
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
		  εForβ = 1.0e-5
		  CosβPlus = Cos(P.β+εForβ)
		  SinβPlus = Sin(P.β+εForβ)
		  CosβMinus = Cos(P.β-εForβ)
		  SinβMinus = Sin(P.β-εForβ)
		  δ = P.δ
		  η = 0.25*(1.0 - δ*δ)
		  
		  // Set up the base case
		  PhaseEvolverBase = New PhaseEvolverClass(P)
		  // We need the following for calculating the z-derivative
		  α0 = PhaseEvolverBase.αN
		  ι0 = PhaseEvolverBase.ιN
		  χax0 = PhaseEvolverBase.χaXN
		  χay0 = PhaseEvolverBase.χaYN
		  χaz0 = PhaseEvolverBase.χaZN
		  χsx0 = PhaseEvolverBase.χsXN
		  χsy0 = PhaseEvolverBase.χsYN
		  χsz0 = PhaseEvolverBase.χsZN
		  
		  // Set up phase evolvers where the value of M1 is tweaked
		  Var ε As Double = 1.0e-5
		  PhaseEvolverM1Minus = New PhaseEvolverClass(Tweak(Item.M1, -ε))
		  PhaseEvolverM1Plus = New PhaseEvolverClass(Tweak(Item.M1, ε))
		  IDεForM1 = 0.5/ε
		  
		  // Set up phase evolvers where the value of M2 is tweaked
		  ε = 1.0e-5
		  PhaseEvolverM2Minus = New PhaseEvolverClass(Tweak(Item.M2, -ε))
		  PhaseEvolverM2Plus = New PhaseEvolverClass(Tweak(Item.M2, ε))
		  IDεForM2 = 0.5/ε
		  
		  // Set up phase evolvers where the value of F0 is adjusted
		  ε = 1.0e-5
		  PhaseEvolverF0Minus = New PhaseEvolverClass(Tweak(Item.F0, -ε))
		  PhaseEvolverF0Plus = New PhaseEvolverClass(Tweak(Item.F0, ε))
		  IDεForF0 = 0.5/ε
		  
		  // Set up phase evolvers where the value of χ10x is adjusted
		  ε = 1.0e-5
		  PhaseEvolverχ10xMinus = New PhaseEvolverClass(Tweak(Item.χ10x, -ε))
		  PhaseEvolverχ10xPlus = New PhaseEvolverClass(Tweak(Item.χ10x, ε))
		  IDεForχ10x = 0.5/ε
		  
		  // Set up phase evolvers where the value of χ10y is adjusted
		  ε = 1.0e-5
		  PhaseEvolverχ10yMinus = New PhaseEvolverClass(Tweak(Item.χ10y, -ε))
		  PhaseEvolverχ10yPlus = New PhaseEvolverClass(Tweak(Item.χ10y, ε))
		  IDεForχ10y = 0.5/ε
		  
		  // Set up phase evolvers where the value of χ10z is adjusted
		  ε = 1.0e-5
		  PhaseEvolverχ10zMinus = New PhaseEvolverClass(Tweak(Item.χ10z, -ε))
		  PhaseEvolverχ10zPlus = New PhaseEvolverClass(Tweak(Item.χ10z, ε))
		  IDεForχ10z = 0.5/ε
		  
		  // Set up phase evolvers where the value of χ20x is adjusted
		  ε = 1.0e-5
		  PhaseEvolverχ20xMinus = New PhaseEvolverClass(Tweak(Item.χ20x, -ε))
		  PhaseEvolverχ20xPlus = New PhaseEvolverClass(Tweak(Item.χ20x, ε))
		  IDεForχ20x = 0.5/ε
		  
		  // Set up phase evolvers where the value of χ20y is adjusted
		  ε = 1.0e-5
		  PhaseEvolverχ20yMinus = New PhaseEvolverClass(Tweak(Item.χ20y, -ε))
		  PhaseEvolverχ20yPlus = New PhaseEvolverClass(Tweak(Item.χ20y, ε))
		  IDεForχ20y = 0.5/ε
		  
		  // Set up phase evolvers where the value of χ20z is adjusted
		  ε = 1.0e-5
		  PhaseEvolverχ20zMinus = New PhaseEvolverClass(Tweak(Item.χ20z, -ε))
		  PhaseEvolverχ20zPlus = New PhaseEvolverClass(Tweak(Item.χ20z, ε))
		  IDεForχ20z = 0.5/ε
		  
		  // Calculate derivative of Z with respect to Λ
		  Var universe As New UniverseClass
		  Var εForΛ As Double = 1.0e-5
		  Var rInSeconds As Double = P.R*(1.0 + εForΛ)
		  Var zpε As Double = universe.GetZFrom(rInSeconds)
		  rInSeconds = P.R*(1.0 - εForΛ)
		  Var zmε As Double = universe.GetZFrom(rInSeconds)
		  DZDlnΛ = (zpε - zmε)/(2.0*εForΛ)
		  Dτr = P.ΔT/P.GM  // Get the value of the main time step at the detector
		  DτF = Dτr/(1.0+P.Z)  // This is time step at the source
		  // do a trial step to get a value of DτIdeal.
		  DτIdeal = 1.0e300 // Initialize this to be something huge
		  // Note that DτIdeal is passed by reference, so each case has an opportunity to
		  // tweak its value. This is necessary because the base case may have no spin,
		  // while some side cases might have a spin that requires a certain step size.
		  // A first argument of zero indicates a trial step here.
		  PhaseEvolverBase.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverM1Minus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverM1Plus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverM2Minus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverM2Plus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverF0Minus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverF0Plus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10xMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10xPlus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10yMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10yPlus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10zMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10zPlus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ20xMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ20xPlus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ20yMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ20yPlus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ20zMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ20zPlus.DoStep(0.0, DτF, DτIdeal)
		  
		  // Now set up the actual first time step
		  // The ratio of the real future step will be some power of two of the main step.
		  // Compute that power of two
		  Var NewStepPower as Integer = Floor(Log(DτIdeal*(1.0+P.Z)/DτF)/Log(2))
		  StepPowerFF = NewStepPower // initalize the CurrentStepPower
		  StepPowerF = NewStepPower
		  StepPowerP = NewStepPower
		  DτFF = Dτr*2^StepPowerF*P.IVOnePlusZ  // and initialize DτFF
		  DτF = DτFF // and set DτF
		  DτP = DτF  // and DτP to be the same
		  
		  // Finally, do an actual first (Euler) phase step with the new step size
		  // Note that setting the first argument to zero indicates a first step here
		  // Note that DτF is half the value of the TwoDτF parameter, so doing a
		  // half step with the past value of each item equal to the present is the same
		  // as doing an Euler step
		  PhaseEvolverBase.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverM1Minus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverM1Plus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverM2Minus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverM2Plus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverF0Minus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverF0Plus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10xMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10xPlus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10yMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10yPlus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10zMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ10zPlus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ20xMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ20xPlus.DoStep(1.0, DτF, DτIdeal)
		  PhaseEvolverχ20yMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ20yPlus.DoStep(0.0, 0.0, DτIdeal)
		  PhaseEvolverχ20zMinus.DoStep(0.0, DτF, DτIdeal)
		  PhaseEvolverχ20zPlus.DoStep(0.0, DτF, DτIdeal)
		  
		  // We also need to do a phase step
		  If StepPowerF > 0 Then // Source step is larger than phase step
		    DoMainPhaseStep
		  Else
		    DoSourcePhaseStep
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DidMainStepOK(MainStepNumber As Integer) As Boolean
		  // This method will execute as many steps of the source evolution code as necessary to stay ahead of
		  // (or at least in step with) steps of the main program.
		  
		  Var OKToContinue As Boolean = True
		  N = MainStepNumber
		  If N = 0 Then // If this is the first step, we have already done both the source step and the phase step
		    MainStepsInSourceStep = 2^StepPowerP  // Set up this variable
		    WhereInSourceStep = 0 // we will simply report the present values
		  ElseIf StepPowerF > 0 Then  // If the step that will be taken is bigger than the main step
		    If LastSourceStep > N Then // and the last source step (which might have been bigger than the current step) is still ahead
		      WhereInSourceStep = WhereInSourceStep + 1   // Update the "WhereInSourceStep" counter
		    Elseif LastSourceStep = N Then  // We have caught up with the source
		      WhereInSourceStep = 0  // and we will simply report present values
		    Else  // main program is now ahead of the source
		      DoSourceStep  // Take a new source step (which will include a phase step of the appropriate size)
		      MainStepsInSourceStep = 2^StepPowerP // This is the number of main steps within the source step just taken
		      // update the source step counter in units of the main step
		      LastSourceStep = LastSourceStep + MainStepsInSourceStep
		      WhereInSourceStep = 1 // we are now at the first step within that total range
		    End If
		    DoMainPhaseStep // whether we take a source step or not, we must take a phase step.
		    // In this case, we do phase steps every main step because the Earth's orbital phase changes
		    // significantly each main step, even if the binary's frequency does not
		  ElseIf StepPowerF = 0 Then // If the next source step will be equal to the main program step
		    If LastSourceStep < N Then // If source is behind the main step
		      DoSourceStep // do a source step
		      DoSourcePhaseStep // and a phase step aligned with the
		      LastSourceStep = LastSourceStep + 1   // update the source step counter
		      WhereInSourceStep = 0  // and we will report the present values
		    ElseIf LastSourceStep = N Then   // I don't think this should happen, but if it does
		      WhereInSourceStep = 0 // we will just report the present values
		    End If
		  Else  // the next source step size will be smaller than the main step size
		    Var stepsToDo As Integer = 2^(-StepPowerF) // get the number of steps to execute in units of the current step size
		    Var stepUnitPower As Integer = StepPowerF // these are the units of StepsToDo
		    Var stepsDone As Integer = 0
		    Do
		      DoSourceStep  // Do a source step
		      DoSourcePhaseStep  // and a phase step
		      If StepPowerF < -10 Then
		        OKToContinue = False
		        Exit
		      End If
		      stepsDone = stepsDone + 1  // Count the step
		      If StepPowerF < stepUnitPower And stepsDone < stepsToDo Then
		        // If the next step size will be smaller and we have not reached the target
		        stepsToDo = 2^(-stepPowerF)   // re-express the target in terms of the next step size
		        stepsDone = stepsDone*2^(stepUnitPower-StepPowerF)  // and rescale the steps already done
		      End If
		    Loop Until stepsDone = stepsToDo
		    If OKToContinue Then // if we haven't exited becase we are too close to coalescence
		      WhereInSourceStep = 0 // and we will report the present values
		    End If
		  End If
		  If OKToContinue Then AssembleDerivatives  // Calculate H at the main step if we can
		  Return OKToContinue
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoMainPhaseStep()
		  // We do this only when the source step is larger than the main step: in such a case,
		  // we need to do phase steps aligned with the main steps because the Earth's orbital
		  // phase may change even if the binary's orbital frequency remains basically constant
		  Var orbitPhase As Double = Parameters.GMΩe*N*Dτr - Parameters.Φ
		  Var twoDτPhase As Double = 2.0*Dτr*Parameters.IVOnePlusZ
		  Var stepRatio As Double = WhereInSourceStep/MainStepsInSourceStep
		  Var oneMinusRatio As Double = 1.0 - stepRatio
		  PhaseEvolverBase.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase, True)
		  PhaseEvolverM1Minus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverM1Plus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverM2Minus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverM2Plus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverF0Minus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverF0Plus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10xMinus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10xPlus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10yMinus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10yPlus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10zMinus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10zPlus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20xMinus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20xPlus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20yMinus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20yPlus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20zMinus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20zPlus.DoMiniPhaseStep(oneMinusRatio, stepRatio, orbitPhase, twoDτPhase)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSourcePhaseStep()
		  // We do this only when the source step equal to or smaller than main step.
		  // In such a case, the phase steps are synchronized with the source steps.
		  Var orbitPhase As Double = Parameters.GMΩe*N*Dτr - Parameters.Φ
		  Var twoDτPhase As Double = 2.0*DτF
		  Var dτRatio As Double = DτF/DτP
		  Var oneMinusRatio As Double = 1.0 - dτRatio
		  PhaseEvolverBase.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase, True)
		  PhaseEvolverM1Minus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverM1Plus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverM2Minus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverM2Plus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverF0Minus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverF0Plus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10xMinus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10xPlus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10yMinus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10yPlus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10zMinus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ10zPlus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20xMinus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20xPlus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20yMinus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20yPlus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20zMinus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		  PhaseEvolverχ20zPlus.DoPhaseStep(dτRatio, oneMinusRatio, orbitPhase, twoDτPhase)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoSourceStep()
		  // This method performs a source step
		  
		  // First, make the future the present
		  StepPowerP = StepPowerF
		  StepPowerF = StepPowerFF
		  DτP = DτF
		  DτF = DτFF
		  
		  // Set up some pre-calculated local variables
		  Var dτRatio As Double = DτF/DτP
		  Var twoDτ As Double = 2.0*DτF
		  // Do the base case and side case steps
		  PhaseEvolverBase.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverM1Minus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverM1Plus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverM2Minus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverM2Plus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverF0Minus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverF0Plus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ10xMinus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ10xPlus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ10yMinus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ10yPlus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ10zMinus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ10zPlus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ20xMinus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ20xPlus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ20yMinus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ20yPlus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ20zMinus.DoStep(dτRatio, twoDτ, DτIdeal)
		  PhaseEvolverχ20zPlus.DoStep(dτRatio, twoDτ, DτIdeal)
		  
		  // This chooses the next time step to be a multiple or fraction of a power of 2
		  // times the main program time step (as seen in the source frame)
		  // The ratio of the real future step will be some power of two of the main step.
		  // Compute that power of two
		  Var NewStepPower as Integer = Floor(Log(DτIdeal*(1.0+Parameters.Z)/Dτr)/Log(2))
		  If NewStepPower > StepPowerF Then NewStepPower = StepPowerF // This power should never increase
		  If NewStepPower < StepPowerF Then // if the new step is smaller
		    StepPowerFF = NewStepPower // this will be the step power for the next step
		    // note that if the power is NOT smaller, everything will remain the same
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetDataAtMainStep(PE As PhaseEvolverClass)
		  If WhereInSourceStep = 0 Then // if we are getting information about the current step,
		    VMN = PE.VN
		    ιMN = PE.ιN
		    αMN = PE.αN
		    ΨrMN = PE.ΨrN
		    χaxMN = PE.χaxN
		    χayMN = PE.χayN
		    χazMN = PE.χazN
		    χsxMN = PE.χsxN
		    χsyMN = PE.χsyN
		    χszMN = PE.χszN
		    DΨrDΘMN = PE.DΨrDΘN
		    DΨrDΦMN = PE.DΨrDΦN
		  Else // if we are interpolating between the current step and a future step,
		    // Get the interpolated values and return them
		    Var stepRatio As Double = WhereInSourceStep/MainStepsInSourceStep
		    Var oneMinusRatio As Double = 1.0 - stepRatio
		    VMN = oneMinusRatio*PE.VN + stepRatio*PE.VPold
		    ιMN = oneMinusRatio*PE.ιN  + stepRatio*PE.ιP
		    αMN = oneMinusRatio*PE.αN + stepRatio*PE.αPold
		    χaxMN = oneMinusRatio*PE.χaxN + stepRatio*PE.χaXP
		    χayMN = oneMinusRatio*PE.χayN + stepRatio*PE.χaYP
		    χazMN = oneMinusRatio*PE.χazN + stepRatio*PE.χazP
		    χsxMN = oneMinusRatio*PE.χsxN + stepRatio*PE.χsxP
		    χsyMN = oneMinusRatio*PE.χsyN + stepRatio*PE.χsyP
		    χszMN = oneMinusRatio*PE.χszN + stepRatio*PE.χszP
		    DΨrDΘMN = oneMinusRatio*PE.DΨrDΘN + stepRatio*PE.DΨrDΘP
		    DΨrDΦMN = oneMinusRatio*PE.DΨrDΦN + stepRatio*PE.DΨrDΦP
		    ΨrMN = PE.ΨrN // in this case, though, the phase step is still equal to the main step
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
		    If DoVDeriv Then vpower = 2.0*VMN
		  Else
		    vpower = VMN*VMN
		  End If
		  HP = sum*vpower
		  
		  If Parameters.PNOrder > 0 Then
		    sum = 0.0
		    jStart = H0PLastIndex + 1
		    For j As Integer = jStart to H1PLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VMN
		    If DoVDeriv Then vpower = 1.5*vpower
		    HP = HP + sum*vpower
		  End If
		  
		  If Parameters.PNOrder > 1 Then
		    sum = 0.0
		    jStart = H1PLastIndex + 1
		    For j As Integer = jStart to H2PLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VMN
		    If DoVDeriv Then vpower = 1.33333333333333333*vpower
		    HP = HP + sum*vpower
		  End If
		  
		  If Parameters.PNOrder > 2 Then
		    jStart = H2PLastIndex + 1
		    sum = 0.0
		    For j As Integer = jStart to H3PLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VMN
		    If DoVDeriv Then vpower = 1.25*vpower
		    HP = HP + sum*vpower
		  End If
		  
		  // now assemble cross polarization
		  sum = 0.0
		  vpower = VMN*VMN
		  jStart = H3PLastIndex + 1
		  For j As Integer = jStart To H0PLastIndex
		    sum = sum + A(j)*Wave(j)
		  Next
		  If DoVDeriv Then
		    vpower = 2.0*VMN
		  Else
		    vpower = VMN*VMN
		  End If
		  HX = sum*vpower
		  
		  If Parameters.PNOrder > 0 Then
		    sum = 0.0
		    jStart = H0XLastIndex + 1
		    For j As Integer = jStart to H1PLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VMN
		    If DoVDeriv Then vpower = 1.5*vpower
		    HX = HX + sum*vpower
		  End If
		  
		  If Parameters.PNOrder > 1 Then
		    sum = 0.0
		    jStart = H1XLastIndex + 1
		    For j As Integer = jStart to H2PLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VMN
		    If DoVDeriv Then vpower = 1.3333333333333333*vpower
		    HX = HX + sum*vpower
		  End If
		  
		  If Parameters.PNOrder > 2 Then
		    jStart = H2XLastIndex + 1
		    sum = 0.0
		    For j As Integer = jStart to H3XLastIndex
		      sum = sum + A(j)*Wave(j)
		    Next
		    vpower = vpower*VMN
		    If DoVDeriv Then vpower = 1.25*vpower
		    HX = HX + sum*vpower
		  End If
		  
		  // Calculate overall wave amplitude constant
		  Var h0 As Double = 0.5*(1.0 - Parameters.δ*Parameters.δ)/Parameters.Λ
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
		DτF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DτFF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DτIdeal As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DτP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dτr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΘMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΦMN As Double
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
		LastSourceStep As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		MainStepsInSourceStep As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		N As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Noise As NoiseClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverBase As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverF0Minus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverF0Plus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverM1Minus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverM1Plus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverM2Minus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverM2Plus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ10xMinus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ10xPlus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ10yMinus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ10yPlus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ10zMinus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ10zPlus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ20xMinus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ20xPlus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ20yMinus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ20yPlus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ20zMinus As PhaseEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseEvolverχ20zPlus As PhaseEvolverClass
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
		StepPowerF As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		StepPowerFF As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		StepPowerP As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		VMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		W(247) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		WhereInSourceStep As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		ι0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ιMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		α0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αMN As Double
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
		χax0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaxMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χay0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χayMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaz0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χazMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsx0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsxMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsy0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsyMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsz0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χszMN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrMN As Double
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
			Name="DτF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DτFF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DτP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Dτr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LastSourceStep"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="MainStepsInSourceStep"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
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
			Name="StepPowerFF"
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
			Name="WhereInSourceStep"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ιMN"
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
			Name="VMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaxMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χayMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χazMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsxMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsyMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χszMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrMN"
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
			Name="DτIdeal"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΘMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΦMN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="N"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
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
	#tag EndViewBehavior
End Class
#tag EndClass
