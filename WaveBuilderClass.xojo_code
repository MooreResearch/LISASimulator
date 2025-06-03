#tag Class
Protected Class WaveBuilderClass
	#tag Method, Flags = &h21
		Private Sub AssembleDerivatives()
		  '// Local variables to hold cross and plus polarizations
		  'Var hp As Double
		  'Var hx As Double
		  '
		  '// Calculate amplitude and derivatives of the amplitude
		  'CalculateAmplitudes(A)
		  'CalculateAmplitudes(DADδ)
		  'CalculateAmplitudes(DADβ)
		  'CalculateAmplitudes(DADι)
		  'CalculateAmplitudes(DADχax)
		  'CalculateAmplitudes(DADχay)
		  'CalculateAmplitudes(DADχaz)
		  'CalculateAmplitudes(DADχsx)
		  'CalculateAmplitudes(DADχsy)
		  'CalculateAmplitudes(DADχsz)
		  '
		  '// Calculate the wave arrays W, DWDΨ, and DWDα
		  'CalculateWaveFactors
		  '
		  '// These variables project the particular wave onto the detector.
		  '// These default values assume an L-shaped detector perpendicular
		  '// to the line of sight. This will be selected if Parameters.Detectors = 0.
		  '// To get the plus polarization in the source frame, choose ψ = 0.
		  '// To get the cross polarization, choose ψ = π/4.
		  'fp = Cos2ψ
		  'fx = Sin2ψ
		  '
		  '// Otherwise, we will calculate these factors for the LISA detector,
		  '// which requires a more complicated calculation
		  'If Parameters.Detectors > 0 Then
		  'Var ρ As Double = Parameters.GMΩe*τrDN
		  'Var s210 As Double = Sin(2.0*ρ - σ1)
		  'Var s012 As Double = Sin(σ1 - 2.0*Parameters.Φ)
		  'Var s412 As Double = Sin(4.0*ρ - σ1 - 2.0*Parameters.Φ)
		  'Var s311 As Double = Sin(3.0*ρ - σ1 - Parameters.Φ)
		  'Var s111 As Double = Sin(ρ - σ1 + Parameters.Φ)
		  'Var c012 As Double = Cos(σ1 - 2.0*Parameters.Φ)
		  'Var c412 As Double = Cos(4.0*ρ - σ1 -2.0*Parameters.Φ)
		  'Var c311 As Double = Cos(3.0*ρ - σ1 - Parameters.Φ)
		  'Var c111 As Double = Cos(ρ - σ1 + Parameters.Φ)
		  '
		  'Var dp As Double = Dpc1*(-6.0*s210 + 9.0*s012 - s412) + Dpc2*C2Θ*(18.0*s210 + 9.0*s012 - s412) _
		  '- Dpc3*S2Θ*(s311 - 3.0*s111)
		  'Var dx As Double = Dxc1*CΘ*(9.0*c012 - c412) - Dxc2*SΘ*(s311 - 3.0*s111)
		  'Var ddpdΘ As Double = -2.0*Dpc2*S2Θ*(18.0*s210 + 9.0*s012 - s412) - 2.0*Dpc3*C2Θ*(s311 - 3.0*s111)
		  'Var ddxdΘ As Double = -Dxc1*SΘ*(9.0*c012 - c412) - Dxc2*CΘ*(s311 - 3.0*s111)
		  'Var ddpdΦ As Double = Dpc1*(-18.0*c012 + 2.0*c412) + Dpc2*C2Θ*(-18.0*c012 + 2.0*c412) _
		  '+ Dpc3*S2Θ*(c311 - 3.0*c111)
		  'Var ddxdΦ As Double = Dxc1*CΘ*(18.0*s012 - 2.0*s412) - Dxc2*SΘ*(c311 + 3.0*c111)
		  'Var fp1 As Double = Cos2ψ*dp - Sin2ψ*dx
		  'Var fx1 As Double = Sin2ψ*dp + Cos2ψ*dx
		  'Var dfp1dΘ As Double = Cos2ψ*ddpdΘ - Sin2ψ*ddxdΘ
		  'Var dfx1dΘ As Double = Sin2ψ*ddpdΘ + Cos2ψ*ddxdΘ
		  'Var dfp1dΦ As Double = Cos2ψ*ddpdΦ - Sin2ψ*ddxdΦ
		  'Var dfx1dΦ As Double = Sin2ψ*ddpdΦ + Cos2ψ*ddxdΦ
		  '
		  '// repeat the whole thing again for detector 2
		  'Var fp2 As Double
		  'Var fx2 As Double
		  'Var dfp2dΘ As Double
		  'Var dfx2dΘ As Double
		  'Var dfp2dΦ As Double
		  'Var dfx2dΦ As Double
		  'If Parameters.Detectors = 2 Then
		  '// Note that if we don't have 2 detectors, then the variables above will all be zero.
		  's210 = Sin(2.0*ρ - σ2)
		  's012 = Sin(σ2 - 2.0*Parameters.Φ)
		  's412 = Sin(4.0*ρ - σ2 - 2.0*Parameters.Φ)
		  's311 = Sin(3.0*ρ - σ2 - Parameters.Φ)
		  's111 = Sin(ρ - σ2 - Parameters.Φ)
		  'c012 = Cos(σ2 - 2.0*Parameters.Φ)
		  'c412 = Cos(4.0*ρ - σ2 -2.0*Parameters.Φ)
		  'c311 = Cos(3.0*ρ - σ2 - Parameters.Φ)
		  'c111 = Cos(ρ - σ2 + Parameters.Φ)
		  '
		  'dp = Dpc1*(-6.0*s210 + 9.0*s012 - s412) + Dpc2*C2Θ*(18.0*s210 + 9.0*s012 - s412) _
		  '- Dpc3*S2Θ*(s311 - 3.0*s111)
		  'dx = Dxc1*CΘ*(9.0*c012 - c412) - Dxc2*SΘ*(s311 - 3.0*s111)
		  'ddpdΘ = -2.0*Dpc2*S2Θ*(18.0*s210 + 9.0*s012 - s412) - 2.0*Dpc3*C2Θ*(s311 - 3.0*s111)
		  'ddxdΘ = -Dxc1*CΘ*(9.0*c012 - c412) - Dxc2*CΘ*(s311 - 3.0*s111)
		  'ddpdΦ = Dpc1*(-18.0*c012 + 2.0*c412) + Dpc2*C2Θ*(-18.0*c012 + 2.0*c412) _
		  '+ Dpc3*S2Θ*(c311 - 3.0*c111)
		  'ddxdΦ = Dxc1*CΘ*(18.0*s012 - 2.0*s412) - Dxc2*SΘ*(c311 + 3.0*c111)
		  'fp2 = Cos2ψ*dp - Sin2ψ*dx
		  'fx2 = Sin2ψ*dp + Cos2ψ*dx
		  'dfp2dΘ = Cos2ψ*ddpdΘ - Sin2ψ*ddxdΘ
		  'dfx2dΘ = Sin2ψ*ddpdΘ + Cos2ψ*ddxdΘ
		  'dfp2dΦ = Cos2ψ*ddpdΦ - Sin2ψ*ddxdΦ
		  'dfx2dΦ = Sin2ψ*ddpdΦ + Cos2ψ*ddxdΦ
		  'End If
		  'fp = fp1 + fp2
		  'fx = fx1 + fx2
		  'End If
		  '
		  '// Get the wave itself
		  'hp = GetHSum(A,W,Plus)
		  'hx = GetHSum(A,W,Cross)
		  'H = H0*(fp*hp + fx*hx)
		  '
		  '// Now let's do the β-derivative, which is the easiest
		  'hp = GetHSum(DADβ,W,Plus)
		  'hx = GetHSum(DADβ,W,Cross)
		  'DH(dβ) = h0*(fp*hp + fx*hx)
		  '
		  '// Calculate the δ derivative, which is the worst
		  'Var dhp As Double = GetHSum(DADδ,W,Plus) 
		  'dhp = dhp + GetHSum(DADι,W,Plus)*SpinResults.DιI(Dδ)
		  'dhp = dhp + GetHSum(DADχax,W,Plus)*SpinResults.DχaxI(Dδ)
		  'dhp = dhp + GetHSum(DADχay,W,Plus)*SpinResults.DχayI(Dδ)
		  'dhp = dhp + GetHSum(DADχaz,W,Plus)*SpinResults.DχazI(Dδ)
		  'dhp = dhp + GetHSum(DADχsx,W,Plus)*SpinResults.DχsxI(Dδ)
		  'dhp = dhp + GetHSum(DADχsy,W,Plus)*SpinResults.DχsyI(Dδ)
		  'dhp = dhp + GetHSum(DADχsz,W,Plus)*SpinResults.DχszI(Dδ)
		  'dhp = dhp + GetHSum(A,DWDα,Plus)*SpinResults.DαI(Dδ)
		  'dhp = dhp + GetHSum(A,DWDΨ,Plus)*SpinResults.DΨI(Dδ)
		  'dhp = dhp + GetHSum(A,W,Plus,Vderiv)*SpinResults.DVI(Dδ)
		  'Var dhx As Double = GetHSum(DADδ,W,Cross)
		  'dhx = dhx + GetHSum(DADι,W,Cross)*SpinResults.DιI(Dδ) 
		  'dhx = dhx + GetHSum(DADχax,W,Cross)*SpinResults.DχaxI(Dδ)
		  'dhx = dhx + GetHSum(DADχay,W,Cross)*SpinResults.DχayI(Dδ)
		  'dhx = dhx + GetHSum(DADχaz,W,Cross)*SpinResults.DχazI(Dδ)
		  'dhx = dhx + GetHSum(DADχsx,W,Cross)*SpinResults.DχsxI(Dδ)
		  'dhx = dhx + GetHSum(DADχsy,W,Cross)*SpinResults.DχsyI(Dδ)
		  'dhx = dhx + GetHSum(DADχsz,W,Cross)*SpinResults.DχszI(Dδ)
		  'dhx = dhx + GetHSum(A,DWDα,Cross)*SpinResults.DαI(Dδ)
		  'dhx = dhx + GetHSum(A,DWDΨ,Cross)*SpinResults.DΨI(Dδ)
		  'dhx = dhx + GetHSum(A,W,Cross,Vderiv)*SpinResults.DVI(Dδ)
		  'DH(Dδ) = h0*(fp*dhp+fx*dhx) + dH0(Dδ)*(fp*hp+fx*hx)
		  '
		  '// Calculate the τc derivative
		  'dhp = GetHSum(DADι,W,Plus)*SpinResults.DιI(Dlnτc)
		  'dhp = dhp + GetHSum(DADχax,W,Plus)*SpinResults.DχaxI(Dlnτc)
		  'dhp = dhp + GetHSum(DADχay,W,Plus)*SpinResults.DχayI(Dlnτc)
		  'dhp = dhp + GetHSum(DADχaz,W,Plus)*SpinResults.DχazI(Dlnτc)
		  'dhp = dhp + GetHSum(DADχsx,W,Plus)*SpinResults.DχsxI(Dlnτc)
		  'dhp = dhp + GetHSum(DADχsy,W,Plus)*SpinResults.DχsyI(Dlnτc)
		  'dhp = dhp + GetHSum(DADχsz,W,Plus)*SpinResults.DχszI(Dlnτc)
		  'dhp = dhp + GetHSum(A,DWDα,Plus)*SpinResults.DαI(Dlnτc)
		  'dhp = dhp + GetHSum(A,DWDΨ,Plus)*SpinResults.DΨI(Dlnτc)
		  'dhp = dhp + GetHSum(A,W,Plus,Vderiv)*SpinResults.DVI(Dlnτc)
		  'dhx = GetHSum(DADι,W,Cross)*SpinResults.DιI(Dlnτc) 
		  'dhx = dhx + GetHSum(DADχax,W,Cross)*SpinResults.DχaxI(Dlnτc)
		  'dhx = dhx + GetHSum(DADχay,W,Cross)*SpinResults.DχayI(Dlnτc)
		  'dhx = dhx + GetHSum(DADχaz,W,Cross)*SpinResults.DχazI(Dlnτc)
		  'dhx = dhx + GetHSum(DADχsx,W,Cross)*SpinResults.DχsxI(Dlnτc)
		  'dhx = dhx + GetHSum(DADχsy,W,Cross)*SpinResults.DχsyI(Dlnτc)
		  'dhx = dhx + GetHSum(DADχsz,W,Cross)*SpinResults.DχszI(Dlnτc)
		  'dhx = dhx + GetHSum(A,DWDα,Cross)*SpinResults.DαI(Dlnτc)
		  'dhx = dhx + GetHSum(A,DWDΨ,Cross)*SpinResults.DΨI(Dlnτc)
		  'dhx = dhx + GetHSum(A,W,Cross,Vderiv)*SpinResults.DVI(Dlnτc)
		  'DH(Dlnτc) = h0*(fp*dhp+fx*dhx)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub AssignWaveFactors(wfs(, , , ) As Double, outW() As Double)
		  // This method assigns wave factors according to the wave table.
		  // The first parameter is the array that contains the wavefactors.
		  // The second array contains the target array to fill with the appropriate factors.
		  // Note that specifying a table of factor derivatives for the first parameter
		  // outputs the appropriate derivatives to the target array while using the
		  // same code from the wave table.
		  
		  // Note that assignments here must agree with CalculateWaveFactors
		  Var cos As Integer = 0
		  Var sin As Integer = 1
		  Var plus As Integer = 0
		  Var minus As Integer = 1
		  
		  // Now assign all the wave factors to their correct spots in the wave array
		  
		  // order 0, plus polarization
		  outW(0) = wfs(cos,2,plus,2)
		  outW(1) = wfs(cos,1,plus,2)
		  outW(2) = wfs(cos,1,minus,2)
		  outW(3) = wfs(cos,2,minus,2)
		  outW(4) = wfs(cos,0,plus,2)
		  
		  // order 1/2, plus polarization
		  outW(5) = wfs(cos,3,plus,3)
		  outW(6) = wfs(cos,1,plus,1)
		  outW(7) = wfs(cos,1,minus,1)
		  outW(8) = wfs(cos,3,plus,1)
		  outW(9) = wfs(cos,1,plus,3)
		  outW(10) = wfs(cos,1,minus,3)
		  outW(11) = wfs(cos,3,minus,1)
		  outW(12) = wfs(cos,3,minus,3)
		  outW(13) = wfs(cos,0,plus,3)
		  outW(14) = wfs(cos,2,plus,1)
		  outW(15) = wfs(cos,2,plus,3)
		  outW(16) = wfs(cos,2,minus,1)
		  outW(17) = wfs(cos,2,minus,3)
		  outW(18) = wfs(cos,0,plus,1)
		  
		  // order 2/2, plus polarization
		  outW(19) = wfs(cos,2,plus,2)
		  outW(20) = wfs(cos,4,plus,4)
		  outW(21) = wfs(cos,3,plus,4)
		  outW(22) = wfs(cos,3,plus,2)
		  outW(23) = wfs(cos,2,plus,4)
		  outW(24) = wfs(cos,4,plus,2)
		  outW(25) = wfs(cos,1,plus,4)
		  outW(26) = wfs(cos,1,minus,2)
		  outW(27) = wfs(cos,2,minus,2)
		  outW(28) = wfs(cos,1,minus,4)
		  outW(29) = wfs(cos,3,minus,2)
		  outW(30) = wfs(cos,2,minus,4)
		  outW(31) = wfs(cos,4,minus,2)
		  outW(32) = wfs(cos,3,minus,4)
		  outW(33) = wfs(cos,4,minus,4)
		  outW(34) = wfs(cos,0,plus,2)
		  outW(35) = wfs(cos,0,plus,4)
		  outW(36) = wfs(cos,1,plus,2)
		  
		  // order 2/2, SO, plus polarization
		  outW(37) = wfs(cos,1,plus,1)
		  outW(38) = wfs(cos,1,minus,1)
		  outW(39) = wfs(sin,1,minus,1)
		  outW(40) = wfs(sin,0,plus,1)
		  outW(41) = wfs(sin,1,plus,1)
		  outW(42) = wfs(cos,1,plus,1)
		  outW(43) = wfs(cos,1,minus,1)
		  outW(44) = wfs(sin,1,minus,1)
		  outW(45) = wfs(sin,0,plus,1)
		  outW(46) = wfs(sin,1,plus,1)
		  
		  // order 3/2, plus polarization
		  outW(47) = wfs(cos,2,plus,2)
		  outW(48) = wfs(cos,1,plus,2)
		  outW(49) = wfs(cos,1,minus,2)
		  outW(50) = wfs(cos,2,minus,2)
		  outW(51) = wfs(cos,0,plus,2)
		  outW(52) = wfs(cos,5,plus,5)
		  outW(53) = wfs(cos,1,plus,1)
		  outW(54) = wfs(cos,3,plus,3)
		  outW(55) = wfs(cos,4,plus,5)
		  outW(56) = wfs(cos,4,plus,3)
		  outW(57) = wfs(cos,5,plus,3)
		  outW(58) = wfs(cos,1,minus,1)
		  outW(59) = wfs(cos,3,plus,1)
		  outW(60) = wfs(cos,3,plus,5)
		  outW(61) = wfs(cos,1,plus,3)
		  outW(62) = wfs(cos,2,plus,5)
		  outW(63) = wfs(cos,4,plus,1)
		  outW(64) = wfs(cos,5,plus,1)
		  outW(65) = wfs(cos,3,minus,1)
		  outW(66) = wfs(cos,1,plus,5)
		  outW(67) = wfs(cos,1,minus,3)
		  outW(68) = wfs(cos,4,minus,1)
		  outW(69) = wfs(cos,5,minus,1)
		  outW(70) = wfs(cos,3,minus,3)
		  outW(71) = wfs(cos,1,minus,5)
		  outW(72) = wfs(cos,2,minus,5)
		  outW(73) = wfs(cos,4,minus,3)
		  outW(74) = wfs(cos,5,minus,3)
		  outW(75) = wfs(cos,3,minus,5)
		  outW(76) = wfs(cos,4,minus,5)
		  outW(77) = wfs(cos,5,minus,5)
		  outW(78) = wfs(cos,0,plus,3)
		  outW(79) = wfs(cos,0,plus,5)
		  outW(80) = wfs(cos,2,plus,3)
		  outW(81) = wfs(cos,2,minus,3)
		  outW(82) = wfs(cos,2,plus,1)
		  outW(83) = wfs(cos,2,minus,1)
		  outW(84) = wfs(cos,0,plus,1)
		  
		  // Order 3/2, SO, plus polarization
		  outW(85) = wfs(cos,0,plus,0)
		  outW(86) = wfs(cos,2,plus,2)
		  outW(87) = wfs(cos,3,plus,2)
		  outW(88) = wfs(cos,3,minus,2)
		  outW(89) = wfs(cos,1,plus,2)
		  outW(90) = wfs(cos,1,minus,2)
		  outW(91) = wfs(cos,2,minus,2)
		  outW(92) = wfs(cos,0,plus,0)
		  outW(93) = wfs(cos,3,plus,0)
		  outW(94) = wfs(cos,0,plus,2)
		  outW(95) = wfs(cos,2,plus,0)
		  outW(96) = wfs(cos,1,plus,0)
		  outW(97) = wfs(sin,1,plus,0)
		  outW(98) = wfs(sin,2,plus,0)
		  outW(99) = wfs(sin,3,plus,0)
		  outW(100) = wfs(sin,1,minus,2)
		  outW(101) = wfs(sin,2,minus,2)
		  outW(102) = wfs(sin,3,minus,2)
		  outW(103) = wfs(sin,0,plus,2)
		  outW(104) = wfs(sin,1,plus,2)
		  outW(105) = wfs(sin,2,plus,2)
		  outW(106) = wfs(sin,3,plus,2)
		  outW(107) = wfs(cos,0,plus,0)
		  outW(108) = wfs(cos,2,plus,2)
		  outW(109)= wfs(cos,3,plus,2)
		  outW(110) = wfs(cos,3,minus,2)
		  outW(111) = wfs(cos,1,plus,2)
		  outW(112) = wfs(cos,1,minus,2)
		  outW(113) = wfs(cos,2,minus,2)
		  outW(114) = wfs(cos,0,plus,0)
		  outW(115) = wfs(cos,3,plus,0)
		  outW(116) = wfs(cos,0,plus,2)
		  outW(117) = wfs(cos,2,plus,0)
		  outW(118) = wfs(cos,1,plus,0)
		  outW(119) = wfs(sin,1,plus,0)
		  outW(120) = wfs(sin,2,plus,0)
		  outW(121) = wfs(sin,3,plus,0)
		  outW(122) = wfs(sin,1,minus,2)
		  outW(123) = wfs(sin,2,minus,2)
		  outW(124) = wfs(sin,3,minus,2)
		  outW(125) = wfs(sin,0,plus,2)
		  outW(126) = wfs(sin,1,plus,2)
		  outW(127) = wfs(sin,2,plus,2)
		  outW(128) = wfs(sin,3,plus,2)
		  
		  // Order 0, cross polarization
		  outW(129) = wfs(sin,1,minus,2)
		  outW(130) = wfs(sin,2,minus,2)
		  outW(131) = wfs(sin,1,plus,2)
		  outW(132) = wfs(sin,2,plus,2)
		  
		  // Order 1/2, cross polarization
		  outW(133) = wfs(sin,1,minus,3)
		  outW(134) = wfs(sin,2,minus,3)
		  outW(135) = wfs(sin,3,minus,3)
		  outW(136) = wfs(sin,1,minus,1)
		  outW(137) = wfs(sin,2,minus,1)
		  outW(138) = wfs(sin,3,minus,1)
		  outW(139) = wfs(sin,0,plus,1)
		  outW(140) = wfs(sin,1,plus,1)
		  outW(141) = wfs(sin,2,plus,1)
		  outW(142) = wfs(sin,3,plus,1)
		  outW(143) = wfs(sin,1,plus,3)
		  outW(144) = wfs(sin,2,plus,3)
		  outW(145) = wfs(sin,3,plus,3)
		  
		  // Order 2/2, cross polarization
		  outW(146) = wfs(sin,1,minus,4)
		  outW(147) = wfs(sin,2,minus,4)
		  outW(148) = wfs(sin,3,minus,4)
		  outW(149) = wfs(sin,4,minus,4)
		  outW(150) = wfs(sin,1,minus,2)
		  outW(151) = wfs(sin,2,minus,2)
		  outW(152) = wfs(sin,3,minus,2)
		  outW(153) = wfs(sin,4,minus,2)
		  outW(154) = wfs(sin,0,plus,2)
		  outW(155) = wfs(sin,1,plus,2)
		  outW(156) = wfs(sin,2,plus,2)
		  outW(157) = wfs(sin,3,plus,2)
		  outW(158) = wfs(sin,4,plus,2)
		  outW(159) = wfs(sin,1,plus,4)
		  outW(160) = wfs(sin,2,plus,4)
		  outW(161) = wfs(sin,3,plus,4)
		  outW(162) = wfs(sin,4,plus,4)
		  
		  // Order 2/2, SO, cross polarization
		  outW(163) = wfs(cos,1,plus,1)
		  outW(164) = wfs(cos,1,minus,1)
		  outW(165) = wfs(sin,1,minus,1)
		  outW(166) = wfs(sin,0,plus,1)
		  outW(167) = wfs(sin,1,plus,1)
		  outW(168) = wfs(cos,1,plus,1)
		  outW(169) = wfs(cos,1,minus,1)
		  outW(170) = wfs(sin,1,minus,1)
		  outW(171) = wfs(sin,0,plus,1)
		  outW(172) = wfs(sin,1,plus,1)
		  
		  // Order 3/2, cross polarization
		  outW(173) = wfs(sin,1,minus,2)
		  outW(174) = wfs(sin,2,minus,2)
		  outW(175) = wfs(sin,1,plus,2)
		  outW(176) = wfs(sin,2,plus,2)
		  outW(177) = wfs(sin,1,minus,5)
		  outW(178) = wfs(sin,2,minus,5)
		  outW(179) = wfs(sin,3,minus,5)
		  outW(180) = wfs(sin,4,minus,5)
		  outW(181) = wfs(sin,5,minus,5)
		  outW(182) = wfs(sin,1,minus,3)
		  outW(183) = wfs(sin,2,minus,3)
		  outW(184) = wfs(sin,3,minus,3)
		  outW(185) = wfs(sin,4,minus,3)
		  outW(186) = wfs(sin,5,minus,3)
		  outW(187) = wfs(sin,1,minus,1)
		  outW(188) = wfs(sin,2,minus,1)
		  outW(189) = wfs(sin,3,minus,1)
		  outW(190) = wfs(sin,4,minus,1)
		  outW(191) = wfs(sin,5,minus,1)
		  outW(192) = wfs(sin,0,plus,1)
		  outW(193) = wfs(sin,0,plus,3)
		  outW(194) = wfs(sin,1,plus,1)
		  outW(195) = wfs(sin,2,plus,1)
		  outW(196) = wfs(sin,3,plus,1)
		  outW(197) = wfs(sin,4,plus,1)
		  outW(198) = wfs(sin,5,plus,1)
		  outW(199) = wfs(sin,1,plus,3)
		  outW(200) = wfs(sin,2,plus,3)
		  outW(201) = wfs(sin,3,plus,3)
		  outW(202) = wfs(sin,4,plus,3)
		  outW(203) = wfs(sin,5,plus,3)
		  outW(204) = wfs(sin,1,plus,5)
		  outW(205) = wfs(sin,2,plus,5)
		  outW(206) = wfs(sin,3,plus,5)
		  outW(207) = wfs(sin,4,plus,5)
		  outW(208) = wfs(sin,5,plus,5)
		  
		  // Order 3/2, SO, cross polarization
		  outW(209) = wfs(cos,0,plus,0)
		  outW(210) = wfs(cos,2,plus,2)
		  outW(211) = wfs(cos,3,plus,2)
		  outW(212) = wfs(cos,1,plus,2)
		  outW(213) = wfs(cos,1,minus,2)
		  outW(214) = wfs(cos,2,minus,2)
		  outW(215) = wfs(cos,3,minus,2)
		  outW(216) = wfs(cos,2,plus,0)
		  outW(217) = wfs(cos,0,plus,2)
		  outW(218) = wfs(cos,3,plus,0)
		  outW(219) = wfs(cos,1,plus,0)
		  outW(220) = wfs(sin,1,plus,0)
		  outW(221) = wfs(sin,2,plus,0)
		  outW(222) = wfs(sin,3,plus,0)
		  outW(223) = wfs(sin,1,minus,2)
		  outW(224) = wfs(sin,2,minus,2)
		  outW(225) = wfs(sin,3,minus,2)
		  outW(226) = wfs(sin,0,plus,2)
		  outW(227) = wfs(sin,1,plus,2)
		  outW(228) = wfs(sin,2,plus,2)
		  outW(229) = wfs(sin,3,plus,2)
		  outW(230) = wfs(cos,0,plus,0)
		  outW(231) = wfs(cos,2,plus,2)
		  outW(232) = wfs(cos,1,plus,2)
		  outW(233) = wfs(cos,3,plus,2)
		  outW(234) = wfs(cos,1,minus,2)
		  outW(235) = wfs(cos,2,minus,2)
		  outW(236) = wfs(cos,3,minus,2)
		  outW(237) = wfs(cos,2,plus,0)
		  outW(238) = wfs(cos,0,plus,2)
		  outW(239) = wfs(cos,3,plus,0)
		  outW(240) = wfs(cos,1,plus,0)
		  outW(241) = wfs(sin,1,plus,0)
		  outW(242) = wfs(sin,2,plus,0)
		  outW(243) = wfs(sin,3,plus,0)
		  outW(244) = wfs(sin,1,minus,2)
		  outW(245) = wfs(sin,2,minus,2)
		  outW(246) = wfs(sin,3,minus,2)
		  outW(247) = wfs(sin,0,plus,2)
		  outW(248) = wfs(sin,1,plus,2)
		  outW(249) = wfs(sin,2,plus,2)
		  outW(250) = wfs(sin,3,plus,2)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub CalculateAmplitudes()
		  // The purpose of this function is to calculate the wave amplitude
		  // for all 250 terms in the extended equation table OR the derivatives
		  // of those amplitudes with respect to δ, β, ι, χsx, χsy, χsz, χax, χay, or χaz
		  // as specified by the argument. We are able to use the same set of equations
		  // in all cases because we can substitute the derivatives of the functions of
		  // the variables above in place of the functions as we set things up.
		  // The array parameter is the array to which the output values are written.
		  // It also tells us which derivative we are taking.
		  
		  // Set up the default sources of variables
		  Var δfs As DeltaFuncsClass = δFunctions  // source for the δ functions
		  Var βfs As BetaFuncsClass = βFunctions  // source for the β-dependent functions
		  Var ιfs As IotaFuncsClass = ιFunctions // source for the ι-dependent functions
		  Var χsx As Double = SpinResults.χsx
		  Var χsy As Double = SpinResults.χsy
		  Var χsz As Double = SpinResults.χsz
		  Var χax As Double = SpinResults.χax
		  Var χay As Double = SpinResults.χay
		  Var χaz As Double = SpinResults.χaz
		  Var pnOrder As Integer = Parameters.PNForA
		  
		  // Load the δ functions into local variables
		  Var δ As Double = δfs.δ
		  Var δ0 As Double = δfs.δ0
		  Var δ1 As Double = δfs.δ1
		  Var δ2 As Double = δfs.δ2
		  Var η As Double = δfs.η
		  Var η10 As Double = δfs.η10
		  Var η2 As Double = δfs.η2
		  Var η3 As Double = δfs.η3
		  
		  // Load the β functions into local variables
		  Var c0β As Double = βfs.c0β
		  Var c2β As Double = βfs.c2β
		  Var c2βs2β As Double = βfs.c2βs2β
		  Var c2βsβ As Double = βfs.c2βsβ
		  Var c2βsβ2 As Double = βfs.c2βsβ2
		  Var c2βsβ3 As Double = βfs.c2βsβ3
		  Var c2β2sβ3 As Double = βfs.c2β2sβ3
		  Var c3β As Double = βfs.c3β
		  Var c3βsβ2 As Double = βfs.c3βsβ2
		  Var c4β As Double = βfs.c4β
		  Var c4βsβ As Double = βfs.c4βsβ
		  Var c5β As Double = βfs.c5β
		  Var cβ As Double = βfs.cβ
		  Var cβ2 As Double = βfs.cβ2
		  Var cβ2sβ As Double = βfs.cβ2sβ
		  Var cβ2s2β As Double =  βfs.cβ2s2β
		  Var cβ3sβ As Double = βfs.cβ3sβ
		  Var cβc2β As Double = βfs.cβc2β
		  Var cβc2βsβ2 As Double = βfs.cβc2βsβ2
		  Var cβc4β As Double = βfs.cβc4β
		  Var cβs3β As Double = βfs.cβs3β
		  Var cβsβ As Double = βfs.cβsβ
		  Var cβsβ2 As Double = βfs.cβsβ2
		  Var cβsβ3 As Double = βfs.cβsβ3
		  Var s2β As Double = βfs.s2β
		  Var s3β As Double = βfs.s3β
		  Var s4β As Double = βfs.s4β
		  Var s5β As Double = βfs.s5β
		  Var sβ As Double = βfs.sβ
		  Var sβ2 As Double = βfs.sβ2
		  Var sβ3 As Double = βfs.sβ3
		  
		  // Load the ι functions into local variables
		  Var c0 As Double = ιfs.c0
		  Var c1 As Double = ιfs.c1
		  Var c10 As Double = ιfs.c10
		  Var c110 As Double = ιfs.c110
		  Var c12 As Double = ιfs.c12
		  Var c12s14 As Double = ιfs.c12s14
		  Var c12s16 As Double = ιfs.c12s16
		  Var c12s18 As Double = ιfs.c12s18
		  Var c13s1 As Double = ιfs.c13s1
		  Var c13s15 As Double = ιfs.c13s15
		  Var c13s17 As Double = ιfs.c13s17
		  Var c13s3 As Double = ιfs.c13s3
		  Var c13s5 As Double = ιfs.c13s5
		  Var c13s7 As Double = ιfs.c13s7
		  Var c14 As Double = ιfs.c14
		  Var c14s12 As Double = ιfs.c14s12
		  Var c14s16 As Double = ιfs.c14s16
		  Var c15s1 As Double = ιfs.c15s1
		  Var c15s12 As Double = ιfs.c15s12
		  Var c15s13 As Double = ιfs.c15s13
		  Var c16 As Double = ιfs.c16
		  Var c16s12 As Double = ιfs.c16s12
		  Var c16s14 As Double = ιfs.c16s14
		  Var c17s1 As Double = ιfs.c17s1
		  Var c17s13 As Double = ιfs.c17s13
		  Var c18 As Double = ιfs.c18
		  Var c18s12 As Double = ιfs.c18s12
		  Var c19s1 As Double = ιfs.c19s1
		  Var c1c2s13 As Double = ιfs.c1c2s13
		  Var c1c2s15 As Double = ιfs.c1c2s15
		  Var c1c2s17 As Double = ιfs.c1c2s17
		  Var c1c4s13 As Double = ιfs.c1c4s13
		  Var c1c4s15 As Double = ιfs.c1c4s15
		  Var c1c6s13 As Double = ιfs.c1c6s13
		  Var c1s13 As Double = ιfs.c1s13
		  Var c1s15 As Double = ιfs.c1s15
		  Var c1s17 As Double = ιfs.c1s17
		  Var c1s19 As Double = ιfs.c1s19
		  Var c2 As Double = ιfs.c2
		  Var c23 As Double = ιfs.c23
		  Var c2c12 As Double = ιfs.c2c12
		  Var c2c12s14 As Double = ιfs.c2c12s14
		  Var c2c13s1 As Double = ιfs.c2c13s1
		  Var c2c13s15 As Double = ιfs.c2c13s15
		  Var c2c14 As Double = ιfs.c2c14
		  Var c2c14s12 As Double = ιfs.c2c14s12
		  Var c2c15s1 As Double = ιfs.c2c15s1
		  Var c2c15s12 As Double = ιfs.c2c15s12
		  Var c2c15s13 As Double = ιfs.c2c15s13
		  Var c2c16 As Double = ιfs.c2c16
		  Var c2c17s1 As Double = ιfs.c2c17s1
		  Var c2s12 As Double = ιfs.c2s12
		  Var c2s14 As Double = ιfs.c2s14
		  Var c2s16 As Double = ιfs.c2s16
		  Var c2s2 As Double = ιfs.c2s2
		  Var c2s22 As Double = ιfs.c2s22
		  Var c2s23 As Double = ιfs.c2s23
		  Var c3s13 As Double = ιfs.c3s13
		  Var c4 As Double = ιfs.c4
		  Var c4c12 As Double = ιfs.c4c12
		  Var c4c12s14 As Double = ιfs.c4c12s14
		  Var c4c13s1 As Double = ιfs.c4c13s1
		  Var c4c14 As Double = ιfs.c4c14
		  Var c4c14s12 As Double = ιfs.c4c14s12
		  Var c4c15s1 As Double = ιfs.c4c15s1
		  Var c4c16 As Double = ιfs.c4c16
		  Var c4s12 As Double = ιfs.c4s12
		  Var c4s14 As Double = ιfs.c4s14
		  Var c4s16 As Double = ιfs.c4s16
		  Var c4s2 As Double = ιfs.c4s2
		  Var c4s22 As Double = ιfs.c4s22
		  Var c4s23 As Double = ιfs.c4s23
		  Var c5s13 As Double = ιfs.c5s13
		  Var c6 As Double = ιfs.c6
		  Var c6c12 As Double = ιfs.c6c12
		  Var c6c13s1 As Double = ιfs.c6c13s1
		  Var c6s12 As Double = ιfs.c6s12
		  Var c6s2 As Double = ιfs.c6s2
		  Var c7s13 As Double = ιfs.c7s13
		  Var c8 As Double = ιfs.c8
		  Var c8c12 As Double = ιfs.c8c12
		  Var c8s12 As Double = ιfs.c8s12
		  Var c8s2 As Double = ιfs.c8s2
		  Var s1 As Double = ιfs.s1
		  Var s10 As Double = ιfs.s10
		  Var s110 As Double = ιfs.s110
		  Var s12 As Double = ιfs.s12
		  Var s13 As Double = ιfs.s13
		  Var s14 As Double = ιfs.s14
		  Var s16 As Double = ιfs.s16
		  Var s18 As Double = ιfs.s18
		  Var s2 As Double = ιfs.s2
		  Var s22 As Double = ιfs.s22
		  Var s23 As Double = ιfs.s23
		  Var s24 As Double = ιfs.s24
		  Var s25 As Double = ιfs.s25
		  Var s4 As Double = ιfs.s4
		  Var s6 As Double = ιfs.s6
		  Var s8 As Double = ιfs.s8
		  
		  // Now we will calculate all of the amplitudes
		  
		  // Clear out anything in the output array to make sure we don't
		  // have leftover stuff that might interfere with things
		  For i As Integer = 0 To 250
		    A(i) = 0.0
		  Next
		  
		  // Order 0, plus polarization
		  A(0) = (-3/2) * δ0 * c0β * c14 - (1/2) * δ0 * c2β * c14
		  A(1) = -2 * δ0 * s2β * c13s1
		  A(2) = 2 * δ0 * s2β * c1s13
		  A(3) = (-3/2) * δ0 * c0β * s14 - (1/2) * δ0 * c2β * s14
		  A(4) = (-3/2) * δ0 * sβ2 * s22
		  
		  // Order 0, cross polarization
		  A(129) = 4 * δ0 * sβ * c1s13
		  A(130) = -2 * δ0 * cβ * s14
		  A(131) = -4 * δ0 * sβ * c13s1
		  A(132) = -2 * δ0 * cβ * c14
		  
		  If PNOrder > 0 Then
		    // Amplitude factors for H1P
		    A(5) = (-45/32) * δ * sβ * c16 - (9/32) * δ * s3β * c16
		    A(6) = (-175/256) * δ * sβ * c12 + (87/64) * δ * sβ * c2c12 - (5/64) * δ * s3β * c2c12 - (5/256) * δ * sβ * c4c12 + (15/256) * δ * s3β * c4c12 + (13/256) * δ * s3β * c12
		    A(7) = (175/256) * δ * sβ * s12 + (87/64) * δ * sβ * c2s12 - (5/64) * δ * s3β * c2s12 + (5/256) * δ * sβ * c4s12 - (15/256) * δ * s3β * c4s12 - (13/256) * δ * s3β * s12
		    A(8) = (-5/32) * δ * sβ * c14s12 - (1/32) * δ * s3β * c14s12
		    A(9) = (-45/32) * δ * sβ * c14s12 + (135/32) * δ * s3β * c14s12
		    A(10) = (45/32) * δ * sβ * c12s14 - (135/32) * δ * s3β * c12s14
		    A(11) = (5/32) * δ * sβ * c12s14 + (1/32) * δ * s3β * c12s14
		    A(12) = (27/16) * δ * sβ * s16 + (9/16) * δ * c2βsβ * s16
		    A(13) = (45/16) * δ * cβsβ2 * s23
		    A(14) = (-85/256) * δ * cβ * s2 - (1/128) * δ * cβc2β * s2 - (1/32) * δ * cβc2β * c2s2 - (3/128) * δ * cβc2β * c4s2 - (11/64) * δ * cβ * s4 - (1/256) * δ * cβ * s6
		    A(15) = (45/256) * δ * cβ * s2 + (81/128) * δ * cβc2β * s2 + (27/32) * δ * cβc2β * c2s2 + (27/128) * δ * cβc2β * c4s2 + (9/64) * δ * cβ * s4 + (9/256) * δ * cβ * s6
		    A(16) = (1/256) * δ * cβc2β * s2 - (85/256) * δ * cβ * s2 + (11/64) * δ * cβ * s4 + (1/64) * δ * cβc2β * s4 - (1/256) * δ * cβ * s6 + (3/256) * δ * cβc2β * s6
		    A(17) = (45/256) * δ * cβ * s2 + (135/256) * δ * cβc2β * s2 - (9/64) * δ * cβ * s4 - (27/64) * δ * cβc2β * s4 + (9/256) * δ * cβ * s6 + (27/256) * δ * cβc2β * s6
		    A(18) = (1/64) * δ * cβsβ2 * s2 + (5/64) * δ * cβsβ2 * s6
		    
		    // Order 1/2, Cross polarization
		    A(136) = (-1/64) * δ * cβsβ * c0 + (43/128) * δ * cβsβ * c2 - (23/128) * δ * s2β * c4 + (5/256) * δ * s2β * c6
		    A(137) = (1/4) * δ * c2β * c1c2s13 - (1/4) * δ * c2β * c1s13 - δ * c1s13
		    A(138) = (1/8) * δ * s2β * c12s14
		    A(139) = (1/2) * δ * sβ2 * s4
		    A(140) = (1/64) * δ * cβsβ * c0 + (43/128) * δ * cβsβ * c2 + (23/128) * δ * s2β * c4 + (5/256) * δ * s2β * c6
		    A(141) = (1/4) * δ * c2β * c2c13s1 - (1/4) * δ * c2β * c13s1 - δ * c0β * c13s1
		    A(142) = (-1/8) * δ * s2β * c14s12
		    A(143) = (45/8) * δ * s2β * c14s12
		    A(144) = (9/2) * δ * c2β * c15s1
		    A(145) = (-9/8) * δ * s2β * c16
		    
		  End If
		  
		  If PNOrder > 1 Then
		    // Order 2/2, Plus polarization
		    A(19) = (59/16) * δ0 * c0β * c14 + (5/2) * δ0 * c2β * c14 - (25/16) * η * c0β * c14 - (13/3) * η * c2β * c14 + (9/16) * η3 * c4β * c14 - (5/8) * η3 * c0β * c2c14 + (11/2) * η3 * c2β * c2c14 - (7/8) * η3 * c4β * c2c14 + (5/16) * η3 * c0β * c4c14 + (1/4) * η3 * c2β * c4c14 + (7/16) * η3 * c4β * c4c14
		    A(20) = 6 * η3 * sβ2 * c0β * c18 + 2 * η3 * c2βsβ2 * c18
		    A(21) = -32 * η3 * cβ3sβ * c17s1
		    A(22) = (5/2) * η3 * s2β * c15s1 - (1/2) * η3 * c2βs2β * c15s1 + 2 * η3 * cβ2s2β * c2c15s1
		    A(23) = 10 * η3 * c0β * c16s12 + 8 * η3 * c2β * c16s12 + 14 * η3 * c4β * c16s12
		    A(24) = (3/2) * η3 * sβ2 * c16s12 + (1/2) * η3 * c2βsβ2 * c16s12
		    A(25) = 56 * η * c2βs2β * c15s13 - 8 * η * s2β * c15s13
		    A(26) = (16/3) * η * s2β * c1s13 + (31/4) * η3 * s2β * c1c2s13 + (1/4) * η3 * s2β * c1c4s13 - (19/16) * η3 * s4β * c1s13 - (7/8) * η3 * s4β * c3s13 - (7/16) * η3 * s4β * c5s13 - 6 * s2β * c1s13
		    A(27) = (59/16) * δ0 * c0β * s14 + (5/2) * δ0 * c2β * s14 - (25/16) * η * c0β * s14 - (13/3) * η * c2β * s14 + (9/16) * η3 * c4β * s14 + (5/8) * η3 * c0β * c2s14 - (11/2) * η3 * c2β * c2s14 + (7/8) * η3 * c4β * c2s14 + (5/16) * η3 * c0β * c4s14 + (1/4) * η3 * c2β * c4s14 + (7/16) * η3 * c4β * c4s14
		    A(28) = 8 * η3 * s2β * c13s15 - 56 * η3 * c2βs2β * c13s15
		    A(29) = (-5/2) * η3 * s2β * c1s15 + (1/2) * η3 * c2βs2β * c1s15 + 2 * η3 * cβ2s2β * c1c2s15
		    A(30) = 10 * η3 * c0β * c12s16 + 8 * η3 * c2β * c12s16 + 14 * η3 * c4β * c12s16
		    A(31) = (3/2) * η3 * sβ2 * c12s16 + (1/2) * η3 * c2βsβ2 * c12s16
		    A(32) = 32 * η3 * cβ3sβ * c1s17
		    A(33) = 6 * η3 * sβ2 * s18 + 2 * η3 * c2βsβ2 * s18
		    A(34) = (349/96) * δ0 * sβ2 * s22 + (25/32) * η3 * c2βsβ2 * s22 - (45/32) * η * sβ2 * s22 + (25/32) * η3 * sβ2 * c4s22 + (35/32) * η3 * c2βsβ2 * c4s22
		    A(35) = (25/4) * η3 * sβ2 * s24 + (35/4) * η3 * c2βsβ2 * s24
		    A(36) = 6 * δ0 * s2β * c13s1 - (16/3) * η * s2β * c13s1 + (31/4) * η3 * s2β * c2c13s1 - (1/4) * η3 * s2β * c4c13s1 + (19/16) * η3 * s4β * c13s1 - (7/8) * η3 * s4β * c13s3 + (7/16) * η3 * s4β * c13s5
		    
		    // Order 2/2, SO, Plus polarization
		    A(37) = δ0 * cβ * c12 * χax - δ0 * sβ * c12 * χaz
		    A(38) = (1/2) * δ0 * cβ * c0 * χax - (1/2) * δ0 * cβ * c2 * χax - δ0 * sβ * s12 * χaz
		    A(39) = -δ0 * cβ * s12 * χay
		    A(40) = -δ0 * sβ * s2 * χay
		    A(41) = -δ0 * cβ * c12 * χay
		    A(42) = δ * cβ * c12 * χsx - δ * sβ * c12 * χsz
		    A(43) = (1/2) * δ * cβ * χsx - (1/2) * δ * cβ * c2 * χsx - δ * sβ * s12 * χsz
		    A(44) = -δ * cβ * s12 * χsy
		    A(45) = -δ * sβ * s2 * χsy
		    A(46) = -δ * cβ * c12 * χsy
		    
		    //Order 2/2, Cross polarization
		    A(146) = -28 * η3 * s3β * c13s15 - 12 * η3 * sβ * c13s15
		    A(147) = -4 * η3 * c3β * c12s16 + 28 * η3 * cβ * c12s16
		    A(148) = 12 * η3 * s3β * c1s17 - 4 * η3 * sβ * c1s17
		    A(149) = 8 * η3 * cβsβ * s18
		    A(150) = (19/4) * η3 * s3β * c1c2s13 - (9/4) * η3 * sβ * c1c2s13 - (7/8) * η3 * s3β * c1c4s13 - (3/8) * η3 * sβ * c1c4s13 + (9/8) * η3 * s3β * c1s13 + (103/24) * η * sβ * c1s13 - (79/8) * δ0 * sβ * c1s13
		    A(151) = (-1/2) * η3 * c3β * c2s14 - (7/2) * η3 * cβ * c2s14 + (7/8) * η3 * c3β * c4s14 - (3/8) * η3 * c3β * s14 + (1/8) * η3 * cβ * c4s14 - (119/24) * η * cβ * s14 + (47/8) * δ0 * cβ * s14
		    A(152) = 3 * η3 * c2βsβ * c1c2s15 + η3 * sβ * c1c2s15 - 4 * η3 * sβ * c1s15
		    A(153) = 2 * η3 * cβsβ2 * c12s16
		    A(154) = (15/2) * η3 * cβsβ2 * c2s22
		    A(155) = (19/4) * η3 * s3β * c2c13s1 - (9/4) * η3 * sβ * c2c13s1 + (7/8) * η3 * s3β * c4c13s1 + (3/8) * η3 * sβ * c4c13s1 - (9/8) * η3 * s3β * c13s1 - (103/24) * η * sβ * c13s1 + (79/8) * δ0 * sβ * c13s1
		    A(156) = (1/2) * η3 * c3β * c2c14 + (7/2) * η3 * cβ * c2c14 + (7/8) * η3 * c3β * c4c14 - (3/8) * η3 * c3β * c14 + (1/8) * η3 * cβ * c4c14 - (119/24) * η * cβ * c14 + (47/8) * δ0 * cβ * c14
		    A(157) = 3 * η3 * c2βsβ * c2c15s1 + η3 * sβ * c2c15s1 + 4 * η3 * sβ * c15s1
		    A(158) = 2 * η3 * cβsβ2 * c16s12
		    A(159) = 28 * η3 * s3β * c15s13 + 12 * η3 * sβ * c15s13
		    A(160) = 28 * η3 * c3β * c16s12 + 4 * η3 * cβ * c16s12
		    A(161) = -24 * η3 * c2βsβ * c17s1 - 8 * η3 * sβ * c17s1
		    A(162) = 8 * η3 * cβsβ2 * c18
		    
		    // Order 2/2, SO, Cross polarization
		    A(163) = (1/2) * δ0 * c0β * c0 * χay + (1/2) * δ0 * c0β * c2 * χay
		    A(164) = δ0 * c0β * s12 * χay
		    A(165) = (1/2) * δ0 * cβ2 * c0 * χax - (1/2) * δ0 * cβ2 * c2 * χax - (1/2) * δ0 * cβsβ * c0 * χaz + (1/2) * δ0 * cβsβ * c2 * χaz
		    A(166) = δ0 * cβsβ * s2 * χax - δ0 * sβ2 * s2 * χaz
		    A(167) = (1/2) * δ0 * cβ2 * c0 * χax + (1/2) * δ0 * cβ2 * c2 * χax - (1/2) * δ0 * cβsβ * c0 * χaz - (1/2) * δ0 * cβsβ * c2 * χaz
		    A(168) = (1/2) * δ * c0β * c0 * χsy + (1/2) * δ * c0β * c2 * χsy
		    A(169) = δ * c0β * s12 * χsy
		    A(170) = (-1/2) * δ * cβ2 * c2 * χsx + (1/2) * δ * cβ2 * c0 * χsx + (1/2) * δ * cβsβ * c2 * χsz - (1/2) * δ * cβsβ * c0 * χsz
		    A(171) = δ * cβsβ * s2 * χsx - δ * sβ2 * s2 * χsz
		    A(172) = (1/2) * δ * cβ2 * c2 * χsx + (1/2) * δ * cβ2 * c0 * χsx - (1/2) * δ * cβsβ * c2 * χsz - (1/2) * δ * cβsβ * c0 * χsz
		    
		  End If
		  
		  If PNOrder > 2 then
		    // Order 3/2, Plus Polarization
		    A(47) = -3 * π * δ0 * c0β * c14 - π * δ0 * c2β * c14
		    A(48) = -4 * π * δ0 * s2β * c13s1
		    A(49) = 4 * π * δ0 * s2β * c1s13
		    A(50) = -3 * π * δ0 * c0β * s14 - π * δ0 * c2β * s14
		    A(51) = -3 * π * δ0 * sβ2 * s22
		    A(52) = (625/128) * δ2 * sβ3 * c110 + (625/384) * δ2 * c2β2sβ3 * c110
		    A(53) = (19/4096) * δ2 * s3β * c2c12 + (35/12288) * δ2 * s5β * c2c12 + (1873/2048) * δ1 * sβ * c2c12 + (1901/8192) * δ * s3β * c2c12 - (10675/12288) * δ * sβ * c2c12
		    A(54) = (6399/2048) * δ2 * s3β * c2c16 - (2187/2048) * δ2 * s5β * c2c16 - (2403/1024) * δ2 * sβ * c2c16 + (3159/8192) * δ2 * s3β * c4c16 + (3645/8192) * δ2 * s5β * c4c16 + (1701/4096) * δ2 * sβ * c4c16
		    A(55) = (-11875/384) * δ2 * cβsβ2 * c19s1 - (3125/384) * δ2 * c3βsβ2 * c19s1
		    A(56) = (351/128) * δ2 * cβsβ2 * c17s1 - (243/128) * δ2 * cβc2βsβ2 * c17s1 + (567/128) * δ2 * cβsβ2 * c2c17s1 + (405/128) * δ2 * cβc2βsβ2 * c2c17s1
		    A(57) = (243/128) * δ2 * sβ3 * c18s12 + (81/128) * δ2 * c2βsβ3 * c18s12
		    A(58) = (1901/8192) * δ * s3β * c2s12 - (10675/12288) * δ * sβ * c2s12 + (2833/16384) * δ * s3β * c4s12 - (1103/24576) * δ * sβ * c4s12 + (9653/65536) * δ * s3β * s12 - (43723/98304) * δ * sβ * s12
		    A(59) = (151/1024) * δ2 * s3β * c2c14s12 - (3/1024) * δ2 * s5β * c2c14s12 - (27/512) * δ2 * sβ * c2c14s12 + (13/4096) * δ2 * s3β * c4c14s12 + (15/4096) * δ2 * s5β * c4c14s12 + (7/2048) * δ2 * sβ * c4c14s12
		    A(60) = (4375/512) * δ2 * sβ * c18s12 + (8125/1024) * δ2 * s3β * c18s12 + (9375/1024) * δ2 * s5β * c18s12
		    A(61) = (20475/4096) * δ * sβ * c14s12 - (149391/8192) * δ * s3β * c14s12 - (3195/2048) * δ1 * sβ * c14s12 + (45711/4096) * δ1 * s3β * c14s12
		    A(62) = (-4375/192) * δ2 * cβ * c17s13 - (625/128) * δ2 * c3β * c17s13 - (3125/128) * δ2 * c5β * c17s13
		    A(63) = (5/192) * δ2 * cβc2βsβ2 * c2c15s13 + (7/192) * δ2 * cβsβ2 * c2c15s13 + (37/192) * δ2 * cβsβ2 * c15s13
		    A(64) = (1/64) * δ2 * sβ3 * c16s14 + (1/192) * δ2 * c2βsβ3 * c16s14
		    A(65) = (151/1024) * δ2 * s3β * c2c12s14 - (3/1024) * δ2 * s5β * c2c12s14 - (27/512) * δ2 * sβ * c2c12s14 - (13/4096) * δ2 * s3β * c4c12s14 - (15/4096) * δ2 * s5β * c4c12s14 - (7/2048) * δ2 * sβ * c4c12s14
		    A(66) = (4375/512) * δ2 * s3β * c16s14 - (21875/512) * δ2 * s5β * c16s14 + (4375/768) * δ2 * sβ * c16s14
		    A(67) = (-10017/1024) * δ2 * s3β * c2c12s14 + (1701/1024) * δ2 * s5β * c2c12s14 - (2187/512) * δ2 * sβ * c2c12s14 - (1701/4096) * δ2 * s3β * c4c12s14 + (8505/4096) * δ2 * s5β * c4c12s14 - (567/2048) * δ2 * sβ * c4c12s14
		    A(68) = (-5/192) * δ1 * cβc2βsβ2 * c2c13s15 - (1/192) * δ2 * cβc2βsβ2 * c13s15 - (7/192) * δ2 * cβsβ2 * c2c13s15 + (37/192) * δ2 * cβsβ2 * c13s15
		    A(69) = (-1/192) * δ2 * c2βsβ3 * c14s16 - (1/64) * δ2 * sβ3 * c14s16
		    A(70) = (-8145/2048) * δ * c2βsβ * s16 - (55539/8192) * δ * sβ * s16 + (1053/256) * δ2 * c2βsβ * c2s16 - (1701/1024) * δ1 * c2βsβ * c4s16 + (4689/1024) * δ1 * c2βsβ * s16
		    A(71) = (-4375/512) * δ2 * s3β * c14s16 + (21875/512) * δ2 * s5β * c14s16 - (4375/768) * δ2 * sβ * c14s16
		    A(72) = (-625/128) * δ2 * c3β * c13s17 - (3125/128) * δ2 * c5β * c13s17 - (4375/192) * δ2 * cβ * c13s17
		    A(73) = (-405/128) * δ2 * cβc2βsβ2 * c1c2s17 - (243/128) * δ2 * cβc2βsβ2 * c1s17 - (567/128) * δ2 * cβsβ2 * c1c2s17 + (351/128) * δ2 * cβsβ2 * c1s17
		    A(74) = (-243/128) * δ2 * sβ3 * c12s18 - (81/128) * δ2 * c2βsβ3 * c12s18
		    A(75) = (-8125/1024) * δ2 * s3β * c12s18 - (9375/1024) * δ2 * s5β * c12s18 - (4375/512) * δ2 * sβ * c12s18
		    A(76) = (-3125/384) * δ2 * c3βsβ2 * c1s19 - (11875/384) * δ2 * cβsβ2 * c1s19
		    A(77) = (-625/384) * δ2 * c2βsβ3 * s110 - (625/128) * δ2 * sβ3 * s110
		    A(78) = (-5103/2048) * δ2 * cβc2βsβ2 * c4s23 - (3969/2048) * δ2 * cβc2βsβ2 * s23 - (1701/2048) * δ2 * cβsβ2 * c4s23 + (10197/2048) * δ1 * cβsβ2 * s23 - (44757/4096) * δ * cβsβ2 * s23
		    A(79) = (-13125/2048) * δ2 * c3βsβ2 * s25 - (21875/2048) * δ2 * cβsβ2 * s25
		    A(80) = (243/512) * δ1 * cβc2β * c2s2 - (2835/1024) * δ * cβc2β * c2s2 - (5319/4096) * δ1 * cβc2β * c4s2 + (135/8192) * δ * cβc2β * c4s2 + (5967/8192) * δ1 * cβc2β * s2 - (37071/16384) * δ * cβc2β * s2
		    A(81) = (5643/4096) * δ2 * cβc2β * s2 - (18603/8192) * δ * cβc2β * s2 + (2835/2048) * δ * cβc2β * s4 + (135/16384) * δ * cβc2β * s6 - (243/65536) * δ2 * c3β * s10 + (2565/16384) * δ2 * c3β * s8
		    A(82) = (11/768) * δ1 * cβc2β * c2s2 + (133/1536) * δ * cβc2β * c2s2 + (77/2048) * δ1 * cβc2β * c4s2 + (211/4096) * δ * cβc2β * c4s2 + (257/12288) * δ1 * cβc2β * s2 + (319/24576) * δ * cβc2β * s2
		    A(83) = (13/6144) * δ1 * cβc2β * s2 - (11/1536) * δ1 * cβc2β * s4 + (77/4096) * δ1 * cβc2β * s6 - (157/12288) * δ * cβc2β * s2 - (133/3072) * δ * cβc2β * s4 + (211/8192) * δ * cβc2β * s6
		    A(84) = (-429/8192) * δ1 * cβsβ2 * s6 - (1/4096) * δ2 * cβc2βsβ2 * s2 + (7/8192) * δ2 * cβc2βsβ2 * s2 - (21/16384) * δ2 * c3βsβ2 * s10 - (35/16384) * δ2 * cβsβ2 * s10
		    
		    // Order 3/2, SO, plus polarization
		    A(85) = η2 * cβsβ * c23 * χsx
		    A(86) = (1/6) * η10 * c2β * c2c14 * χsz - (7/2) * η * c2β * c14 * χsz - δ0 * c2β * c14 * χsz - (1/3) * η10 * s2β * c2c14 * χsx + (1/2) * η10 * c0β * c2c14 * χsz - (19/6) * η * s2β * c14 * χsx - (5/2) * η * c0β * c14 * χsz + (7/3) * δ0 * s2β * c14 * χsx - 3 * δ0 * c0β * c14 * χsz
		    A(87) = (1/2) * η10 * c0β * c15s1 * χsx + (1/6) * η10 * c2β * c15s1 * χsx
		    A(88) = (1/2) * η10 * c0β * c1s15 * χsx + (1/6) * η10 * c2β * c1s15 * χsx
		    A(89) = (7/12) * η10 * c2β * c2c13s1 * χsx + (79/12) * η * c2β * c13s1 * χsx - (13/6) * δ0 * c2β * c13s1 * χsx - (1/4) * η10 * c0β * c2c13s1 * χsx + (2/3) * η10 * s2β * c2c13s1 * χsz - (17/4) * η * c0β * c13s1 * χsx - 7 * η * s2β * c13s1 * χsz + (3/2) * δ0 * c0β * c13s1 * χsx - 2 * δ0 * s2β * c13s1 * χsz
		    A(90) = (-7/12) * η10 * c2β * c1c2s13 * χsx + (79/12) * η * c2β * c1s13 * χsx - (13/6) * δ0 * c2β * c1s13 * χsx + (1/4) * η10 * c0β * c1c2s13 * χsx + (2/3) * η10 * s2β * c1c2s13 * χsz - (17/4) * η * c0β * c1s13 * χsx + (3/2) * δ0 * c0β * c1s13 * χsx - 7 * η * s2β * c1s13 * χsz - 2 * δ0 * s2β * c1s13 * χsz
		    A(91) = (1/6) * η10 * c2β * c2s14 * χsz + (7/2) * η * c2β * s14 * χsz + δ0 * c2β * s14 * χsz - (1/3) * η10 * s2β * c2s14 * χsx + (1/2) * η10 * c0β * c2s14 * χsz + (19/6) * η * s2β * s14 * χsx - (7/3) * δ0 * s2β * s14 * χsx + (5/2) * η * c0β * s14 * χsz + 3 * δ0 * c0β * s14 * χsz
		    A(92) = (-3/2) * η2 * sβ2 * c2s22 * χsz
		    A(93) = (3/8) * η2 * c0β * s23 * χsx + (1/8) * η2 * c2β * s23 * χsx
		    A(94) = (1/3) * η10 * cβsβ * c2s22 * χsx + (1/2) * η10 * sβ2 * c2s22 * χsz
		    A(95) = (3/4) * η2 * c0β * c2s22 * χsz + (1/4) * η2 * c2β * c2s22 * χsz - (1/2) * η2 * s2β * c2s22 * χsx
		    A(96) = (-11/32) * η2 * c2β * s2 * χsx - (3/8) * η2 * c0β * s23 * χsx - (7/32) * η2 * c2β * s6 * χsx + (1/4) * η2 * s2β * s2 * χsz - (1/4) * η2 * s2β * s6 * χsz
		    A(97) = (15/16) * η2 * c0β * s2 * χsy - (3/16) * η2 * c2β * s2 * χsy + (9/16) * η2 * c0β * c4s2 * χsy - (5/16) * η2 * c2β * c4s2 * χsy
		    A(98) = -η2 * cβsβ * c2s22 * χsy
		    A(99) = (3/8) * η2 * c0β * s23 * χsy + (1/8) * η2 * c2β * s23 * χsy
		    A(100) = (-5/12) * η10 * c2β * c1c2s13 * χsy + (3/4) * η10 * c0β * c1c2s13 * χsy + (1/4) * η10 * c0β * c1s13 * χsy - (31/12) * η * c2β * c1s13 * χsy - (11/6) * δ0 * c2β * c1s13 * χsy
		    A(101) = (-7/3) * δ0 * s2β * s14 * χsy - (5/6) * η * s2β * s14 * χsy - (1/3) * η10 * s2β * c2s14 * χsy
		    A(102) = (1/2) * η10 * c0β * c1s15 * χsy + (1/6) * η10 * c2β * c1s15 * χsy
		    A(103) = (-1/3) * δ0 * cβsβ * s22 * χsy - (11/6) * η * cβsβ * s22 * χsy
		    A(104) = (1/4) * η10 * c0β * c13s1 * χsy - (3/4) * η10 * c0β * c2c13s1 * χsy + (5/12) * η10 * c2β * c2c13s1 * χsy - (31/12) * η * c2β * c13s1 * χsy - (11/6) * δ0 * c2β * c13s1 * χsy
		    A(105) = (7/3) * δ0 * s2β * c14 * χsy + (5/6) * η * s2β * c14 * χsy - (1/3) * η10 * s2β * c2c14 * χsy
		    A(106) = (1/2) * η10 * c0β * c15s1 * χsy + (1/6) * η10 * c2β * c15s1 * χsy
		    A(107) = 2 * δ * cβsβ * c23 * χax
		    A(108) = -3 * δ * c0β * c14 * χaz - δ * c2β * c14 * χaz + 5 * δ * c0β * c2c14 * χaz + (5/3) * δ * c2β * c2c14 * χaz + (7/3) * δ * s2β * c14 * χax - (10/3) * δ * s2β * c2c14 * χax
		    A(109) = 5 * δ * c0β * c15s1 * χax + (5/3) * δ * c2β * c15s1 * χax
		    A(110) = 5 * δ * c0β * c1s15 * χax + (5/3) * δ * c2β * c1s15 * χax
		    A(111) = (35/6) * δ * c2β * c2c13s1 * χax - (13/6) * δ * c2β * c13s1 * χax - (5/2) * δ * c0β * c2c13s1 * χax + (20/3) * δ * s2β * c2c13s1 * χaz + (3/2) * δ * c0β * c13s1 * χax - 2 * δ * s2β * c13s1 * χaz
		    A(112) = (-35/6) * δ * c2β * c1c2s13 * χax - (13/6) * δ * c2β * c1s13 * χax + (5/2) * δ * c0β * c1c2s13 * χax - (20/3) * δ * s2β * c1c2s13 * χaz + (3/2) * δ * c0β * c1s13 * χax - 2 * δ * s2β * c1s13 * χaz
		    A(113) = (5/3) * δ * c2β * c2s14 * χaz + δ * c2β * s14 * χaz - (10/3) * δ * s2β * c2s14 * χax + 5 * δ * c0β * c2s14 * χaz - (7/3) * δ * s2β * s14 * χax + 3 * δ * c0β * s14 * χaz
		    A(114) = -3 * δ * sβ2 * c2s22 * χaz
		    A(115) = (3/4) * δ * c0β * s23 * χax + (1/4) * δ * c2β * s23 * χax
		    A(116) = (10/3) * δ * cβsβ * c2s22 * χax + 5 * δ * sβ2 * c2s22 * χaz
		    A(117) = (3/2) * δ * c0β * c2s22 * χaz + (1/2) * δ * c2β * c2s22 * χaz - δ * s2β * c2s22 * χax
		    A(118) = (-11/16) * δ * c2β * s2 * χax - (7/16) * δ * c2β * s6 * χax - (3/4) * δ * c0β * s23 * χax + (1/2) * δ * s2β * s2 * χaz - (1/2) * δ * s2β * s6 * χaz
		    A(119) = (-5/8) * δ * c2β * c4s2 * χay - (3/8) * δ * c2β * s2 * χay + (9/8) * δ * c0β * c4s2 * χay + (15/8) * δ * c0β * s2 * χay
		    A(120) = -2 * δ * cβsβ * c2s22 * χay
		    A(121) = (3/4) * δ * c0β * s23 * χay + (1/4) * δ * c2β * s23 * χay
		    A(122) = (-25/6) * δ * c2β * c1c2s13 * χay - (11/6) * δ * c2β * c1s13 * χay + (15/2) * δ * c0β * c1c2s13 * χay + (5/2) * δ * c0β * c1s13 * χay
		    A(123) = (-7/3) * δ * s2β * s14 * χay - (10/3) * δ * s2β * c2s14 * χay
		    A(124) = 5 * δ * c0β * c1s15 * χay + (5/3) * δ * c2β * c1s15 * χay
		    A(125) = (-1/3) * δ * cβsβ * s22 * χay
		    A(126) = (25/6) * δ * c2β * c2c13s1 * χay - (11/6) * δ * c2β * c13s1 * χay - (15/2) * δ * c0β * c2c13s1 * χay + (5/2) * δ * c0β * c13s1 * χay
		    A(127) = (7/3) * δ * s2β * c14 * χay - (10/3) * δ * s2β * c2c14 * χay
		    A(128) = 5 * δ * c0β * c15s1 * χay + (5/3) * δ * c2β * c15s1 * χay
		    
		    // Order 3/2, cross polarization
		    A(173) = 8 * π * δ0 * sβ * c1s13
		    A(174) = -4 * π * δ0 * cβ * s14
		    A(175) = -8 * π * δ0 * sβ * c13s1
		    A(176) = -4 * π * δ0 * cβ * c14
		    A(177) = (4375/192) * δ2 * s2β * c14s16 + (4375/128) * δ2 * s4β * c14s16
		    A(178) = (-625/48) * δ2 * c2β * c13s17 - (625/16) * δ2 * c4β * c13s17
		    A(179) = (625/128) * δ2 * s2β * c12s18 - (5625/256) * δ2 * s4β * c12s18
		    A(180) = (-625/24) * δ2 * c2βsβ2 * c1s19 - (625/48) * δ2 * sβ2 * c1s19
		    A(181) = (-625/96) * δ2 * cβ * sβ3 * s110
		    A(182) = (459/128) * δ2 * s2β * c2c12s14 - (2079/256) * δ2 * s4β * c2c12s14 + (567/512) * δ2 * s2β * c4c12s14 + (1701/1024) * δ2 * s4β * c4c12s14 - (4923/512) * δ1 * s2β * c12s14 - (945/1024) * δ2 * s4β * c12s14 + (22203/1024) * δ * s2β * c12s14
		    A(183) = (27/16) * δ2 * c2β * c1c2s15 - (81/128) * δ2 * c2β * c1c4s15 + (1233/128) * δ1 * c2β * c1s15 - (4689/256) * δ * c2β * c1s15 + (27/16) * δ2 * c4β * c1c2s15 + (27/8) * δ2 * c1c2s15 - (243/128) * δ2 * c4β * c1c4s15 + (27/128) * δ2 * c4β * c1s15 + (27/16) * δ2 * c0β * c1s15
		    A(184) = (-621/256) * δ2 * c2βs2β * c2s16 - (2187/1024) * δ2 * c2βs2β * c4s16 - (1377/1024) * δ2 * c2βs2β * s16 + (837/256) * δ2 * s2β * c2s16 + (243/1024) * δ2 * s2β * c4s16 + (4761/1024) * δ1 * s2β * s16 - (11673/2048) * δ * s2β * s16
		    A(185) = (-81/16) * δ2 * c2βsβ2 * c1c2s17 - (27/16) * δ2 * c2βsβ2 * c1s17 - (81/32) * δ2 * sβ2 * c1c2s17 + (81/32) * δ2 * sβ2 * c1s17
		    A(186) = (-81/32) * δ2 * cβsβ3 * c12s18
		    A(187) = (-7/49152) * δ2 * s2β * c10 - (7/32768) * δ2 * s4β * c10 + (11/6144) * δ2 * c2βs2β * c4 - (91/16384) * δ2 * c2βs2β * c6 - (173/24576) * δ2 * cβs3β * c2 + (6031/24576) * δ1 * cβsβ * c2 - (10511/49152) * δ * cβsβ * c2
		    A(188) = (-37/256) * δ1 * c2β * c1c2s13 - (347/512) * δ * c2β * c1c2s13 + (3/128) * δ2 * c2β * c1c4s13 - (1/768) * δ2 * c2β * c1c6s13 - (35/128) * δ1 * c2β * c1s13 - (23/768) * δ * c2β * c1s13
		    A(189) = (13/128) * δ2 * s2β * c2c12s14 + (11/256) * δ2 * s4β * c2c12s14 + (1/512) * δ2 * s2β * c4c12s14 - (9/1024) * δ2 * s4β * c4c12s14 - (29/512) * δ1 * s2β * c12s14 + (5/1024) * δ2 * s4β * c12s14 - (355/1024) * δ * s2β * c12s14
		    A(190) = (-1/24) * δ2 * c2βsβ2 * c2c13s15 + (1/24) * δ2 * c2βsβ2 * c13s15 - (1/48) * δ2 * sβ2 * c2c13s15 + (7/48) * δ2 * sβ2 * c13s15
		    A(191) = (-1/48) * δ2 * cβsβ3 * c14s16
		    A(192) = (-1/128) * δ2 * c2βsβ2 * s4 - (7/256) * δ2 * c2βsβ2 * s8 + (45/128) * δ1 * sβ2 * s4 - (5/256) * δ2 * sβ2 * s8 - (77/256) * δ * sβ2 * s4
		    A(193) = (-189/32) * δ2 * c2βsβ2 * c2s23 - (135/32) * δ2 * sβ2 * c2s23
		    A(194) = (-7/49152) * δ2 * s2β * c10 - (7/32768) * δ2 * s4β * c10 - (11/6144) * δ2 * c2βs2β * c4 - (91/16384) * δ2 * c2βs2β * c6 - (173/24576) * δ2 * cβs3β * c2 + (6031/24576) * δ1 * cβsβ * c2 - (10511/49152) * δ * cβsβ * c2
		    A(195) = (37/256) * δ1 * c2β * c2c13s1 + (347/512) * δ * c2β * c2c13s1 + (3/128) * δ2 * c2β * c4c13s1 + (1/768) * δ2 * c2β * c6c13s1 - (35/128) * δ1 * c2β * c13s1 - (23/768) * δ * c2β * c13s1
		    A(196) = (13/128) * δ2 * s2β * c2c14s12 + (11/246) * δ2 * s4β * c2c14s12 - (1/512) * δ2 * s2β * c4c14s12 + (9/1024) * δ2 * s4β * c4c14s12 - (5/1024) * δ2 * s4β * c14s12 + (355/1024) * δ * s2β * c14s12 + (29/512) * δ1 * s2β * c14s12
		    A(197) = (7/48) * δ2 * sβ2 * c15s12 + (1/24) * δ2 * c2βsβ2 * c15s12 + (1/48) * δ2 * sβ2 * c2c15s12 + (1/24) * δ2 * c2βsβ2 * c2c15s12
		    A(198) = (1/48) * δ2 * cβsβ3 * c16s14
		    A(199) = (459/128) * δ2 * s2β * c2c14s12 - (2079/512) * δ2 * s4β * c2c14s12 - (567/512) * δ2 * s2β * c4c14s12 - (1701/1024) * δ2 * s4β * c4c14s12 + (945/1024) * δ2 * s4β * c14s12 + (4923/512) * δ1 * s2β * c14s12 - (22203/1024) * δ * s2β * c14s12
		    A(200) = (-27/16) * δ2 * c2β * c2c15s1 - (81/128) * δ2 * c2β * c4c15s1 - (27/16) * δ2 * c4β * c2c15s1 - (27/8) * δ2 * c0β * c2c15s1 - (243/128) * δ2 * c4β * c4c15s1 + (27/128) * δ2 * c4β * c15s1 + (27/16) * δ2 * c0β * c15s1 + (1233/128) * δ1 * c2β * c15s1 - (4689/256) * δ * c2β * c15s1
		    A(201) = (837/256) * δ2 * s2β * c2c16 - (621/512) * δ2 * s4β * c2c16 - (243/1024) * δ2 * s2β * c4c16 + (2187/2048) * δ2 * s4β * c4c16 + (1377/2048) * δ2 * s4β * c16 - (4761/1024) * δ1 * s2β * c16 + (11673/2048) * δ * s2β * c16
		    A(202) = (81/16) * δ2 * c2βsβ2 * c2c17s1 - (27/16) * δ2 * c2βsβ2 * c17s1 + (81/32) * δ2 * sβ2 * c2c17s1 + (81/32) * δ2 * sβ2 * c17s1
		    A(203) = (81/32) * δ2 * cβsβ3 * c18s12
		    A(204) = (-4375/192) * δ2 * s2β * c16s14 - (4375/128) * δ2 * s4β * c16s14
		    A(205) = (-625/48) * δ2 * c2β * c17s13 - (625/16) * δ2 * c4β * c17s13
		    A(206) = (-625/128) * δ2 * s2β * c18s12 + (5625/256) * δ2 * s4β * c18s12
		    A(207) = (-625/24) * δ2 * c2βsβ2 * c19s1 - (625/48) * δ2 * sβ2 * c19s1
		    A(208) = (625/96) * δ2 * cβsβ3 * c110
		    
		    // Order 3/2, SO, cross polarization
		    A(209) = η2 * sβ * c23 * χsy
		    A(210) = (2/3) * η10 * sβ * c2c14 * χsy - (5/3) * η * sβ * c14 * χsy - (14/3) * δ0 * sβ * c14 * χsy
		    A(211) = (-2/3) * η10 * cβ * c15s1 * χsy
		    A(212) = (1/3) * η10 * cβ * c2c13s1 * χsy + (7/3) * η * cβ * c13s1 * χsy - (2/3) * δ0 * cβ * c13s1 * χsy
		    A(213) = (-1/3) * η10 * cβ * c1c2s13 * χsy + (7/3) * η * cβ * c1s13 * χsy - (2/3) * δ0 * cβ * c1s13 * χsy
		    A(214) = (2/3) * η10 * sβ * c2s14 * χsy + (5/3) * η * sβ * s14 * χsy + (14/3) * δ0 * sβ * s14 * χsy
		    A(215) = (-2/3) * η10 * cβ * c1s15 * χsy
		    A(216) = η2 * sβ * c2s22 * χsy
		    A(217) = (1/3) * η10 * sβ * c2s22 * χsy
		    A(218) = (-1/2) * η2 * cβ * s23 * χsy
		    A(219) = (-5/8) * η2 * cβ * s2 * χsy - (1/8) * η2 * cβ * s6 * χsy
		    A(220) = (-3/4) * η2 * cβ * s2 * χsx - (1/4) * η2 * cβ * c4s2 * χsx - η2 * sβ * c4s2 * χsz
		    A(221) = η2 * cβ * c2s22 * χsz - η2 * sβ * c2s22 * χsx
		    A(222) = 2 * η2 * cβ * s23 * χsx
		    A(223) = (-2/3) * δ0 * cβ * c1s13 * χsx - (10/3) * δ0 * cβ * c1c2s13 * χsx - (5/3) * η * cβ * c1s13 * χsx + 4 * η * c3β * c1s13 * χsx - (1/3) * δ0 * cβ * c1c2s13 * χsx
		    A(224) = 4 * δ0 * cβ * s14 * χsz + (20/3) * δ0 * cβ * c2s14 * χsz + (5) * η * cβ * s14 * χsz + 4 * η * c3β * s14 * χsz + (2/3) * δ0 * cβ * c2s14 * χsz
		    A(225) = (2/3) * η10 * cβ * c1s15 * χsx
		    A(226) = -6 * η * cβsβ2 * s22 * χsz + (1/3) * δ0 * sβ * s22 * χsx - (7/6) * η * sβ * s22 * χsx + 3 * η * c2βsβ * s22 * χsx
		    A(227) = (-5/3) * η * cβ * c13s1 * χsx + 4 * η * c3β * c13s1 * χsx + (1/3) * η * cβ * c2c13s1 * χsx - (2/3) * δ0 * cβ * c13s1 * χsx
		    A(228) = (-5) * η * cβ * c14 * χsz - η * c3β * c14 * χsz + (2/3) * η * cβ * c2c14 * χsz - 4 * δ0 * cβ * c14 * χsz
		    A(229) = (2/3) * η10 * cβ * c15s1 * χsx
		    A(230) = 2 * δ * sβ * c23 * χay
		    A(231) = (-14/3) * δ * sβ * c14 * χay + (20/3) * δ * sβ * c2c14 * χay
		    A(232) = (-2/3) * δ * cβ * c13s1 * χay + (10/3) * δ * cβ * c2c13s1 * χay
		    A(233) = (-20/3) * δ * cβ * c15s1 * χay
		    A(234) = (-10/3) * δ * cβ * c1c2s13 * χay - (2/3) * δ * cβ * c1s13 * χay
		    A(235) = (20/3) * δ * sβ * c2s14 * χay + (14/3) * δ * sβ * s14 * χay
		    A(236) = (-20/3) * δ * cβ * c1s15 * χay
		    A(237) = 2 * δ * sβ * c2s22 * χay
		    A(238) = (10/3) * δ * sβ * c2s22 * χay
		    A(239) = -δ * cβ * s13 * χay
		    A(240) = (-5/4) * δ * cβ * s2 * χay - (1/4) * δ * cβ * s6 * χay
		    A(241) = (-1/2) * δ * cβ * c4s2 * χax - 2 * δ * sβ * c4s2 * χaz - (3/2) * δ * cβ * s2 * χax
		    A(242) = 2 * δ * cβ * c2s22 * χaz - 2 * δ * sβ * c2s22 * χax
		    A(243) = δ * cβ * s23 * χax
		    A(244) = (-10/3) * δ * cβ * c1c2s13 * χax - (40/3) * δ * sβ * c1c2s13 * χaz - (2/3) * δ * cβ * c1s13 * χax - 4 * δ * sβ * c1s13 * χaz
		    A(245) = (20/3) * δ * cβ * c2s14 * χaz - (20/3) * δ * sβ * c2s14 * χax + 4 * δ * cβ * s14 * χaz - (14/3) * δ * sβ * s14 * χax
		    A(246) = (20/3) * δ * cβ * c1s15 * χax
		    A(247) = (1/3) * δ * sβ * s22 * χax
		    A(248) = (10/3) * δ * cβ * c2c13s1 * χax - (2/3) * δ * cβ * c13s1 * χax + (40/3) * δ * sβ * c2c13s1 * χaz - 4 * δ * sβ * c13s1 * χaz
		    A(249) = (20/3) * δ * cβ * c2c14 * χaz - 4 * δ * cβ * c14 * χaz + (14/3) * δ * sβ * c14 * χax - (20/3) * δ * sβ * c2c14 * χax
		    A(250) = (20/3) * δ * cβ * c15s1 * χax
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub CalculateWaveFactors()
		  // Calculate the received wave factors at the current time as
		  // well as the derivatives of those wave factors
		  
		  Var wfs(1,5,1,5) As Double
		  Var dwfdα(1,5,1,5) As Double
		  Var dwfdΨ(1,5,1,5) As Double
		  Var cos As Integer = 0
		  Var sin As Integer = 1
		  Var plus As Integer = 0
		  Var minus As Integer = 1
		  
		  // set up basic frequencies for Ψ
		  Var c1 As Double = Cos(ΨrDN)
		  Var s1 As Double = Sin(ΨrDN)
		  Var c2 As Double = c1*c1 - s1*s1
		  Var s2 As Double = 2.0*c1*s1
		  Var c3 As Double = c2*c1 - s2*s1
		  Var s3 As Double = s2*c1 + c2*s1
		  Var c4 As Double = c3*c1 - s3*s1
		  Var s4 As Double = s3*c1 + c3*s1
		  Var c5 As Double = c4*c1 - s4*s1
		  Var s5 As Double = s4*c1 + c4*s1
		  
		  // set up factors for the received phase only
		  wfs(cos,0,plus,0) = 1.0
		  wfs(sin,0,plus,0) = 0.0
		  wfs(cos,0,minus,0) = 1.0
		  wfs(sin,0,minus,0) = 0.0
		  wfs(cos,0,plus,1) = c1
		  wfs(sin,0,plus,1) = s1
		  wfs(cos,0,plus,2) = c2
		  wfs(sin,0,plus,2) = s2
		  wfs(cos,0,plus,3) = c3
		  wfs(sin,0,plus,3) = s3
		  wfs(cos,0,plus,4) = c4
		  wfs(sin,0,plus,4) = s4
		  wfs(cos,0,plus,5) = c5
		  wfs(sin,0,plus,5) = s5
		  
		  c1 = Cos(SpinResults.α)
		  s1 = Sin(SpinResults.α)
		  c2 = c1*c1 - s1*s1
		  s2 = 2.0*c1*s1
		  c3 = c2*c1 - s2*s1
		  s3 = s2*c1 + c2*s1
		  c4 = c3*c1 - s3*s1
		  s4 = s3*c1 + c3*s1
		  c5 = c4*c1 - s4*s1
		  s5 = s5*c1 + c5*s1
		  
		  // set up factors for alpha alone
		  wfs(cos,1,plus,0) = c1
		  wfs(sin,1,plus,0) = s1
		  wfs(cos,2,plus,0) = c2
		  wfs(sin,2,plus,0) = s2
		  wfs(cos,3,plus,0) = c3
		  wfs(sin,3,plus,0) = s3
		  wfs(cos,4,plus,0) = c4
		  wfs(sin,4,plus,0) = s4
		  wfs(cos,5,plus,0) = c5
		  wfs(sin,5,plus,0) = s5
		  
		  // Now basically calculate all possible combinations
		  For k As Integer = 1 to 5
		    for j as Integer = 1 to 5
		      wfs(cos,j,plus,k) = wfs(cos,j,plus,0)*wfs(cos,0,plus,k) - wfs(sin,j,plus,0)*wfs(sin,0,plus,k)
		      wfs(cos,j,minus,k) = wfs(cos,j,plus,0)*wfs(cos,0,plus,k) + wfs(sin,j,plus,0)*wfs(sin,0,plus,k)
		      wfs(sin,j,plus,k) = wfs(sin,j,plus,0)*wfs(cos,0,plus,k) + wfs(cos,j,plus,0)*wfs(sin,0,plus,k)
		      wfs(sin,j,minus,k) = wfs(sin,j,plus,0)*wfs(cos,0,plus,k) - wfs(cos,j,plus,0)*wfs(sin,0,plus,k)
		    Next
		    wfs(cos,0,minus,k) = wfs(cos,0,plus,k)
		    wfs(sin,0,minus,k) = -wfs(sin,0,plus,k)
		  Next
		  
		  // Now calculate the derivatives of all these factors
		  For k As Integer = 0 to 5
		    For j As Integer = 0 to 5
		      dwfdα(cos,j,plus,k) = -j*wfs(sin,j,plus,k)
		      dwfdα(sin,j,plus,k) = j*wfs(cos,j,plus,k)
		      dwfdα(cos,j,minus,k) = -j*wfs(sin,j,minus,k)
		      dwfdα(sin,j,minus,k) = j*wfs(cos,j,minus,k)
		      
		      dwfdΨ(cos,j,plus,k) = -k*wfs(sin,j,plus,k)
		      dwfdΨ(sin,j,plus,k) = k*wfs(cos,j,plus,k)
		      dwfdΨ(cos,j,minus,k) = k*wfs(sin,j,minus,k)
		      dwfdΨ(sin,j,minus,k) = -k*wfs(cos,j,minus,k)
		    Next
		  Next
		  
		  // finally, populate the W, DWDα, and DWDΨ arrays with the appropriate values from the wave table
		  AssignWaveFactors(wfs,W)
		  AssignWaveFactors(dwfdα,DWDα)
		  AssignWaveFactors(dwfdΨ,DWDΨ)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(P As CaseInfoClass)
		  // Initialize constants
		  Parameters = P
		  VeSinΘ = P.Ve*Sin(P.Θ)
		  VeCosΘ = P.Ve*Cos(P.Θ)
		  Δτr = P.ΔT/P.GM
		  Δτ = Δτr/(1.0 + P.Z)
		  H0 = P.H0
		  
		  // Initialize the SpinEvolver class
		  SpinEvolver = New SpinEvolverClass(P)
		  
		  // Set up classes for calculating function values
		  δFunctions = New DeltaFuncsClass
		  δFunctions.SetValues(P.δ)
		  
		  βFunctions = New BetaFuncsClass
		  βFunctions.SetValues(P.β)
		  
		  ιFunctions = New IotaFuncsClass
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DidDetectorStepOK(StepNumber As Integer) As Boolean
		  // If we are within two detector steps of coalescence, bail out
		  If (StepNumber + 2)*Δτ > Parameters.τc Then Return False
		  
		  // Otherwise, check if we can get data from the spin evolver
		  τrDN = StepNumber*Δτr
		  SpinResults = SpinEvolver.GetSpinDataAtTime(τrDN)
		  If SpinResults = Nil Then Return False  // If the method returns nothing, coalescence must have happened
		  
		  // Set up functions that depend on Iota
		  ιFunctions.SetValues(SpinResults.ι)
		  
		  // Calculate the wave phase
		  ΨrDN = SpinResults.Ψ
		  // do the following instead of the above if we want the data in the orbiting LISA frame
		  // ΨrDN = ΨrDP + (1.0 + Parameters.Ve*Sin(Parameters.Θ)*Sin(Parameters.GMΩe*τrm - Parameters.Φ))*(spinData.Ψ - ΨP)
		  // ΨrDP = ΨrDN
		  // ΨP = spinData.Ψ
		  
		  // Retrieve the current value of α
		  αDN = SpinResults.α
		  
		  // Calculate the amplitudes
		  CalculateAmplitudes
		  
		  // Calculate polarization factors
		  Var fpfx() As Double = Parameters.Detector.GetFPAndFX(τrDN)
		  FP = fpfx(0)
		  FX = fpfx(1)
		  
		  // Calculate W, DWDα, and DWDΨ factors for the base case only
		  If Parameters.IsBaseCase Then CalculateWaveFactors
		  
		  // We have completed the detector step successfully
		  Return True
		  
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		A(LastWaveTermIndex) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DWDα(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DWDΨ(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FX As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		H0 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Parameters As CaseInfoClass
	#tag EndProperty

	#tag Property, Flags = &h21
		Private SpinEvolver As SpinEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SpinResults As SpinResultsClass
	#tag EndProperty

	#tag Property, Flags = &h21
		Private VeCosΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private VeSinΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		W(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private ιFunctions As IotaFuncsClass
	#tag EndProperty

	#tag Property, Flags = &h0
		αDN As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private βFunctions As BetaFuncsClass
	#tag EndProperty

	#tag Property, Flags = &h21
		Private δFunctions As DeltaFuncsClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Δτ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Δτr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		τrDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrDN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrDP As Double
	#tag EndProperty


	#tag Constant, Name = Cross, Type = Boolean, Dynamic = False, Default = \"True", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Plus, Type = Boolean, Dynamic = False, Default = \"False", Scope = Public
	#tag EndConstant


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
			Name="ΨrDN"
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
			Name="αDN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FX"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
