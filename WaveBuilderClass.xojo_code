#tag Class
Protected Class WaveBuilderClass
	#tag Method, Flags = &h21
		Private Sub AssembleDerivatives()
		  // These static constants help us build the detector functions
		  // (Static variables are only calculated once the first time this method is
		  // called and retain their values during future calls.)
		  
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
		  
		  // These static variables help with the calculation of the wave amplitude
		  // and its derivatives
		  Static η As Double = 0.25*(1.0-Parameters.δ*Parameters.δ)
		  h0 = 2.0*Parameters.M*η/Parameters.R
		  Static dh0dlnM As Double = h0
		  dh0dδ = -0.5*h0*Parameters.δ/η
		  Static dh0dlnR As Double = -h0
		  
		  // These static variables indicate which polarization we are calculating
		  // or whether we are calculating a derivative with respect to v.
		  Static cross As Boolean = True
		  Static plus As Boolean = False
		  Static vderiv As Boolean = True
		  
		  // Local variables to hold cross and plus polarizations
		  Var hp As Double
		  Var hx As Double
		  
		  // Calculate amplitude and derivatives of the amplitude
		  CalculateAmplitudes(A)
		  CalculateAmplitudes(DADδ)
		  CalculateAmplitudes(DADβ)
		  CalculateAmplitudes(DADι)
		  CalculateAmplitudes(DADχax)
		  CalculateAmplitudes(DADχay)
		  CalculateAmplitudes(DADχaz)
		  CalculateAmplitudes(DADχsx)
		  CalculateAmplitudes(DADχsy)
		  CalculateAmplitudes(DADχsz)
		  
		  // Calculate the wave arrays W, DWDΨ, and DWDα
		  CalculateWaveFactors
		  
		  // These variables project the particular wave onto the detector.
		  // These default values assume an L-shaped detector perpendicular
		  // to the line of sight. This will be selected if Parameters.Detectors = 0.
		  // To get the plus polarization in the source frame, choose ψ = 0.
		  // To get the cross polarization, choose ψ = π/4.
		  fp = cos2ψ
		  fx = sin2ψ
		  
		  // Otherwise, we will calculate these factors for the LISA detector,
		  // which requires a more complicated calculation
		  If Parameters.Detectors > 0 Then
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
		    
		    Var dp As Double = dpc1*(-6.0*s210 + 9.0*s012 - s412) + dpc2*c2Θ*(18.0*s210 + 9.0*s012 - s412) _
		    - dpc3*s2Θ*(s311 - 3.0*s111)
		    Var dx As Double = dxc1*cΘ*(9.0*c012 - c412) - dxc2*sΘ*(s311 - 3.0*s111)
		    Var ddpdΘ As Double = -2.0*dpc2*s2Θ*(18.0*s210 + 9.0*s012 - s412) - 2.0*dpc3*c2Θ*(s311 - 3.0*s111)
		    Var ddxdΘ As Double = -dxc1*sΘ*(9.0*c012 - c412) - dxc2*cΘ*(s311 - 3.0*s111)
		    Var ddpdΦ As Double = dpc1*(-18.0*c012 + 2.0*c412) + dpc2*c2Θ*(-18.0*c012 + 2.0*c412) _
		    + dpc3*s2Θ*(c311 - 3.0*c111)
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
		      
		      dp = dpc1*(-6.0*s210 + 9.0*s012 - s412) + dpc2*c2Θ*(18.0*s210 + 9.0*s012 - s412) _
		      - dpc3*s2Θ*(s311 - 3.0*s111)
		      dx = dxc1*cΘ*(9.0*c012 - c412) - dxc2*sΘ*(s311 - 3.0*s111)
		      ddpdΘ = -2.0*dpc2*s2Θ*(18.0*s210 + 9.0*s012 - s412) - 2.0*dpc3*c2Θ*(s311 - 3.0*s111)
		      ddxdΘ = -dxc1*sΘ*(9.0*c012 - c412) - dxc2*cΘ*(s311 - 3.0*s111)
		      ddpdΦ = dpc1*(-18.0*c012 + 2.0*c412) + dpc2*c2Θ*(-18.0*c012 + 2.0*c412) _
		      + dpc3*s2Θ*(c311 - 3.0*c111)
		      ddxdΦ = dxc1*cΘ*(18.0*s012 - 2.0*s412) - dxc2*sΘ*(c311 + 3.0*c111)
		      fp2 = cos2ψ*dp - sin2ψ*dx
		      fx2 = sin2ψ*dp + cos2ψ*dx
		      dfp2dΘ = cos2ψ*ddpdΘ - sin2ψ*ddxdΘ
		      dfx2dΘ = sin2ψ*ddpdΘ + cos2ψ*ddxdΘ
		      dfp2dΦ = cos2ψ*ddpdΦ - sin2ψ*ddxdΦ
		      dfx2dΦ = sin2ψ*ddpdΦ + cos2ψ*ddxdΦ
		    End If
		    fp = fp1 + fp2
		    fx = fx1 + fx2
		  End If
		  
		  // Get the wave itself
		  hp = GetHSum(A,W,plus)
		  hx = GetHSum(A,W,cross)
		  H = h0*(fp*hp + fx*hx)
		  
		  // Now let's do the β-derivative, which is the easiest
		  hp = GetHSum(DADβ,W,plus)
		  hx = GetHSum(DADβ,W,cross)
		  DHDq(dβ) = h0*(fp*hp + fx*hx)
		  
		  // Calculate the δ derivative, which is the worst
		  Var dhp As Double = GetHSum(DADδ,W,plus) _
		  + GetHSum(DADι,W,plus)*SpinResults.DιI(Dδ) _
		  + GetHSum(DADχax,W,plus)*SpinResults.DχaxI(Dδ) _
		  + GetHSum(DADχay,W,plus)*SpinResults.DχayI(Dδ) _
		  + GetHSum(DADχaz,W,plus)*SpinResults.DχazI(Dδ) _
		  + GetHSum(DADχsx,W,plus)*SpinResults.DχsxI(Dδ) _
		  + GetHSum(DADχsy,W,plus)*SpinResults.DχsyI(Dδ) _
		  + GetHSum(DADχsz,W,plus)*SpinResults.DχszI(Dδ) _
		  + GetHSum(A,DWDα,plus)*SpinResults.DαI(Dδ) _
		  + GetHSum(A,DWDΨ,plus)*SpinResults.DΨI(Dδ) _
		  + GetHSum(A,W,plus,vderiv)*SpinResults.DVI(Dδ)
		  Var dhx As Double = GetHSum(DADδ,W,cross) _
		  + GetHSum(DADι,W,cross)*SpinResults.DιI(Dδ) _
		  + GetHSum(DADχax,W,cross)*SpinResults.DχaxI(Dδ) _
		  + GetHSum(DADχay,W,cross)*SpinResults.DχayI(Dδ) _
		  + GetHSum(DADχaz,W,cross)*SpinResults.DχazI(Dδ) _
		  + GetHSum(DADχsx,W,cross)*SpinResults.DχsxI(Dδ) _
		  + GetHSum(DADχsy,W,cross)*SpinResults.DχsyI(Dδ) _
		  + GetHSum(DADχsz,W,cross)*SpinResults.DχszI(Dδ) _
		  + GetHSum(A,DWDα,cross)*SpinResults.DαI(Dδ) _
		  + GetHSum(A,DWDΨ,cross)*SpinResults.DΨI(Dδ) _
		  + GetHSum(A,W,cross,vderiv)*SpinResults.DVI(Dδ)
		  DHDq(Dδ) = h0*(fp*dhp+fx*dhx) + dh0dδ*(fp*hp+fx*hx)
		  
		  
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
		Private Sub CalculateAmplitudes(theA() As Double)
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
		  Var χDeriv as Boolean = False   // Flag indicating we are calculating a spin derivative
		  Var PNOrder As Integer = Parameters.PNOrder
		  
		  // Now decide on the basis of the parameters whether we are doing a derivative
		  // and if so, what derivative
		  If theA = DADδ Then
		    δfs = δFunDerivs
		  ElseIf theA = DADβ Then
		    βfs = βFunDerivs
		  Elseif theA = DADι Then
		    ιfs = ιFunDerivs
		  ElseIf theA = DADχax Then
		    χax = 1.0
		    χay = 0.0
		    χaz = 0.0
		    χsx = 0.0
		    χsy = 0.0
		    χsz = 0.0
		    χDeriv = True
		  Elseif theA = DADχay Then
		    χax = 0.0
		    χay = 1.0
		    χaz = 0.0
		    χsx = 0.0
		    χsy = 0.0
		    χsz = 0.0
		    χDeriv = True
		  Elseif theA = DADχaz Then
		    χax = 0.0
		    χay = 0.0
		    χaz = 1.0
		    χsx = 0.0
		    χsy = 0.0
		    χsz = 0.0
		    χDeriv = True
		  Elseif theA = DADχsx Then
		    χax = 0.0
		    χay = 0.0
		    χaz = 0.0
		    χsx = 1.0
		    χsy = 0.0
		    χsz = 0.0
		    χDeriv = True
		  Elseif theA = DADχsy Then
		    χax = 0.0
		    χay = 0.0
		    χaz = 0.0
		    χsx = 0.0
		    χsy = 1.0
		    χsz = 0.0
		    χDeriv = True
		  Elseif theA = DADχsz Then
		    χax = 0.0
		    χay = 0.0
		    χaz = 0.0
		    χsx = 0.0
		    χsy = 0.0
		    χsz = 1.0
		    χDeriv = True
		  End If
		  
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
		    theA(i) = 0.0
		  Next
		  
		  // Calculate the non-spin terms
		  // (but only if we are not doing a spin derivative)
		  If Not χDeriv Then
		    
		    // Order 0, plus polarization
		    theA(0) = (-3/2) * δ0 * c0β * c14 - (1/2) * δ0 * c2β * c14
		    theA(1) = -2 * δ0 * s2β * c13s1
		    theA(2) = 2 * δ0 * s2β * c1s13
		    theA(3) = (-3/2) * δ0 * c0β * s14 - (1/2) * δ0 * c2β * s14
		    theA(4) = (-3/2) * δ0 * sβ2 * s22
		    
		    // Order 0, cross polarization
		    theA(129) = 4 * δ0 * sβ * c1s13
		    theA(130) = -2 * δ0 * cβ * s14
		    theA(131) = -4 * δ0 * sβ * c13s1
		    theA(132) = -2 * δ0 * cβ * c14
		    
		    If PNOrder > 0 Then
		      // Amplitude factors for H1P
		      theA(5) = (-45/32) * δ * sβ * c16 - (9/32) * δ * s3β * c16
		      theA(6) = (-175/256) * δ * sβ * c12 + (87/64) * δ * sβ * c2c12 - (5/64) * δ * s3β * c2c12 - (5/256) * δ * sβ * c4c12 + (15/256) * δ * s3β * c4c12 + (13/256) * δ * s3β * c12
		      theA(7) = (175/256) * δ * sβ * s12 + (87/64) * δ * sβ * c2s12 - (5/64) * δ * s3β * c2s12 + (5/256) * δ * sβ * c4s12 - (15/256) * δ * s3β * c4s12 - (13/256) * δ * s3β * s12
		      theA(8) = (-5/32) * δ * sβ * c14s12 - (1/32) * δ * s3β * c14s12
		      theA(9) = (-45/32) * δ * sβ * c14s12 + (135/32) * δ * s3β * c14s12
		      theA(10) = (45/32) * δ * sβ * c12s14 - (135/32) * δ * s3β * c12s14
		      theA(11) = (5/32) * δ * sβ * c12s14 + (1/32) * δ * s3β * c12s14
		      theA(12) = (27/16) * δ * sβ * s16 + (9/16) * δ * c2βsβ * s16
		      theA(13) = (45/16) * δ * cβsβ2 * s23
		      theA(14) = (-85/256) * δ * cβ * s2 - (1/128) * δ * cβc2β * s2 - (1/32) * δ * cβc2β * c2s2 - (3/128) * δ * cβc2β * c4s2 - (11/64) * δ * cβ * s4 - (1/256) * δ * cβ * s6
		      theA(15) = (45/256) * δ * cβ * s2 + (81/128) * δ * cβc2β * s2 + (27/32) * δ * cβc2β * c2s2 + (27/128) * δ * cβc2β * c4s2 + (9/64) * δ * cβ * s4 + (9/256) * δ * cβ * s6
		      theA(16) = (1/256) * δ * cβc2β * s2 - (85/256) * δ * cβ * s2 + (11/64) * δ * cβ * s4 + (1/64) * δ * cβc2β * s4 - (1/256) * δ * cβ * s6 + (3/256) * δ * cβc2β * s6
		      theA(17) = (45/256) * δ * cβ * s2 + (135/256) * δ * cβc2β * s2 - (9/64) * δ * cβ * s4 - (27/64) * δ * cβc2β * s4 + (9/256) * δ * cβ * s6 + (27/256) * δ * cβc2β * s6
		      theA(18) = (1/64) * δ * cβsβ2 * s2 + (5/64) * δ * cβsβ2 * s6
		      
		      // Order 1/2, Cross polarization
		      theA(136) = (-1/64) * δ * cβsβ * c0 + (43/128) * δ * cβsβ * c2 - (23/128) * δ * s2β * c4 + (5/256) * δ * s2β * c6
		      theA(137) = (1/4) * δ * c2β * c1c2s13 - (1/4) * δ * c2β * c1s13 - δ * c1s13
		      theA(138) = (1/8) * δ * s2β * c12s14
		      theA(139) = (1/2) * δ * sβ2 * s4
		      theA(140) = (1/64) * δ * cβsβ * c0 + (43/128) * δ * cβsβ * c2 + (23/128) * δ * s2β * c4 + (5/256) * δ * s2β * c6
		      theA(141) = (1/4) * δ * c2β * c2c13s1 - (1/4) * δ * c2β * c13s1 - δ * c0β * c13s1
		      theA(142) = (-1/8) * δ * s2β * c14s12
		      theA(143) = (45/8) * δ * s2β * c14s12
		      theA(144) = (9/2) * δ * c2β * c15s1
		      theA(145) = (-9/8) * δ * s2β * c16
		      
		    End If
		    If PNOrder > 1 Then
		      // Order 2/2, Plus polarization
		      theA(19) = (59/16) * δ0 * c0β * c14 + (5/2) * δ0 * c2β * c14 - (25/16) * η * c0β * c14 - (13/3) * η * c2β * c14 + (9/16) * η3 * c4β * c14 - (5/8) * η3 * c0β * c2c14 + (11/2) * η3 * c2β * c2c14 - (7/8) * η3 * c4β * c2c14 + (5/16) * η3 * c0β * c4c14 + (1/4) * η3 * c2β * c4c14 + (7/16) * η3 * c4β * c4c14
		      theA(20) = 6 * η3 * sβ2 * c0β * c18 + 2 * η3 * c2βsβ2 * c18
		      theA(21) = -32 * η3 * cβ3sβ * c17s1
		      theA(22) = (5/2) * η3 * s2β * c15s1 - (1/2) * η3 * c2βs2β * c15s1 + 2 * η3 * cβ2s2β * c2c15s1
		      theA(23) = 10 * η3 * c0β * c16s12 + 8 * η3 * c2β * c16s12 + 14 * η3 * c4β * c16s12
		      theA(24) = (3/2) * η3 * sβ2 * c16s12 + (1/2) * η3 * c2βsβ2 * c16s12
		      theA(25) = 56 * η * c2βs2β * c15s13 - 8 * η * s2β * c15s13
		      theA(26) = (16/3) * η * s2β * c1s13 + (31/4) * η3 * s2β * c1c2s13 + (1/4) * η3 * s2β * c1c4s13 - (19/16) * η3 * s4β * c1s13 - (7/8) * η3 * s4β * c3s13 - (7/16) * η3 * s4β * c5s13 - 6 * s2β * c1s13
		      theA(27) = (59/16) * δ0 * c0β * s14 + (5/2) * δ0 * c2β * s14 - (25/16) * η * c0β * s14 - (13/3) * η * c2β * s14 + (9/16) * η3 * c4β * s14 + (5/8) * η3 * c0β * c2s14 - (11/2) * η3 * c2β * c2s14 + (7/8) * η3 * c4β * c2s14 + (5/16) * η3 * c0β * c4s14 + (1/4) * η3 * c2β * c4s14 + (7/16) * η3 * c4β * c4s14
		      theA(28) = 8 * η3 * s2β * c13s15 - 56 * η3 * c2βs2β * c13s15
		      theA(29) = (-5/2) * η3 * s2β * c1s15 + (1/2) * η3 * c2βs2β * c1s15 + 2 * η3 * cβ2s2β * c1c2s15
		      theA(30) = 10 * η3 * c0β * c12s16 + 8 * η3 * c2β * c12s16 + 14 * η3 * c4β * c12s16
		      theA(31) = (3/2) * η3 * sβ2 * c12s16 + (1/2) * η3 * c2βsβ2 * c12s16
		      theA(32) = 32 * η3 * cβ3sβ * c1s17
		      theA(33) = 6 * η3 * sβ2 * s18 + 2 * η3 * c2βsβ2 * s18
		      theA(34) = (349/96) * δ0 * sβ2 * s22 + (25/32) * η3 * c2βsβ2 * s22 - (45/32) * η * sβ2 * s22 + (25/32) * η3 * sβ2 * c4s22 + (35/32) * η3 * c2βsβ2 * c4s22
		      theA(35) = (25/4) * η3 * sβ2 * s24 + (35/4) * η3 * c2βsβ2 * s24
		      theA(36) = 6 * δ0 * s2β * c13s1 - (16/3) * η * s2β * c13s1 + (31/4) * η3 * s2β * c2c13s1 - (1/4) * η3 * s2β * c4c13s1 + (19/16) * η3 * s4β * c13s1 - (7/8) * η3 * s4β * c13s3 + (7/16) * η3 * s4β * c13s5
		      
		      //Order 2/2, Cross polarization
		      theA(146) = -28 * η3 * s3β * c13s15 - 12 * η3 * sβ * c13s15
		      theA(147) = -4 * η3 * c3β * c12s16 + 28 * η3 * cβ * c12s16
		      theA(148) = 12 * η3 * s3β * c1s17 - 4 * η3 * sβ * c1s17
		      theA(149) = 8 * η3 * cβsβ * s18
		      theA(150) = (19/4) * η3 * s3β * c1c2s13 - (9/4) * η3 * sβ * c1c2s13 - (7/8) * η3 * s3β * c1c4s13 - (3/8) * η3 * sβ * c1c4s13 + (9/8) * η3 * s3β * c1s13 + (103/24) * η * sβ * c1s13 - (79/8) * δ0 * sβ * c1s13
		      theA(151) = (-1/2) * η3 * c3β * c2s14 - (7/2) * η3 * cβ * c2s14 + (7/8) * η3 * c3β * c4s14 - (3/8) * η3 * c3β * s14 + (1/8) * η3 * cβ * c4s14 - (119/24) * η * cβ * s14 + (47/8) * δ0 * cβ * s14
		      theA(152) = 3 * η3 * c2βsβ * c1c2s15 + η3 * sβ * c1c2s15 - 4 * η3 * sβ * c1s15
		      theA(153) = 2 * η3 * cβsβ2 * c12s16
		      theA(154) = (15/2) * η3 * cβsβ2 * c2s22
		      theA(155) = (19/4) * η3 * s3β * c2c13s1 - (9/4) * η3 * sβ * c2c13s1 + (7/8) * η3 * s3β * c4c13s1 + (3/8) * η3 * sβ * c4c13s1 - (9/8) * η3 * s3β * c13s1 - (103/24) * η * sβ * c13s1 + (79/8) * δ0 * sβ * c13s1
		      theA(156) = (1/2) * η3 * c3β * c2c14 + (7/2) * η3 * cβ * c2c14 + (7/8) * η3 * c3β * c4c14 - (3/8) * η3 * c3β * c14 + (1/8) * η3 * cβ * c4c14 - (119/24) * η * cβ * c14 + (47/8) * δ0 * cβ * c14
		      theA(157) = 3 * η3 * c2βsβ * c2c15s1 + η3 * sβ * c2c15s1 + 4 * η3 * sβ * c15s1
		      theA(158) = 2 * η3 * cβsβ2 * c16s12
		      theA(159) = 28 * η3 * s3β * c15s13 + 12 * η3 * sβ * c15s13
		      theA(160) = 28 * η3 * c3β * c16s12 + 4 * η3 * cβ * c16s12
		      theA(161) = -24 * η3 * c2βsβ * c17s1 - 8 * η3 * sβ * c17s1
		      theA(162) = 8 * η3 * cβsβ2 * c18
		    End If
		    
		    If PNOrder > 2 then
		      // Order 3/2, Plus Polarization
		      theA(47) = -3 * π * δ0 * c0β * c14 - π * δ0 * c2β * c14
		      theA(48) = -4 * π * δ0 * s2β * c13s1
		      theA(49) = 4 * π * δ0 * s2β * c1s13
		      theA(50) = -3 * π * δ0 * c0β * s14 - π * δ0 * c2β * s14
		      theA(51) = -3 * π * δ0 * sβ2 * s22
		      theA(52) = (625/128) * δ2 * sβ3 * c110 + (625/384) * δ2 * c2β2sβ3 * c110
		      theA(53) = (19/4096) * δ2 * s3β * c2c12 + (35/12288) * δ2 * s5β * c2c12 + (1873/2048) * δ1 * sβ * c2c12 + (1901/8192) * δ * s3β * c2c12 - (10675/12288) * δ * sβ * c2c12
		      theA(54) = (6399/2048) * δ2 * s3β * c2c16 - (2187/2048) * δ2 * s5β * c2c16 - (2403/1024) * δ2 * sβ * c2c16 + (3159/8192) * δ2 * s3β * c4c16 + (3645/8192) * δ2 * s5β * c4c16 + (1701/4096) * δ2 * sβ * c4c16
		      theA(55) = (-11875/384) * δ2 * cβsβ2 * c19s1 - (3125/384) * δ2 * c3βsβ2 * c19s1
		      theA(56) = (351/128) * δ2 * cβsβ2 * c17s1 - (243/128) * δ2 * cβc2βsβ2 * c17s1 + (567/128) * δ2 * cβsβ2 * c2c17s1 + (405/128) * δ2 * cβc2βsβ2 * c2c17s1
		      theA(57) = (243/128) * δ2 * sβ3 * c18s12 + (81/128) * δ2 * c2βsβ3 * c18s12
		      theA(58) = (1901/8192) * δ * s3β * c2s12 - (10675/12288) * δ * sβ * c2s12 + (2833/16384) * δ * s3β * c4s12 - (1103/24576) * δ * sβ * c4s12 + (9653/65536) * δ * s3β * s12 - (43723/98304) * δ * sβ * s12
		      theA(59) = (151/1024) * δ2 * s3β * c2c14s12 - (3/1024) * δ2 * s5β * c2c14s12 - (27/512) * δ2 * sβ * c2c14s12 + (13/4096) * δ2 * s3β * c4c14s12 + (15/4096) * δ2 * s5β * c4c14s12 + (7/2048) * δ2 * sβ * c4c14s12
		      theA(60) = (4375/512) * δ2 * sβ * c18s12 + (8125/1024) * δ2 * s3β * c18s12 + (9375/1024) * δ2 * s5β * c18s12
		      theA(61) = (20475/4096) * δ * sβ * c14s12 - (149391/8192) * δ * s3β * c14s12 - (3195/2048) * δ1 * sβ * c14s12 + (45711/4096) * δ1 * s3β * c14s12
		      theA(62) = (-4375/192) * δ2 * cβ * c17s13 - (625/128) * δ2 * c3β * c17s13 - (3125/128) * δ2 * c5β * c17s13
		      theA(63) = (5/192) * δ2 * cβc2βsβ2 * c2c15s13 + (7/192) * δ2 * cβsβ2 * c2c15s13 + (37/192) * δ2 * cβsβ2 * c15s13
		      theA(64) = (1/64) * δ2 * sβ3 * c16s14 + (1/192) * δ2 * c2βsβ3 * c16s14
		      theA(65) = (151/1024) * δ2 * s3β * c2c12s14 - (3/1024) * δ2 * s5β * c2c12s14 - (27/512) * δ2 * sβ * c2c12s14 - (13/4096) * δ2 * s3β * c4c12s14 - (15/4096) * δ2 * s5β * c4c12s14 - (7/2048) * δ2 * sβ * c4c12s14
		      theA(66) = (4375/512) * δ2 * s3β * c16s14 - (21875/512) * δ2 * s5β * c16s14 + (4375/768) * δ2 * sβ * c16s14
		      theA(67) = (-10017/1024) * δ2 * s3β * c2c12s14 + (1701/1024) * δ2 * s5β * c2c12s14 - (2187/512) * δ2 * sβ * c2c12s14 - (1701/4096) * δ2 * s3β * c4c12s14 + (8505/4096) * δ2 * s5β * c4c12s14 - (567/2048) * δ2 * sβ * c4c12s14
		      theA(68) = (-5/192) * δ1 * cβc2βsβ2 * c2c13s15 - (1/192) * δ2 * cβc2βsβ2 * c13s15 - (7/192) * δ2 * cβsβ2 * c2c13s15 + (37/192) * δ2 * cβsβ2 * c13s15
		      theA(69) = (-1/192) * δ2 * c2βsβ3 * c14s16 - (1/64) * δ2 * sβ3 * c14s16
		      theA(70) = (-8145/2048) * δ * c2βsβ * s16 - (55539/8192) * δ * sβ * s16 + (1053/256) * δ2 * c2βsβ * c2s16 - (1701/1024) * δ1 * c2βsβ * c4s16 + (4689/1024) * δ1 * c2βsβ * s16
		      theA(71) = (-4375/512) * δ2 * s3β * c14s16 + (21875/512) * δ2 * s5β * c14s16 - (4375/768) * δ2 * sβ * c14s16
		      theA(72) = (-625/128) * δ2 * c3β * c13s17 - (3125/128) * δ2 * c5β * c13s17 - (4375/192) * δ2 * cβ * c13s17
		      theA(73) = (-405/128) * δ2 * cβc2βsβ2 * c1c2s17 - (243/128) * δ2 * cβc2βsβ2 * c1s17 - (567/128) * δ2 * cβsβ2 * c1c2s17 + (351/128) * δ2 * cβsβ2 * c1s17
		      theA(74) = (-243/128) * δ2 * sβ3 * c12s18 - (81/128) * δ2 * c2βsβ3 * c12s18
		      theA(75) = (-8125/1024) * δ2 * s3β * c12s18 - (9375/1024) * δ2 * s5β * c12s18 - (4375/512) * δ2 * sβ * c12s18
		      theA(76) = (-3125/384) * δ2 * c3βsβ2 * c1s19 - (11875/384) * δ2 * cβsβ2 * c1s19
		      theA(77) = (-625/384) * δ2 * c2βsβ3 * s110 - (625/128) * δ2 * sβ3 * s110
		      theA(78) = (-5103/2048) * δ2 * cβc2βsβ2 * c4s23 - (3969/2048) * δ2 * cβc2βsβ2 * s23 - (1701/2048) * δ2 * cβsβ2 * c4s23 + (10197/2048) * δ1 * cβsβ2 * s23 - (44757/4096) * δ * cβsβ2 * s23
		      theA(79) = (-13125/2048) * δ2 * c3βsβ2 * s25 - (21875/2048) * δ2 * cβsβ2 * s25
		      theA(80) = (243/512) * δ1 * cβc2β * c2s2 - (2835/1024) * δ * cβc2β * c2s2 - (5319/4096) * δ1 * cβc2β * c4s2 + (135/8192) * δ * cβc2β * c4s2 + (5967/8192) * δ1 * cβc2β * s2 - (37071/16384) * δ * cβc2β * s2
		      theA(81) = (5643/4096) * δ2 * cβc2β * s2 - (18603/8192) * δ * cβc2β * s2 + (2835/2048) * δ * cβc2β * s4 + (135/16384) * δ * cβc2β * s6 - (243/65536) * δ2 * c3β * s10 + (2565/16384) * δ2 * c3β * s8
		      theA(82) = (11/768) * δ1 * cβc2β * c2s2 + (133/1536) * δ * cβc2β * c2s2 + (77/2048) * δ1 * cβc2β * c4s2 + (211/4096) * δ * cβc2β * c4s2 + (257/12288) * δ1 * cβc2β * s2 + (319/24576) * δ * cβc2β * s2
		      theA(83) = (13/6144) * δ1 * cβc2β * s2 - (11/1536) * δ1 * cβc2β * s4 + (77/4096) * δ1 * cβc2β * s6 - (157/12288) * δ * cβc2β * s2 - (133/3072) * δ * cβc2β * s4 + (211/8192) * δ * cβc2β * s6
		      theA(84) = (-429/8192) * δ1 * cβsβ2 * s6 - (1/4096) * δ2 * cβc2βsβ2 * s2 + (7/8192) * δ2 * cβc2βsβ2 * s2 - (21/16384) * δ2 * c3βsβ2 * s10 - (35/16384) * δ2 * cβsβ2 * s10
		      
		      // Order 3/2, cross polarization
		      theA(173) = 8 * π * δ0 * sβ * c1s13
		      theA(174) = -4 * π * δ0 * cβ * s14
		      theA(175) = -8 * π * δ0 * sβ * c13s1
		      theA(176) = -4 * π * δ0 * cβ * c14
		      theA(177) = (4375/192) * δ2 * s2β * c14s16 + (4375/128) * δ2 * s4β * c14s16
		      theA(178) = (-625/48) * δ2 * c2β * c13s17 - (625/16) * δ2 * c4β * c13s17
		      theA(179) = (625/128) * δ2 * s2β * c12s18 - (5625/256) * δ2 * s4β * c12s18
		      theA(180) = (-625/24) * δ2 * c2βsβ2 * c1s19 - (625/48) * δ2 * sβ2 * c1s19
		      theA(181) = (-625/96) * δ2 * cβ * sβ3 * s110
		      theA(182) = (459/128) * δ2 * s2β * c2c12s14 - (2079/256) * δ2 * s4β * c2c12s14 + (567/512) * δ2 * s2β * c4c12s14 + (1701/1024) * δ2 * s4β * c4c12s14 - (4923/512) * δ1 * s2β * c12s14 - (945/1024) * δ2 * s4β * c12s14 + (22203/1024) * δ * s2β * c12s14
		      theA(183) = (27/16) * δ2 * c2β * c1c2s15 - (81/128) * δ2 * c2β * c1c4s15 + (1233/128) * δ1 * c2β * c1s15 - (4689/256) * δ * c2β * c1s15 + (27/16) * δ2 * c4β * c1c2s15 + (27/8) * δ2 * c1c2s15 - (243/128) * δ2 * c4β * c1c4s15 + (27/128) * δ2 * c4β * c1s15 + (27/16) * δ2 * c0β * c1s15
		      theA(184) = (-621/256) * δ2 * c2βs2β * c2s16 - (2187/1024) * δ2 * c2βs2β * c4s16 - (1377/1024) * δ2 * c2βs2β * s16 + (837/256) * δ2 * s2β * c2s16 + (243/1024) * δ2 * s2β * c4s16 + (4761/1024) * δ1 * s2β * s16 - (11673/2048) * δ * s2β * s16
		      theA(185) = (-81/16) * δ2 * c2βsβ2 * c1c2s17 - (27/16) * δ2 * c2βsβ2 * c1s17 - (81/32) * δ2 * sβ2 * c1c2s17 + (81/32) * δ2 * sβ2 * c1s17
		      theA(186) = (-81/32) * δ2 * cβsβ3 * c12s18
		      theA(187) = (-7/49152) * δ2 * s2β * c10 - (7/32768) * δ2 * s4β * c10 + (11/6144) * δ2 * c2βs2β * c4 - (91/16384) * δ2 * c2βs2β * c6 - (173/24576) * δ2 * cβs3β * c2 + (6031/24576) * δ1 * cβsβ * c2 - (10511/49152) * δ * cβsβ * c2
		      theA(188) = (-37/256) * δ1 * c2β * c1c2s13 - (347/512) * δ * c2β * c1c2s13 + (3/128) * δ2 * c2β * c1c4s13 - (1/768) * δ2 * c2β * c1c6s13 - (35/128) * δ1 * c2β * c1s13 - (23/768) * δ * c2β * c1s13
		      theA(189) = (13/128) * δ2 * s2β * c2c12s14 + (11/256) * δ2 * s4β * c2c12s14 + (1/512) * δ2 * s2β * c4c12s14 - (9/1024) * δ2 * s4β * c4c12s14 - (29/512) * δ1 * s2β * c12s14 + (5/1024) * δ2 * s4β * c12s14 - (355/1024) * δ * s2β * c12s14
		      theA(190) = (-1/24) * δ2 * c2βsβ2 * c2c13s15 + (1/24) * δ2 * c2βsβ2 * c13s15 - (1/48) * δ2 * sβ2 * c2c13s15 + (7/48) * δ2 * sβ2 * c13s15
		      theA(191) = (-1/48) * δ2 * cβsβ3 * c14s16
		      theA(192) = (-1/128) * δ2 * c2βsβ2 * s4 - (7/256) * δ2 * c2βsβ2 * s8 + (45/128) * δ1 * sβ2 * s4 - (5/256) * δ2 * sβ2 * s8 - (77/256) * δ * sβ2 * s4
		      theA(193) = (-189/32) * δ2 * c2βsβ2 * c2s23 - (135/32) * δ2 * sβ2 * c2s23
		      theA(194) = (-7/49152) * δ2 * s2β * c10 - (7/32768) * δ2 * s4β * c10 - (11/6144) * δ2 * c2βs2β * c4 - (91/16384) * δ2 * c2βs2β * c6 - (173/24576) * δ2 * cβs3β * c2 + (6031/24576) * δ1 * cβsβ * c2 - (10511/49152) * δ * cβsβ * c2
		      theA(195) = (37/256) * δ1 * c2β * c2c13s1 + (347/512) * δ * c2β * c2c13s1 + (3/128) * δ2 * c2β * c4c13s1 + (1/768) * δ2 * c2β * c6c13s1 - (35/128) * δ1 * c2β * c13s1 - (23/768) * δ * c2β * c13s1
		      theA(196) = (13/128) * δ2 * s2β * c2c14s12 + (11/246) * δ2 * s4β * c2c14s12 - (1/512) * δ2 * s2β * c4c14s12 + (9/1024) * δ2 * s4β * c4c14s12 - (5/1024) * δ2 * s4β * c14s12 + (355/1024) * δ * s2β * c14s12 + (29/512) * δ1 * s2β * c14s12
		      theA(197) = (7/48) * δ2 * sβ2 * c15s12 + (1/24) * δ2 * c2βsβ2 * c15s12 + (1/48) * δ2 * sβ2 * c2c15s12 + (1/24) * δ2 * c2βsβ2 * c2c15s12
		      theA(198) = (1/48) * δ2 * cβsβ3 * c16s14
		      theA(199) = (459/128) * δ2 * s2β * c2c14s12 - (2079/512) * δ2 * s4β * c2c14s12 - (567/512) * δ2 * s2β * c4c14s12 - (1701/1024) * δ2 * s4β * c4c14s12 + (945/1024) * δ2 * s4β * c14s12 + (4923/512) * δ1 * s2β * c14s12 - (22203/1024) * δ * s2β * c14s12
		      theA(200) = (-27/16) * δ2 * c2β * c2c15s1 - (81/128) * δ2 * c2β * c4c15s1 - (27/16) * δ2 * c4β * c2c15s1 - (27/8) * δ2 * c0β * c2c15s1 - (243/128) * δ2 * c4β * c4c15s1 + (27/128) * δ2 * c4β * c15s1 + (27/16) * δ2 * c0β * c15s1 + (1233/128) * δ1 * c2β * c15s1 - (4689/256) * δ * c2β * c15s1
		      theA(201) = (837/256) * δ2 * s2β * c2c16 - (621/512) * δ2 * s4β * c2c16 - (243/1024) * δ2 * s2β * c4c16 + (2187/2048) * δ2 * s4β * c4c16 + (1377/2048) * δ2 * s4β * c16 - (4761/1024) * δ1 * s2β * c16 + (11673/2048) * δ * s2β * c16
		      theA(202) = (81/16) * δ2 * c2βsβ2 * c2c17s1 - (27/16) * δ2 * c2βsβ2 * c17s1 + (81/32) * δ2 * sβ2 * c2c17s1 + (81/32) * δ2 * sβ2 * c17s1
		      theA(203) = (81/32) * δ2 * cβsβ3 * c18s12
		      theA(204) = (-4375/192) * δ2 * s2β * c16s14 - (4375/128) * δ2 * s4β * c16s14
		      theA(205) = (-625/48) * δ2 * c2β * c17s13 - (625/16) * δ2 * c4β * c17s13
		      theA(206) = (-625/128) * δ2 * s2β * c18s12 + (5625/256) * δ2 * s4β * c18s12
		      theA(207) = (-625/24) * δ2 * c2βsβ2 * c19s1 - (625/48) * δ2 * sβ2 * c19s1
		      theA(208) = (625/96) * δ2 * cβsβ3 * c110
		    End If
		  End If
		  
		  // Now do spin stuff
		  
		  If PNOrder > 1 Then
		    // Order 2/2, SO, Plus polarization
		    theA(37) = δ0 * cβ * c12 * χax - δ0 * sβ * c12 * χaz
		    theA(38) = (1/2) * δ0 * cβ * c0 * χax - (1/2) * δ0 * cβ * c2 * χax - δ0 * sβ * s12 * χaz
		    theA(39) = -δ0 * cβ * s12 * χay
		    theA(40) = -δ0 * sβ * s2 * χay
		    theA(41) = -δ0 * cβ * c12 * χay
		    theA(42) = δ * cβ * c12 * χsx - δ * sβ * c12 * χsz
		    theA(43) = (1/2) * δ * cβ * χsx - (1/2) * δ * cβ * c2 * χsx - δ * sβ * s12 * χsz
		    theA(44) = -δ * cβ * s12 * χsy
		    theA(45) = -δ * sβ * s2 * χsy
		    theA(46) = -δ * cβ * c12 * χsy
		    
		    // Order 2/2, SO, Cross polarization
		    theA(163) = (1/2) * δ0 * c0β * c0 * χay + (1/2) * δ0 * c0β * c2 * χay
		    theA(164) = δ0 * c0β * s12 * χay
		    theA(165) = (1/2) * δ0 * cβ2 * c0 * χax - (1/2) * δ0 * cβ2 * c2 * χax - (1/2) * δ0 * cβsβ * c0 * χaz + (1/2) * δ0 * cβsβ * c2 * χaz
		    theA(166) = δ0 * cβsβ * s2 * χax - δ0 * sβ2 * s2 * χaz
		    theA(167) = (1/2) * δ0 * cβ2 * c0 * χax + (1/2) * δ0 * cβ2 * c2 * χax - (1/2) * δ0 * cβsβ * c0 * χaz - (1/2) * δ0 * cβsβ * c2 * χaz
		    theA(168) = (1/2) * δ * c0β * c0 * χsy + (1/2) * δ * c0β * c2 * χsy
		    theA(169) = δ * c0β * s12 * χsy
		    theA(170) = (-1/2) * δ * cβ2 * c2 * χsx + (1/2) * δ * cβ2 * c0 * χsx + (1/2) * δ * cβsβ * c2 * χsz - (1/2) * δ * cβsβ * c0 * χsz
		    theA(171) = δ * cβsβ * s2 * χsx - δ * sβ2 * s2 * χsz
		    theA(172) = (1/2) * δ * cβ2 * c2 * χsx + (1/2) * δ * cβ2 * c0 * χsx - (1/2) * δ * cβsβ * c2 * χsz - (1/2) * δ * cβsβ * c0 * χsz
		    
		    If PNOrder > 2 Then
		      // Order 3/2, SO, plus polarization
		      theA(85) = η2 * cβsβ * c23 * χsx
		      theA(86) = (1/6) * η10 * c2β * c2c14 * χsz - (7/2) * η * c2β * c14 * χsz - δ0 * c2β * c14 * χsz - (1/3) * η10 * s2β * c2c14 * χsx + (1/2) * η10 * c0β * c2c14 * χsz - (19/6) * η * s2β * c14 * χsx - (5/2) * η * c0β * c14 * χsz + (7/3) * δ0 * s2β * c14 * χsx - 3 * δ0 * c0β * c14 * χsz
		      theA(87) = (1/2) * η10 * c0β * c15s1 * χsx + (1/6) * η10 * c2β * c15s1 * χsx
		      theA(88) = (1/2) * η10 * c0β * c1s15 * χsx + (1/6) * η10 * c2β * c1s15 * χsx
		      theA(89) = (7/12) * η10 * c2β * c2c13s1 * χsx + (79/12) * η * c2β * c13s1 * χsx - (13/6) * δ0 * c2β * c13s1 * χsx - (1/4) * η10 * c0β * c2c13s1 * χsx + (2/3) * η10 * s2β * c2c13s1 * χsz - (17/4) * η * c0β * c13s1 * χsx - 7 * η * s2β * c13s1 * χsz + (3/2) * δ0 * c0β * c13s1 * χsx - 2 * δ0 * s2β * c13s1 * χsz
		      theA(90) = (-7/12) * η10 * c2β * c1c2s13 * χsx + (79/12) * η * c2β * c1s13 * χsx - (13/6) * δ0 * c2β * c1s13 * χsx + (1/4) * η10 * c0β * c1c2s13 * χsx + (2/3) * η10 * s2β * c1c2s13 * χsz - (17/4) * η * c0β * c1s13 * χsx + (3/2) * δ0 * c0β * c1s13 * χsx - 7 * η * s2β * c1s13 * χsz - 2 * δ0 * s2β * c1s13 * χsz
		      theA(91) = (1/6) * η10 * c2β * c2s14 * χsz + (7/2) * η * c2β * s14 * χsz + δ0 * c2β * s14 * χsz - (1/3) * η10 * s2β * c2s14 * χsx + (1/2) * η10 * c0β * c2s14 * χsz + (19/6) * η * s2β * s14 * χsx - (7/3) * δ0 * s2β * s14 * χsx + (5/2) * η * c0β * s14 * χsz + 3 * δ0 * c0β * s14 * χsz
		      theA(92) = (-3/2) * η2 * sβ2 * c2s22 * χsz
		      theA(93) = (3/8) * η2 * c0β * s23 * χsx + (1/8) * η2 * c2β * s23 * χsx
		      theA(94) = (1/3) * η10 * cβsβ * c2s22 * χsx + (1/2) * η10 * sβ2 * c2s22 * χsz
		      theA(95) = (3/4) * η2 * c0β * c2s22 * χsz + (1/4) * η2 * c2β * c2s22 * χsz - (1/2) * η2 * s2β * c2s22 * χsx
		      theA(96) = (-11/32) * η2 * c2β * s2 * χsx - (3/8) * η2 * c0β * s23 * χsx - (7/32) * η2 * c2β * s6 * χsx + (1/4) * η2 * s2β * s2 * χsz - (1/4) * η2 * s2β * s6 * χsz
		      theA(97) = (15/16) * η2 * c0β * s2 * χsy - (3/16) * η2 * c2β * s2 * χsy + (9/16) * η2 * c0β * c4s2 * χsy - (5/16) * η2 * c2β * c4s2 * χsy
		      theA(98) = -η2 * cβsβ * c2s22 * χsy
		      theA(99) = (3/8) * η2 * c0β * s23 * χsy + (1/8) * η2 * c2β * s23 * χsy
		      theA(100) = (-5/12) * η10 * c2β * c1c2s13 * χsy + (3/4) * η10 * c0β * c1c2s13 * χsy + (1/4) * η10 * c0β * c1s13 * χsy - (31/12) * η * c2β * c1s13 * χsy - (11/6) * δ0 * c2β * c1s13 * χsy
		      theA(101) = (-7/3) * δ0 * s2β * s14 * χsy - (5/6) * η * s2β * s14 * χsy - (1/3) * η10 * s2β * c2s14 * χsy
		      theA(102) = (1/2) * η10 * c0β * c1s15 * χsy + (1/6) * η10 * c2β * c1s15 * χsy
		      theA(103) = (-1/3) * δ0 * cβsβ * s22 * χsy - (11/6) * η * cβsβ * s22 * χsy
		      theA(104) = (1/4) * η10 * c0β * c13s1 * χsy - (3/4) * η10 * c0β * c2c13s1 * χsy + (5/12) * η10 * c2β * c2c13s1 * χsy - (31/12) * η * c2β * c13s1 * χsy - (11/6) * δ0 * c2β * c13s1 * χsy
		      theA(105) = (7/3) * δ0 * s2β * c14 * χsy + (5/6) * η * s2β * c14 * χsy - (1/3) * η10 * s2β * c2c14 * χsy
		      theA(106) = (1/2) * η10 * c0β * c15s1 * χsy + (1/6) * η10 * c2β * c15s1 * χsy
		      theA(107) = 2 * δ * cβsβ * c23 * χax
		      theA(108) = -3 * δ * c0β * c14 * χaz - δ * c2β * c14 * χaz + 5 * δ * c0β * c2c14 * χaz + (5/3) * δ * c2β * c2c14 * χaz + (7/3) * δ * s2β * c14 * χax - (10/3) * δ * s2β * c2c14 * χax
		      theA(109) = 5 * δ * c0β * c15s1 * χax + (5/3) * δ * c2β * c15s1 * χax
		      theA(110) = 5 * δ * c0β * c1s15 * χax + (5/3) * δ * c2β * c1s15 * χax
		      theA(111) = (35/6) * δ * c2β * c2c13s1 * χax - (13/6) * δ * c2β * c13s1 * χax - (5/2) * δ * c0β * c2c13s1 * χax + (20/3) * δ * s2β * c2c13s1 * χaz + (3/2) * δ * c0β * c13s1 * χax - 2 * δ * s2β * c13s1 * χaz
		      theA(112) = (-35/6) * δ * c2β * c1c2s13 * χax - (13/6) * δ * c2β * c1s13 * χax + (5/2) * δ * c0β * c1c2s13 * χax - (20/3) * δ * s2β * c1c2s13 * χaz + (3/2) * δ * c0β * c1s13 * χax - 2 * δ * s2β * c1s13 * χaz
		      theA(113) = (5/3) * δ * c2β * c2s14 * χaz + δ * c2β * s14 * χaz - (10/3) * δ * s2β * c2s14 * χax + 5 * δ * c0β * c2s14 * χaz - (7/3) * δ * s2β * s14 * χax + 3 * δ * c0β * s14 * χaz
		      theA(114) = -3 * δ * sβ2 * c2s22 * χaz
		      theA(115) = (3/4) * δ * c0β * s23 * χax + (1/4) * δ * c2β * s23 * χax
		      theA(116) = (10/3) * δ * cβsβ * c2s22 * χax + 5 * δ * sβ2 * c2s22 * χaz
		      theA(117) = (3/2) * δ * c0β * c2s22 * χaz + (1/2) * δ * c2β * c2s22 * χaz - δ * s2β * c2s22 * χax
		      theA(118) = (-11/16) * δ * c2β * s2 * χax - (7/16) * δ * c2β * s6 * χax - (3/4) * δ * c0β * s23 * χax + (1/2) * δ * s2β * s2 * χaz - (1/2) * δ * s2β * s6 * χaz
		      theA(119) = (-5/8) * δ * c2β * c4s2 * χay - (3/8) * δ * c2β * s2 * χay + (9/8) * δ * c0β * c4s2 * χay + (15/8) * δ * c0β * s2 * χay
		      theA(120) = -2 * δ * cβsβ * c2s22 * χay
		      theA(121) = (3/4) * δ * c0β * s23 * χay + (1/4) * δ * c2β * s23 * χay
		      theA(122) = (-25/6) * δ * c2β * c1c2s13 * χay - (11/6) * δ * c2β * c1s13 * χay + (15/2) * δ * c0β * c1c2s13 * χay + (5/2) * δ * c0β * c1s13 * χay
		      theA(123) = (-7/3) * δ * s2β * s14 * χay - (10/3) * δ * s2β * c2s14 * χay
		      theA(124) = 5 * δ * c0β * c1s15 * χay + (5/3) * δ * c2β * c1s15 * χay
		      theA(125) = (-1/3) * δ * cβsβ * s22 * χay
		      theA(126) = (25/6) * δ * c2β * c2c13s1 * χay - (11/6) * δ * c2β * c13s1 * χay - (15/2) * δ * c0β * c2c13s1 * χay + (5/2) * δ * c0β * c13s1 * χay
		      theA(127) = (7/3) * δ * s2β * c14 * χay - (10/3) * δ * s2β * c2c14 * χay
		      theA(128) = 5 * δ * c0β * c15s1 * χay + (5/3) * δ * c2β * c15s1 * χay
		      
		      // Order 3/2, SO, cross polarization
		      theA(209) = η2 * sβ * c23 * χsy
		      theA(210) = (2/3) * η10 * sβ * c2c14 * χsy - (5/3) * η * sβ * c14 * χsy - (14/3) * δ0 * sβ * c14 * χsy
		      theA(211) = (-2/3) * η10 * cβ * c15s1 * χsy
		      theA(212) = (1/3) * η10 * cβ * c2c13s1 * χsy + (7/3) * η * cβ * c13s1 * χsy - (2/3) * δ0 * cβ * c13s1 * χsy
		      theA(213) = (-1/3) * η10 * cβ * c1c2s13 * χsy + (7/3) * η * cβ * c1s13 * χsy - (2/3) * δ0 * cβ * c1s13 * χsy
		      theA(214) = (2/3) * η10 * sβ * c2s14 * χsy + (5/3) * η * sβ * s14 * χsy + (14/3) * δ0 * sβ * s14 * χsy
		      theA(215) = (-2/3) * η10 * cβ * c1s15 * χsy
		      theA(216) = η2 * sβ * c2s22 * χsy
		      theA(217) = (1/3) * η10 * sβ * c2s22 * χsy
		      theA(218) = (-1/2) * η2 * cβ * s23 * χsy
		      theA(219) = (-5/8) * η2 * cβ * s2 * χsy - (1/8) * η2 * cβ * s6 * χsy
		      theA(220) = (-3/4) * η2 * cβ * s2 * χsx - (1/4) * η2 * cβ * c4s2 * χsx - η2 * sβ * c4s2 * χsz
		      theA(221) = η2 * cβ * c2s22 * χsz - η2 * sβ * c2s22 * χsx
		      theA(222) = 2 * η2 * cβ * s23 * χsx
		      theA(223) = (-2/3) * δ0 * cβ * c1s13 * χsx - (10/3) * δ0 * cβ * c1c2s13 * χsx - (5/3) * η * cβ * c1s13 * χsx + 4 * η * c3β * c1s13 * χsx - (1/3) * δ0 * cβ * c1c2s13 * χsx
		      theA(224) = 4 * δ0 * cβ * s14 * χsz + (20/3) * δ0 * cβ * c2s14 * χsz + (5) * η * cβ * s14 * χsz + 4 * η * c3β * s14 * χsz + (2/3) * δ0 * cβ * c2s14 * χsz
		      theA(225) = (2/3) * η10 * cβ * c1s15 * χsx
		      theA(226) = -6 * η * cβsβ2 * s22 * χsz + (1/3) * δ0 * sβ * s22 * χsx - (7/6) * η * sβ * s22 * χsx + 3 * η * c2βsβ * s22 * χsx
		      theA(227) = (-5/3) * η * cβ * c13s1 * χsx + 4 * η * c3β * c13s1 * χsx + (1/3) * η * cβ * c2c13s1 * χsx - (2/3) * δ0 * cβ * c13s1 * χsx
		      theA(228) = (-5) * η * cβ * c14 * χsz - η * c3β * c14 * χsz + (2/3) * η * cβ * c2c14 * χsz - 4 * δ0 * cβ * c14 * χsz
		      theA(229) = (2/3) * η10 * cβ * c15s1 * χsx
		      theA(230) = 2 * δ * sβ * c23 * χay
		      theA(231) = (-14/3) * δ * sβ * c14 * χay + (20/3) * δ * sβ * c2c14 * χay
		      theA(232) = (-2/3) * δ * cβ * c13s1 * χay + (10/3) * δ * cβ * c2c13s1 * χay
		      theA(233) = (-20/3) * δ * cβ * c15s1 * χay
		      theA(234) = (-10/3) * δ * cβ * c1c2s13 * χay - (2/3) * δ * cβ * c1s13 * χay
		      theA(235) = (20/3) * δ * sβ * c2s14 * χay + (14/3) * δ * sβ * s14 * χay
		      theA(236) = (-20/3) * δ * cβ * c1s15 * χay
		      theA(237) = 2 * δ * sβ * c2s22 * χay
		      theA(238) = (10/3) * δ * sβ * c2s22 * χay
		      theA(239) = -δ * cβ * s13 * χay
		      theA(240) = (-5/4) * δ * cβ * s2 * χay - (1/4) * δ * cβ * s6 * χay
		      theA(241) = (-1/2) * δ * cβ * c4s2 * χax - 2 * δ * sβ * c4s2 * χaz - (3/2) * δ * cβ * s2 * χax
		      theA(242) = 2 * δ * cβ * c2s22 * χaz - 2 * δ * sβ * c2s22 * χax
		      theA(243) = δ * cβ * s23 * χax
		      theA(244) = (-10/3) * δ * cβ * c1c2s13 * χax - (40/3) * δ * sβ * c1c2s13 * χaz - (2/3) * δ * cβ * c1s13 * χax - 4 * δ * sβ * c1s13 * χaz
		      theA(245) = (20/3) * δ * cβ * c2s14 * χaz - (20/3) * δ * sβ * c2s14 * χax + 4 * δ * cβ * s14 * χaz - (14/3) * δ * sβ * s14 * χax
		      theA(246) = (20/3) * δ * cβ * c1s15 * χax
		      theA(247) = (1/3) * δ * sβ * s22 * χax
		      theA(248) = (10/3) * δ * cβ * c2c13s1 * χax - (2/3) * δ * cβ * c13s1 * χax + (40/3) * δ * sβ * c2c13s1 * χaz - 4 * δ * sβ * c13s1 * χaz
		      theA(249) = (20/3) * δ * cβ * c2c14 * χaz - 4 * δ * cβ * c14 * χaz + (14/3) * δ * sβ * c14 * χax - (20/3) * δ * sβ * c2c14 * χax
		      theA(250) = (20/3) * δ * cβ * c15s1 * χax
		    End If
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
		Sub Constructor(P As CaseInfoClass = Nil)
		  // If P is Nil we are just creating the class to check variable names
		  If P <> Nil Then
		    // Initialize constants
		    Parameters = P
		    π = P.π
		    VeSinΘ = P.Ve*Sin(P.Θ)
		    VeCosΘ = P.Ve*Cos(P.Θ)
		    Δτr = P.ΔT/P.GM
		    Δτ = Δτr/(1.0 + P.Z)
		    SpinEvolver = New SpinEvolverClass(P)
		    
		    // Initialize the Noise class
		    //Noise = New NoiseClass(Parameters.ΔT)
		    
		    // Set up classes for calculating derivatives
		    
		    δFunctions = New DeltaFuncsClass
		    δFunctions.SetValues(P.δ)
		    δFunDerivs = New DeltaFuncsClass
		    δFunDerivs.SetDerivs(P.δ)
		    
		    βFunctions = New BetaFuncsClass
		    βFunctions.SetValues(P.β)
		    βFunDerivs = New BetaFuncsClass
		    βFunDerivs.SetDerivs(P.β)
		    
		    ιFunctions = New IotaFuncsClass
		    ιFunDerivs = New IotaFuncsClass
		    
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DidDetectorStepOK(StepNumber As Integer) As Boolean
		  // If we are within two detector steps of coalescence, bail out
		  If (StepNumber + 2)*Δτ > Parameters.τc Then
		    Return False
		  End If
		  
		  // Otherwise, check if we can get data from the spin evolver
		  τrDN = StepNumber*Δτr
		  SpinResults = SpinEvolver.GetSpinDataAtTime(τrDN)
		  If SpinResults = Nil Then Return False  // If the method returns nothing, coalescence must have happened
		  
		  // Set up functions that depend on Iota
		  ιFunctions.SetValues(SpinResults.ι)
		  ιFunDerivs.SetDerivs(SpinResults.ι)
		  
		  // Calculate the wave phase
		  ΨrDN = SpinResults.Ψ
		  // do the following instead of the above if we want the data in the orbiting LISA frame
		  // ΨrDN = ΨrDP + (1.0 + Parameters.Ve*Sin(Parameters.Θ)*Sin(Parameters.GMΩe*τrm - Parameters.Φ))*(spinData.Ψ - ΨP)
		  // ΨrDP = ΨrDN
		  // ΨP = spinData.Ψ
		  
		  // Calculate the wave
		  AssembleDerivatives
		  
		  // Write out useful information for plotting (if this is not a case from a file)
		  If Parameters.DataRecorder <> Nil Then Parameters.DataRecorder.WriteData
		  
		  // We have completed the detector step successfully
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetHSum(Amp() As Double, Wave() As Double, Cross As Boolean, DoVDeriv As Boolean = False) As Double
		  // These constants define static variables indicating the endpoints of
		  // polarizations and post-newtonian orders. Remember that
		  Static H0PLastIndex As Integer = 4
		  Static H1PLastIndex As integer = 18
		  Static H2PLastIndex As Integer = 46
		  Static H3PLastIndex As Integer = 128
		  Static H0XLastIndex As Integer = 132
		  Static H1XLastIndex As Integer = 145
		  Static H2XLastIndex As Integer = 172
		  Static H3XLastIndex As Integer = 250
		  
		  Var jStart As Integer
		  Var sum As Double
		  Var v As Double = SpinResults.V
		  Var h As Double
		  Var vpower As Double
		  
		  If Not Cross Then  // The user has requested plus polarization
		    For j As Integer = 0 To H0PLastIndex
		      sum = sum + Amp(j)*W(j)
		    Next
		    If DoVDeriv Then
		      If DoVDeriv Then vpower = 2.0*v
		    Else
		      vpower = v*v
		    End If
		    h = sum*vpower
		    
		    If Parameters.PNOrder > 0 Then
		      sum = 0.0
		      jStart = H0PLastIndex + 1
		      For j As Integer = jStart to H1PLastIndex
		        sum = sum + Amp(j)*Wave(j)
		      Next
		      vpower = vpower*v
		      If DoVDeriv Then vpower = 1.5*vpower
		      h = h + sum*vpower
		    End If
		    
		    If Parameters.PNOrder > 1 Then
		      sum = 0.0
		      jStart = H1PLastIndex + 1
		      For j As Integer = jStart to H2PLastIndex
		        sum = sum + Amp(j)*Wave(j)
		      Next
		      vpower = vpower*v
		      If DoVDeriv Then vpower = 1.33333333333333333*vpower
		      h = h + sum*vpower
		    End If
		    
		    If Parameters.PNOrder > 2 Then
		      jStart = H2PLastIndex + 1
		      sum = 0.0
		      For j As Integer = jStart to H3PLastIndex
		        sum = sum + Amp(j)*Wave(j)
		      Next
		      vpower = vpower*v
		      If DoVDeriv Then vpower = 1.25*vpower
		      h = h + sum*vpower
		    End If
		    
		    
		  Else  // The cross polarization is requested
		    vpower = v*v
		    jStart = H3PLastIndex + 1
		    For j As Integer = jStart To H0XLastIndex
		      sum = sum + Amp(j)*Wave(j)
		    Next
		    If DoVDeriv Then
		      vpower = 2.0*v
		    Else
		      vpower = v*v
		    End If
		    h = sum*vpower
		    
		    If Parameters.PNOrder > 0 Then
		      sum = 0.0
		      jStart = H0XLastIndex + 1
		      For j As Integer = jStart to H1XLastIndex
		        sum = sum + Amp(j)*Wave(j)
		      Next
		      vpower = vpower*v
		      If DoVDeriv Then vpower = 1.5*vpower
		      h = h + sum*vpower
		    End If
		    
		    If Parameters.PNOrder > 1 Then
		      sum = 0.0
		      jStart = H1XLastIndex + 1
		      For j As Integer = jStart to H2XLastIndex
		        sum = sum + Amp(j)*Wave(j)
		      Next
		      vpower = vpower*v
		      If DoVDeriv Then vpower = 1.3333333333333333*vpower
		      h = h + sum*vpower
		    End If
		    
		    If Parameters.PNOrder > 2 Then
		      jStart = H2XLastIndex + 1
		      sum = 0.0
		      For j As Integer = jStart to H3XLastIndex
		        sum = sum + Amp(j)*Wave(j)
		      Next
		      vpower = vpower*v
		      If DoVDeriv Then vpower = 1.25*vpower
		      h = h + sum*vpower
		    End If
		  End If
		  
		  // Return the summed terms
		  Return h
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetNamedValue(theName As String) As Double
		  If theName = "t-y" Then 
		    If Parameters = Nil Then
		      Return 0
		    Else
		      Return τrDN * Parameters.GM / Parameters.Year
		    End If
		  End If
		  If theName = "t-s" Then
		    If Parameters = Nil Then
		      Return 0
		    Else
		      Return τrDN * Parameters.GM
		    End If
		  End If
		  If theName = "H" Then Return H
		  If theName = "V" Then Return SpinResults.V
		  If theName = "α" Then Return SpinResults.α
		  If theName = "ι" Then Return SpinResults.ι
		  If theName = "Ψ" Or theName = "ΨrDN" Then Return ΨrDN
		  If theName = "χ1x" Then Return SpinResults.χsx + SpinResults.χax
		  If theName = "χ1y" Then Return SpinResults.χsy + SpinResults.χay
		  If theName = "χ1z" Then Return SpinResults.χsz + SpinResults.χaz
		  If theName = "χ2x" Then Return SpinResults.χsx - SpinResults.χax
		  If theName = "χ2y" Then Return SpinResults.χsy - SpinResults.χay
		  If theName = "χ2z" Then Return SpinResults.χsz - SpinResults.χaz
		  
		  // This part handles a request for a value in an array. (Handle all non-array possibilities first.)
		  Var parts() As String = theName.Split("(") // split the name into parts at the open parenthesis
		  Var arrayName As String = parts(0) // we are always going to have this part
		  If parts.LastIndex = 0 Then Raise New RuntimeException("No Open Parenthesis")
		  If Not parts(1).EndsWith(")") Then Raise New RuntimeException("No Close Parenthesis")
		  parts = parts(1).Split(")") // split at the close parenthesis
		  If parts.LastIndex > 1 or Not parts(1).IsEmpty Then Raise New RuntimeException("Characters After Close Parenthesis")
		  parts = parts(0).Split(",") // split at a comma, if there is one
		  Var index1 As Integer = -1
		  If parts(0).ToInteger.ToString <> parts(0) Then
		    Raise New RuntimeException("Index Not An Integer")
		  Else
		    index1 = parts(0).ToInteger
		    If index1 < 0 Then Raise New RuntimeException("Index Negative")
		  End If
		  Var index2 As Integer = -1
		  If parts.LastIndex > 0 Then
		    If parts.LastIndex > 1 Then Raise New RuntimeException("Too Many Indices")
		    If parts(1).ToInteger.ToString <> parts(1) Then Raise New RuntimeException("Index Not An Integer")
		    index2 = parts(1).ToInteger
		    If index2 < 0 Then Raise New RuntimeException("Index Negative")
		  End If
		  
		  // Handle necdet array properties safely
		  If necdet = Nil Then
		    Raise New RuntimeException("Error: necdet is not initialized.")
		  End If
		  
		  Select Case arrayName
		  Case "fdiff"
		    //If index1 <= UBound(necdet.nDHDq) Then Return ((necdet.nDHDq(index1) - DHDq(index1)) / DHDq(index1)) / necdet.h0diff
		    If index1 <= UBound(necdet.nDHDq) Then Return (necdet.nDHDq(index1) - DHDq(index1)) / necdet.h0diff
		  Case "nDHDq"
		    If index1 <= UBound(necdet.nDHDq) Then Return necdet.nDHDq(index1)
		  Case "nDVI"
		    If index1 <= UBound(necdet.nDVI) Then Return necdet.nDVI(index1)
		  Case "nDιI"
		    If index1 <= UBound(necdet.nDιI) Then Return necdet.nDιI(index1)
		  Case "nDαI"
		    If index1 <= UBound(necdet.nDαI) Then Return necdet.nDαI(index1)
		  Case "nDχaxI"
		    If index1 <= UBound(necdet.nDχaxI) Then Return necdet.nDχaxI(index1)
		  Case "nDχayI"
		    If index1 <= UBound(necdet.nDχayI) Then Return necdet.nDχayI(index1)
		  Case "nDχazI"
		    If index1 <= UBound(necdet.nDχazI) Then Return necdet.nDχazI(index1)
		  Case "nDχsxI"
		    If index1 <= UBound(necdet.nDχsxI) Then Return necdet.nDχsxI(index1)
		  Case "nDχsyI"
		    If index1 <= UBound(necdet.nDχsyI) Then Return necdet.nDχsyI(index1)
		  Case "nDχszI"
		    If index1 <= UBound(necdet.nDχszI) Then Return necdet.nDχszI(index1)
		  Case "nDΨI"
		    If index1 <= UBound(necdet.nDΨI) Then Return necdet.nDΨI(index1)
		  End Select
		  
		  // Handle arrays W and A
		  If arrayName = "W" And IndicesCheck(index1, 250, index2, -1) Then Return W(index1)
		  If arrayName = "A" And IndicesCheck(index1, 250, index2, -1) Then Return A(index1)
		  
		  Raise New RuntimeException("Name Not Found")
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function IndicesCheck(index1 as Integer, max1 as Integer, index2 as Integer, max2 as Integer) As Boolean
		  If index2 = 1 And max2 <> -1 Then Raise New RuntimeException("No Required Second Index")
		  If index1 > max1 Or index2 > max2 Then Raise New RuntimeException("Index Out of Range")
		  Return True
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		A(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DADι(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DADβ(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DADδ(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DADχax(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DADχay(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DADχaz(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DADχsx(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DADχsy(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DADχsz(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dh0dδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DHDq(14) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DWDα(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DWDΨ(250) As Double
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
		FP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FX As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		H As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		H0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		necdet As NecdetsClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Noise As NoiseClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseInfoClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SpinEvolver As SpinEvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SpinResults As SpinResultsClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VeCosΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VeSinΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		W(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ιFunctions As IotaFuncsClass
	#tag EndProperty

	#tag Property, Flags = &h0
		ιFunDerivs As IotaFuncsClass
	#tag EndProperty

	#tag Property, Flags = &h0
		βFunctions As BetaFuncsClass
	#tag EndProperty

	#tag Property, Flags = &h0
		βFunDerivs As BetaFuncsClass
	#tag EndProperty

	#tag Property, Flags = &h0
		δFunctions As DeltaFuncsClass
	#tag EndProperty

	#tag Property, Flags = &h0
		δFunDerivs As DeltaFuncsClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Δτ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Δτr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		π As Double = 3.1415926535897
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
			Name="DZDlnR"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H"
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
	#tag EndViewBehavior
End Class
#tag EndClass
