#tag Class
Protected Class HCalculatorClass
	#tag Method, Flags = &h0
		Sub AddTermsAndDerivs(DWhich As DerivativeSet, VPower As Integer)
		  // Get initial values for the various variables
		  Var h As Double = DWhich.Value
		  Var ha As Double = DWhich.Dh0*H0
		  Var DhDV0 As Double = DWhich.DV0
		  Var DhDz As Double = DWhich.Dz
		  Var DhDβ As Double = DWhich.Dβ
		  Var DhDδ As Double = DWhich.Dδ
		  Var DhDΘ As Double = DWhich.DΘ
		  Var DhDΦ As Double = DWhich.DΦ
		  Var DhDλ0 As Double = DWhich.Dλ0
		  Var DhDχ10x As Double = DWhich.Dχ10x
		  Var DhDχ10y As Double = DWhich.Dχ10y
		  Var DhDχ10z As Double = DWhich.Dχ10z
		  Var DhDχ20x As Double = DWhich.Dχ20x
		  Var DhDχ20y As Double = DWhich.Dχ20y
		  Var DhDχ20z As Double = DWhich.Dχ20z
		  Var Vterm As Double = 1.0
		  Select Case VPower
		  Case 0
		    VTerm = H0V2
		  Case 1
		    VTerm = H0V2*V
		  Case 2
		    VTerm = H0V2*V2
		  Case 3
		    VTerm = H0V2*V3
		  End Select
		  
		  For k As Integer = 1 to NTerms
		    // The simple wave term
		    Var snratio As Double = SnR(Abs(NΨ(k)))
		    Var wave As Double = WaveFor(NCvS(k), Nα(k), NΨ(k)+5)
		    Var amp As Double = VTerm*A(0,k)
		    h = h + amp*wave
		    
		    // from now on, we modify terms by this ratio, because
		    // we may be able to hear some terms better than others above the noise
		    amp = amp*snratio 
		    ha = ha + amp*wave
		    
		    // β Derivative
		    Var dAkDβ As Double = snratio*Vterm*(A(IndexForβ+1,k) - A(IndexForβ,k))*Inverse2εβ
		    DhDβ = DhDβ + dAkDβ*wave
		    
		    // α and Ψr Derivatives
		    Var dWave As Double
		    If NCvS(k) = 0 Then
		      dWave = -WaveFor(1, Nα(k), NΨ(k)+5)
		    Else
		      dWave = WaveFor(0, Nα(k), NΨ(k)+5)
		    End If
		    Var dhkDα As Double = amp*DWave*Nα(k)
		    Var dhkDΨr As Double = amp*DWave*NΨ(k)
		    
		    // Θ, Φ, and λ0 derivatives
		    DhDΘ = DhDΘ + dhkDΨr*Derivs.DΨrDΘ
		    DhDΦ = DhDΦ + dhkDΨr*Derivs.DΨrDΦ
		    DhDλ0 = DhDλ0 + dhkDΨr
		    
		    // δ Derivative
		    Var dAkDδ As Double = VTerm*(A(IndexForδ+1,k) - A(IndexForδ,k))*Inverse2εδ*snratio
		    Var dAkDι As Double = VTerm*(A(IndexForι+1,k) - A(IndexForι,k))*Inverse2ει*snratio
		    Var dAkDχax As Double = VTerm*(A(IndexForχax+1,k) - A(IndexForχax,k))*Inverse2εχax*snratio
		    Var dAkDχay As Double = VTerm*(A(IndexForχay+1,k) - A(IndexForχay,k))*Inverse2εχay*snratio
		    Var dAkDχaz As Double = VTerm*(A(IndexForχaz+1,k) - A(IndexForχaz,k))*Inverse2εχaz*snratio
		    Var dAkDχsx As Double = VTerm*(A(IndexForχsx+1,k) - A(IndexForχsx,k))*Inverse2εχsx*snratio
		    Var dAkDχsy As Double = VTerm*(A(IndexForχsy+1,k) - A(IndexForχsy,k))*Inverse2εχsy*snratio
		    Var dAkDχsz As Double = VTerm*(A(IndexForχsz+1,k) - A(IndexForχsz,k))*Inverse2εχsz*snratio
		    Var dhkDV As Double = (amp/V)*(VPower+2)*wave
		    dAkDδ = dAkDδ + dAkDι*Derivs.DιDδ + dAkDχax*Derivs.DχaxDδ + dAkDχay*Derivs.DχayDδ _
		    + dAkDχaz*Derivs.DχazDδ + dAkDχsx*Derivs.DχsxDδ + dAkDχsy*Derivs.DχsyDδ + dAkDχsz*Derivs.DχszDδ
		    DhDδ = DhDδ + dhkDV*Derivs.DvDδ + dAkDδ*wave + dhkDα*Derivs.DαDδ + dhkDΨr*Derivs.DΨrDδ
		    
		    // z Derivative
		    Var dAkDz As Double = dAkDι*Derivs.DιDZ + dAkDχax*Derivs.DχaxDZ + dAkDχay*Derivs.DχayDZ _
		    + dAkDχaz*Derivs.DχazDZ + dAkDχsx*Derivs.DχsxDZ + dAkDχsy*Derivs.DχsyDZ + dAkDχsz*Derivs.DχszDZ
		    DhDZ = DhDZ + dhkDV*Derivs.DvDZ + dAkDz*wave + dhkDα*Derivs.DαDZ + dhkDΨr*Derivs.DΨrDZ
		    
		    // V0 Derivative
		    Var dAkDV0 As Double = dAkDι*Derivs.DιDV0 + dAkDχax*Derivs.DχaxDV0 + dAkDχay*Derivs.DχayDV0 _
		    + dAkDχaz*Derivs.DχazDV0 + dAkDχsx*Derivs.DχsxDV0 + dAkDχsy*Derivs.DχsyDV0 + dAkDχsz*Derivs.DχszDV0
		    DhDV0 = DhDV0 + dhkDV*Derivs.DvDV0 + dAkDV0*wave + dhkDα*Derivs.DαDV0 + dhkDΨr*Derivs.DΨrDV0
		    
		    // χ10x Derivative
		    Var dAkDχ10x As Double = dAkDι*Derivs.DιDχ10x + dAkDχax*Derivs.DχaxDχ10x + dAkDχay*Derivs.DχayDχ10x _
		    + dAkDχaz*Derivs.DχazDχ10x + dAkDχsx*Derivs.DχsxDχ10x + dAkDχsy*Derivs.DχsyDχ10x + dAkDχsz*Derivs.DχszDχ10x
		    DhDχ10x = DhDχ10x + dhkDV*Derivs.DvDχ10x + dAkDχ10x*wave + dhkDα*Derivs.DαDχ10x + dhkDΨr*Derivs.DΨrDχ10x
		    
		    // χ10y Derivative
		    Var dAkDχ10y As Double = dAkDι*Derivs.DιDχ10y + dAkDχax*Derivs.DχaxDχ10y + dAkDχay*Derivs.DχayDχ10y _
		    + dAkDχaz*Derivs.DχazDχ10y + dAkDχsx*Derivs.DχsxDχ10y + dAkDχsy*Derivs.DχsyDχ10y + dAkDχsz*Derivs.DχszDχ10y
		    DhDχ10y = DhDχ10y + dhkDV*Derivs.DvDχ10y + dAkDχ10y*wave + dhkDα*Derivs.DαDχ10y + dhkDΨr*Derivs.DΨrDχ10y
		    
		    // χ10z Derivative
		    Var dAkDχ10z As Double = dAkDι*Derivs.DιDχ10z + dAkDχax*Derivs.DχaxDχ10z + dAkDχay*Derivs.DχayDχ10z _
		    + dAkDχaz*Derivs.DχazDχ10z + dAkDχsx*Derivs.DχsxDχ10z + dAkDχsy*Derivs.DχsyDχ10z + dAkDχsz*Derivs.DχszDχ10z
		    DhDχ10z = DhDχ10z + dhkDV*Derivs.DvDχ10z + dAkDχ10z*wave + dhkDα*Derivs.DαDχ10z + dhkDΨr*Derivs.DΨrDχ10z
		    
		    // χ20x Derivative
		    Var dAkDχ20x As Double = dAkDι*Derivs.DιDχ20x + dAkDχax*Derivs.DχaxDχ20x + dAkDχay*Derivs.DχayDχ20x _
		    + dAkDχaz*Derivs.DχazDχ20x + dAkDχsx*Derivs.DχsxDχ20x + dAkDχsy*Derivs.DχsyDχ20x + dAkDχsz*Derivs.DχszDχ20x
		    DhDχ20x = DhDχ20x + dhkDV*Derivs.DvDχ20x + dAkDχ20x*wave + dhkDα*Derivs.DαDχ20x + dhkDΨr*Derivs.DΨrDχ20x
		    
		    // χ20y Derivative
		    Var dAkDχ20y As Double = dAkDι*Derivs.DιDχ20y + dAkDχax*Derivs.DχaxDχ20y + dAkDχay*Derivs.DχayDχ20y _
		    + dAkDχaz*Derivs.DχazDχ20y + dAkDχsx*Derivs.DχsxDχ20y + dAkDχsy*Derivs.DχsyDχ20y + dAkDχsz*Derivs.DχszDχ20y
		    DhDχ20y = DhDχ20y + dhkDV*Derivs.DvDχ20y + dAkDχ20y*wave + dhkDα*Derivs.DαDχ20y + dhkDΨr*Derivs.DΨrDχ20y
		    
		    // χ20z Derivative
		    Var dAkDχ20z As Double = dAkDι*Derivs.DιDχ20z + dAkDχax*Derivs.DχaxDχ20z + dAkDχay*Derivs.DχayDχ20z _
		    + dAkDχaz*Derivs.DχazDχ20z + dAkDχsx*Derivs.DχsxDχ20z + dAkDχsy*Derivs.DχsyDχ20z + dAkDχsz*Derivs.DχszDχ20z
		    DhDχ20z = DhDχ20z + dhkDV*Derivs.DvDχ20z + dAkDχ20z*wave + dhkDα*Derivs.DαDχ20z + dhkDΨr*Derivs.DΨrDχ20z
		  Next
		  
		  // Finally, add everything back into the record
		  DWhich.Value = h
		  DWhich.Dh0 = ha/H0
		  DWhich.DV0 = DhDV0
		  DWhich.Dz = DhDz
		  DWhich.Dβ = DhDβ
		  DWhich.Dδ = DhDδ
		  DWhich.DΘ = DhDΘ
		  DWhich.DΦ = DhDΦ
		  DWhich.Dλ0 = DhDλ0
		  DWhich.Dχ10x = DhDχ10x
		  DWhich.Dχ10y = DhDχ10y
		  DWhich.Dχ10z = DhDχ10z
		  DWhich.Dχ20x = DhDχ20x
		  DWhich.Dχ20y = DhDχ20y
		  DWhich.Dχ20z = DhDχ20z
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalcTotalHAndDerivs(theATA As Matrix)
		  // Set up some useful local values so that we don't need to
		  // calculate them multiple times
		  Var ρ As Double = Parameters.GMΩe*Values.τr
		  Var twoρ As Double = 2*ρ
		  Var threeρ As Double = 3*ρ
		  Var fourρ As Double = 4*ρ
		  Var Φ As Double = Parameters.Φ
		  
		  // Set up the sine and cosines for functions for detector 1
		  Var arg22 As Double = twoρ-DC2σ1
		  Var arg422 As Double = fourρ-DC2σ1-DC2Φ
		  Var arg321 As Double = threeρ-DC2σ1-Φ
		  Var arg121 As Double = ρ-DC2σ1+Φ
		  Var sin22 As Double = Sin(arg22)
		  Var sin422 As Double = Sin(arg422)
		  Var sin321 As Double = Sin(arg321)
		  Var sin121 As Double = Sin(arg121)
		  Var cos22 As Double = Cos(arg22)
		  Var cos422 As Double = Cos(arg422)
		  Var cos321 As Double = Cos(arg321)
		  Var cos121 As Double = Cos(arg121)
		  
		  // Calculate the D+ factor and its derivative with respect to Θ
		  Var term1 As Double = DC3*(-6.0*sin22 + DCSinσ1x9- sin422)
		  Var term2 As Double = DC1*(18.0*sin22 + DCSinσ1x9+ sin422)
		  Var term3 As Double = -DC2*(sin321 - 3.0*sin121)
		  Var dPlus1 As Double = term1 + DCCos2Θ*term2 + DCSin2Θ*term3
		  Var dDp1DΘ As Double = 2*(-DCSin2Θ*term2 + DCCos2Θ*term3)
		  // Calculate the derivative of D+ with respect to Φ
		  term1 = -2*DC3*(DCCosσ1x9 - cos422)
		  term2 = Term1/3.0
		  term3 = DC2*(cos321 + 3.0*cos121)
		  Var dDp1DΦ As Double = term1 + DCCos2Θ*term2 + DCSin2Θ*term3
		  // Calculate the Dx factor and its derivative with respect to Θ
		  term1 = 4*DC1*(DCCosσ1x9 - cos422)
		  term2 = DC2*(cos321 - 3*cos121)
		  Var dCross1 As Double = DCCosΘ*term1 + DCSinΘ*term2
		  Var dDx1DΘ As Double = -DCSinΘ*term1 + DCCosΘ*term2
		  // Calculate the derivative of Dx with respect to Φ
		  term1 = -8*DC1*(DCSinσ1x9- sin422)
		  term2 = -DC2*(sin321 + 3*sin121)
		  Var dDx1DΦ As Double = DCCosΘ*term1 + DCSinΘ*term2
		  // Finally, Calculate the F+ and Fx factors for Detector 1
		  Var fPlus1 As Double = DCHalfCos2ψ*DPlus1 - DCHalfSin2ψ*DCross1
		  Var fCross1 As Double = DCHalfSin2ψ*DPlus1 + DCHalfCos2ψ*DCross1
		  
		  // Now repeat the whole thing with σ2 replacing σ1
		  arg22 = twoρ-DC2σ2
		  arg422 = fourρ-DC2σ2-DC2Φ
		  arg321 = threeρ-DC2σ2-Φ
		  arg121 = ρ-DC2σ2+Φ
		  sin22 = Sin(arg22)
		  sin422 = Sin(arg422)
		  sin321 = Sin(arg321)
		  sin121 = Sin(arg121)
		  cos22 = Cos(arg22)
		  cos422 = Cos(arg422)
		  cos321 = Cos(arg321)
		  cos121 = Cos(arg121)
		  // Calculate the D+ factor and its derivative with respect to Θ
		  term1 = DC3*(-6.0*sin22 + DCSinσ2x9 - sin422)
		  term2 = DC1*(18.0*sin22 + DCSinσ2x9 + sin422)
		  term3 = -DC2*(sin321 - 3.0*sin121)
		  Var dPlus2 As Double = term1 + DCCos2Θ*term2 + DCSin2Θ*term3
		  Var dDp2DΘ As Double = 2*(-DCSin2Θ*term2 + DCCos2Θ*term3)
		  // Calculate the derivative of D+ with respect to Φ
		  term1 = -2*DC3*(DCCosσ2x9 - cos422)
		  term2 = term1/3.0
		  term3 = DC2*(cos321 + 3.0*cos121)
		  Var dDp2DΦ As Double = term1 + DCCos2Θ*Term2 + DCSin2Θ*term3
		  // Calculate the Dx factor and its derivative with respect to Θ
		  term1 = 4*DC1*(DCCosσ2x9 - cos422)
		  term2 = DC2*(cos321 - 3*cos121)
		  Var dCross2 As Double = DCCosΘ*term1 + DCSinΘ*term2
		  Var dDx2DΘ As Double = -DCSinΘ*term1 + DCCosΘ*term2
		  // Calculate the derivative of Dx with respect to Φ
		  term1 = -8*DC1*(DCSinσ2x9 - sin422)
		  term2 = -DC2*(sin321 + 3*sin121)
		  Var  dDx2DΦ As Double = DCCosΘ*term1 + DCSinΘ*term2
		  // Finally, Calculate the F+ and Fx factors for Detector 1
		  Var fPlus2 As Double = DCHalfCos2ψ*dPlus2 - DCHalfSin2ψ*dCross2
		  Var fCross2 As Double = DCHalfSin2ψ*dPlus2 + DCHalfCos2ψ*dCross2
		  
		  // This will calculate the total signal H plus all its derivatives
		  Var fPlus As Double = fPlus1 + fPlus2
		  Var fCross As Double = fCross1 + fCross2
		  Var hp As Double = DHP.Value
		  Var hx As Double = DHX.Value
		  Var hpa As Double= DHP.Dh0*H0
		  Var hxa As Double = DHP.Dh0*H0
		  DH.Value = fPlus*hp + fCross*hx
		  DH.Dh0 = fPlus*DHP.Dh0 + fCross*DHX.Dh0
		  DH.Dψ = 2*(-fCross*hpa + fPlus*hpa)
		  Var dFpDq As Double = DCHalfCos2ψ*(dDp1DΘ + dDp2DΘ) - DCHalfSin2ψ*(dDx1DΘ + dDx2DΘ)
		  Var dFxDq As Double = DCHalfSin2ψ*(dDp1DΘ + dDp2DΘ) + DCHalfCos2ψ*(dDx1DΘ + dDx2DΘ)
		  DH.DΘ = dFpDq*hpa + dFxDq*hxa + fPlus*DHP.DΘ + fCross*DHX.DΘ
		  DFpDq = DCHalfCos2ψ*(dDp1DΦ + dDp2DΦ ) - DCHalfSin2ψ*(dDx1DΦ  + dDx2DΦ )
		  DFxDq  = DCHalfSin2ψ*(dDp1DΦ  + dDp2DΦ ) + DCHalfCos2ψ*(dDx1DΦ  + dDx2DΦ)
		  DH.DΦ = dFpDq*hpa + dFxDq*hxa + fPlus*DHP.DΦ + fCross*DHX.DΦ
		  DH.Dβ = fPlus*DHP.Dβ + fCross*DHX.Dβ
		  DH.DV0 = fPlus*DHP.DV0 + fCross*DHX.DV0
		  DH.Dz = fPlus*DHP.DZ+ fCross*DHX.DZ
		  DH.Dδ = fPlus*DHP.Dδ+ fCross*DHX.Dδ
		  DH.Dλ0 = fPlus*DHP.Dλ0+ fCross*DHX.Dλ0
		  DH.Dχ10x = fPlus*DHP.Dχ10x+ fCross*DHX.Dχ10x
		  DH.Dχ10y = fPlus*DHP.Dχ10y+ fCross*DHX.Dχ10y
		  DH.Dχ10z = fPlus*DHP.Dχ10z+ fCross*DHX.Dχ10z
		  DH.Dχ20x = fPlus*DHP.Dχ20x+ fCross*DHX.Dχ20x
		  DH.Dχ20y = fPlus*DHP.Dχ20y+ fCross*DHX.Dχ20y
		  DH.Dχ20z = fPlus*DHP.Dχ20z+ fCross*DHX.Dχ20z
		  
		  // Load Derivatives into the ATAMatrix
		  Var InverseNoise As Double = 1.0/Sn2
		  // Load the derivatives into an array
		  DArray(0) = DH.Dh0*InverseNoise
		  DArray(1) = DH.Dδ*InverseNoise
		  DArray(2) = DH.DV0*InverseNoise
		  DArray(3) = DH.Dz*InverseNoise
		  DArray(4) = DH.Dβ*InverseNoise
		  DArray(5) = DH.Dψ*InverseNoise
		  DArray(6) = DH.Dλ0*InverseNoise
		  DArray(7) = DH.DΘ*InverseNoise
		  DArray(8) = DH.DΦ*InverseNoise
		  DArray(9) = DH.Dχ10x*InverseNoise
		  DArray(10) = DH.Dχ10y*InverseNoise
		  DArray(11) = DH.Dχ10z*InverseNoise
		  DArray(12) = DH.Dχ20x*InverseNoise
		  DArray(13) = DH.Dχ20y*InverseNoise
		  DArray(14) = DH.Dχ20z*InverseNoise
		  // Add all the derivatives into the ATA matrix
		  For j As Integer = 0 to 14
		    For k As Integer = 0 to 14
		      TheATA.pData(j,k) = TheATA.pData(j,k) + DArray(j)*DArray(k)
		    Next
		  Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Calculate(theValues As CurrentValuesClass, theDerivs As CurrentDerivativesClass, theATA As Matrix)
		  Values = TheValues
		  Derivs = TheDerivs
		  V = Values.V
		  V2 = V*V
		  V3 = V2*V
		  H0 = Parameters.H0
		  H0V2 = H0*V2
		  
		  // Get noise ratios
		  // This is the value of the observed orbital frequency in Hz
		  Var fN As Double =  V*V*V/(2*Parameters.π*Parameters.GM*(1.0 + Parameters.Z))
		  //  get the noise at various frequencies
		  Var n2 As Double = Noise.GetNoise(2*fN) // This is the noise at the fundamental gravitational wave frequency
		  // The following array contains ratios that we will use to enhance derivatives of harmonics at higher frequencies
		  // to reflect how they may be better or more poorly received by the detector than the fundamental harmonic
		  SnR(1 )= Sqrt(n2/Noise.GetNoise(fN))
		  SnR(2) = 1.0
		  SnR(3) = Sqrt(n2/Noise.GetNoise(3*fN))
		  SnR(4) = Sqrt(n2/Noise.GetNoise(4*fN))
		  SnR(5) = Sqrt(n2/Noise.GetNoise(5*fN))
		  SnR(6) = Sqrt(n2/Noise.GetNoise(6*fN))
		  Sn2 = Sqrt(n2)  // This is the basic square root of noise at the current fundamental gravitational wave frequency
		  
		  // Set up all the wavefunctions
		  UpdateSinCosArrays
		  
		  // Set up some local variables for quantities we will refer to often
		  Var ι As Double = Values.ι
		  Var α As Double = Values.α
		  Var Ψr As Double = Values.Ψr
		  Var χa As Vector = Values.χa
		  Var χs As Vector = Values.χs
		  
		  // Update all sets of amplitude parameters
		  For i As Integer = 0 to 18
		    APSet(i).Update(ι, α, Ψr, χa, χs)
		  Next
		  
		  // Do all the cases for all the terms
		  DoAllCases
		  
		  // Calculate the final terms and add into the ATA
		  CalcTotalHAndDerivs(theATA)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(CaseParameters As CaseParametersClass)
		  Parameters  = CaseParameters  // Store a reference to the case's parameters
		  DH = New DerivativeSet  // Initialize derivative sets
		  DHP = New DerivativeSet
		  DHX = New DerivativeSet
		  Noise = New NoiseClass(Parameters.ΔT)
		  
		  // Initialize amplitude parameter sets
		  Var ει As Double = 1.0e-6   // These need not all be the same
		  Var εβ As Double = 1.0e-6
		  Var εδ As Double = 1.0e-6
		  Var εχax As Double = 1.0e-6
		  Var εχay As Double = 1.0e-6
		  Var εχaz As Double = 1.0e-6
		  Var εχsx As Double = 1.0e-6
		  Var εχsy As Double = 1.0e-6
		  Var εχsz As Double = 1.0e-6
		  
		  // Initialize denominators for derivatives
		  Inverse2ει = 0.5/ει
		  Inverse2εβ = 0.5/εβ
		  Inverse2εδ = 0.5/εδ
		  Inverse2εχax = 0.5/εχax
		  Inverse2εχay = 0.5/εχay
		  Inverse2εχaz = 0.5/εχaz
		  Inverse2εχsx = 0.5/εχsx
		  Inverse2εχsy = 0.5/εχsy
		  Inverse2εχsz = 0.5/εχsz
		  
		  // Initialize indexes
		  IndexForι = 1
		  IndexForβ = 3
		  IndexForδ = 5
		  IndexForχax = 7
		  IndexForχay = 9
		  IndexForχaz = 11
		  IndexForχsx = 13
		  IndexForχsy = 15
		  IndexForχsz = 17
		  
		  // Initialize the amplitude parameters for each case
		  // This is the base case
		  APSet(0) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.None, 0.0)
		  // These are the ι side cases, minus and plus
		  APSet(IndexForι) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.ι, -ει)
		  APSet(IndexForι+1) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.ι, ει)
		  // These are the β side cases, minus and plus
		  APSet(IndexForβ) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.β, -εβ)
		  APSet(IndexForβ+1) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.β, εβ)
		  // These are the δ side cases, minus and plus
		  APSet(IndexForδ) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.δ, -εδ)
		  APSet(IndexForδ+1) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.δ, εδ)
		  // These are the χax side cases, minus and plus
		  APSet(IndexForχax) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χax, -εχax)
		  APSet(IndexForχax+1) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χax, εχax)
		  // These are the χay side cases, minus and plus
		  APSet(IndexForχay) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χay, -εχay)
		  APSet(IndexForχay+1) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χay, εχay)
		  // These are the χaz side cases, minus and plus
		  APSet(IndexForχaz) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χaz, -εχaz)
		  APSet(IndexForχaz+1) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χaz, εχaz)
		  // These are the χsx side cases, minus and plus
		  APSet(IndexForχsx) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χsx, -εχsx)
		  APSet(IndexForχsx+1) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χsx, εχsx)
		  // These are the χsy side cases, minus and plus
		  APSet(IndexForχsy) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χsy, -εχsy)
		  APSet(IndexForχsy+1) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χsy, εχsy)
		  // These are the χsz side cases, minus and plus
		  APSet(IndexForχsz) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χsz, -εχsz)
		  APSet(IndexForχsz+1) = New AmplitudeParameters(CaseParameters, AmplitudeParameters.Item.χsz, εχsz)
		  
		  // Initialize constants for the detector functions
		  DCCosΘ = Cos(Parameters.Θ)
		  DCCos2Θ = Cos(2*Parameters.Θ)
		  DCSinΘ = Sin(Parameters.Θ)
		  DCSin2Θ = Sin(2*Parameters.Θ)
		  DC2Φ = 2*Parameters.Φ
		  DC2σ1 = 2*(0.75*Parameters.π + Parameters.ρ0)
		  DC2σ2 = DC2σ1 + 4*Parameters.π/3.0
		  DCSinσ1x9= 9*Sin(DC2σ1-2*Parameters.Φ)
		  DCSinσ1x9= 9*Sin(DC2σ2-2*Parameters.Φ)
		  DCHalfSin2ψ = 0.5*Sin(2*Parameters.ψ)
		  DCHalfCos2ψ = 0.5*Cos(2*Parameters.ψ)
		  Var r3 As Double = Sqrt(3)
		  DC1 = r3/64
		  DC2 = 3/16
		  DC3 = 3*r3/64
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoAllCases()
		  For i As Integer = 0 to 18
		    HP0DoCase(i)
		  Next
		  AddTermsAndDerivs(DHP, 0)
		  For i As Integer = 0 to 18
		    HX0DoCase(i)
		  Next
		  AddTermsAndDerivs(DHX, 0)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HP0DoCase(NCase As Integer)
		  Var AP As AmplitudeParameters = APSet(NCase)
		  Var C1 As Double = AP.C1
		  Var C1p3 As Double = C1*C1*C1
		  Var C1p4 As Double = C1p3*C1
		  Var S1 As Double = AP.S1
		  Var S1p3 As Double = S1*S1*S1
		  Var S1p4 As Double = S1p3*S1
		  Var C2β As Double = AP.C2β
		  Var S2β As Double = AP.S2β
		  Var Sβp2 As Double = AP.Sβ*AP.Sβ
		  Var S2p2 As Double = AP.S2*AP.S2
		  
		  // Set up the amplitude array
		  A(NCase,1) = (-1.5-0.5*C2β)*C1p4
		  A(NCase,2) = -2*S2β*C1p3*S1
		  A(NCase,3) =  2.0*S1*S1*S1*S2β*C1
		  A(NCase,4) = (-1.5-0.5*C2β)*S1p4
		  A(NCase,5) = -1.5*Sβp2*S2p2
		  NTerms = 5 // this is how many terms we have
		  
		  If NCase = 0 Then // if we are doing the center case
		    // Set up the α array
		    Nα(1) = 2
		    Nα(2) = 1
		    Nα(3) = 1
		    Nα(4) = 2
		    Nα(5) = 0
		    
		    // Set up the Ψr array
		    NΨ(1) = 2
		    NΨ(2) = 2
		    NΨ(3) = -2
		    NΨ(4) = -2
		    NΨ(5) = 2
		    
		    // Set up the Cosine Versus Sine Array
		    // (All wave terms here are cosine)
		    NCvS(1) = 0
		    NCvS(2) = 0
		    NCvS(3) = 0
		    NCvS(4) = 0
		    NCvS(5) = 0
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HP1DoCase(NCase As Integer)
		  'AddTerm(AddressOf GetA1, 3, 3, False)
		  'AddTerm(AddressOf GetA2, 1, 1, False)
		  'AddTerm(AddressOf GetA3, 1, -1, False)
		  'AddTerm(AddressOf GetA4, 3,1, False)
		  'AddTerm(AddressOf GetA5, 1, 3, False)
		  'AddTerm(AddressOf GetA6, 1, -3, False)
		  'AddTerm(AddressOf GetA7, 3, -1, False)
		  'AddTerm(AddressOf GetA8, 3, -3, False)
		  'AddTerm(AddressOf GetA9, 0, 3, False)
		  'AddTerm(AddressOf GetA10,2, 1, False)
		  'AddTerm(AddressOf GetA11, 2, 3, False)
		  'AddTerm(AddressOf GetA12, 2, -1, False)
		  'AddTerm(AddressOf GetA13, 2, -3, False)
		  'AddTerm(AddressOf GetA14, 0, 1, False)
		  '
		  '1 return Parameters.δ*(AP.C1^6)*(-45/32*AP.Sβ-9/32*AP.S3β)
		  '2 return Parameters.δ*(AP.C1^2)*(-175/256*AP.Sβ+AP.C2*(87/64*AP.Sβ-5/64*AP.S3β)+AP.C4*(-5/256*AP.Sβ+15/256*AP.S3β)+13/256*AP.S3β)
		  '3 return Parameters.δ*(AP.S1^2)*(175/256*AP.Sβ+AP.C2*(87/64*AP.Sβ-5/64*AP.S3β)+AP.C4*(5/256*AP.Sβ-15/256*AP.S3β)-13/256*AP.S3β)
		  '4 return Parameters.δ*(AP.C1^4)*(AP.S1^2)*(-5/32*AP.Sβ-AP.S3β/32)
		  '5 return Parameters.δ*(AP.C1^4)*(AP.S1^2)*(-45/32*AP.Sβ+AP.S3β*135/32)
		  '6 return Parameters.δ*(AP.C1^2)*(AP.S1^4)*(45/32*AP.Sβ-AP.S3β*135/32)
		  '7 return Parameters.δ*(AP.C1^2)*(AP.S1^4)*(5/32*AP.Sβ+AP.S3β/32)
		  '8 return Parameters.δ*(AP.S1^6)*AP.Sβ*(27/16+9/16*AP.C2β)
		  '9 return Parameters.δ*(45/16)*(AP.S2^3)*(AP.Cβ)*(AP.Sβ^2)
		  '10 return Parameters.δ*((-85/256*AP.Cβ-AP.Cβ*AP.C2β/128-AP.Cβ*AP.C2β*AP.C2/32-3/128*AP.Cβ*AP.C2β*AP.C4)*AP.S2-11/64*AP.Cβ*AP.S4-AP.Cβ*AP.S6/256)
		  '11 return Parameters.δ*((45/256*AP.Cβ+AP.Cβ*AP.C2β*81/128+AP.Cβ*AP.C2β*AP.C2*27/32+27/128*AP.Cβ*AP.C2β*AP.C4)*AP.S2+9/64*AP.Cβ*AP.S4+AP.Cβ*AP.S6*9/256)
		  '12 return Parameters.δ*((-85/256*AP.Cβ+AP.Cβ*AP.C2β*1/256)*AP.S2+(11/64*AP.Cβ+1/64*AP.Cβ*AP.C2β)*AP.S4-(1/256*AP.Cβ+3/256*AP.Cβ*AP.C2β)*AP.S6)
		  '13 return Parameters.δ*((45/256*AP.Cβ+AP.Cβ*AP.C2β*135/256)*AP.S2-(9/64*AP.Cβ+27/64*AP.Cβ*AP.C2β)*AP.S4+(9/256*AP.Cβ+27/256*AP.Cβ*AP.C2β)*AP.S6)
		  '14 return Parameters.δ*(1/64*AP.Cβ*AP.Sβ^2*AP.S2+5/64*AP.Cβ*AP.Sβ*AP.S6)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HP2DoCase(NCase As Integer)
		  
		  'AddTerm(AddressOf GetA1, 2, 2, False)
		  'AddTerm(AddressOf GetA2, 4, 4, False)
		  'AddTerm(AddressOf GetA3, 3, 4, False)
		  'AddTerm(AddressOf GetA4, 3, 2, False)
		  'AddTerm(AddressOf GetA5, 2, 4, False)
		  'AddTerm(AddressOf GetA6, 4, 2, False)
		  'AddTerm(AddressOf GetA7, 1, 4, False)
		  'AddTerm(AddressOf GetA8, 1, -2, False)
		  'AddTerm(AddressOf GetA9, 2, -2, False)
		  'AddTerm(AddressOf GetA10, 1, -4, False)
		  'AddTerm(AddressOf GetA11, 3, -2, False)
		  'AddTerm(AddressOf GetA12, 2, -4, False)
		  'AddTerm(AddressOf GetA13, 4, -2, False)
		  'AddTerm(AddressOf GetA14, 3, -4, False)
		  'AddTerm(AddressOf GetA15, 4, -4, False)
		  'AddTerm(AddressOf GetA16, 0, 2, False)
		  'AddTerm(AddressOf GetA17, 0, 4, False)
		  'AddTerm(AddressOf GetA18, 1, 2, False)
		  '
		  '1 return (59/16+5/2*AP.C2β-3/16*AP.C4β+(5/24-11/6*AP.C2β+7/24*AP.C4β)*AP.C2-(5/48+1/12*AP.C2β+7/48*AP.C4β)*AP.C4)*AP.C1^4+(-25/16-13/3*AP.C2β+9/16*AP.C4β+(-5/8+11/2*AP.C2β-7/8*AP.C4β)*AP.C2+(5/16+1/4*AP.C2β+7/16*AP.C4β)*AP.C4)*AP.η*AP.C1^4
		  '2 return (6+2*AP.C2β)*AP.η*AP.C1^8*AP.Sβ^2-(2+2/3*AP.C2β)*AP.C1^8*AP.Sβ^2
		  '3 return 32*(1/3-AP.η)*AP.Cβ^3*AP.C1^7*AP.Sβ*AP.S1
		  '4 return ((1/6*AP.C2β-5/6)*AP.S2β-2/3*AP.Cβ^2*AP.C2*AP.S2β+AP.η*((5/2-1/2*AP.C2β)*AP.S2β+2*AP.Cβ^2*AP.C2*AP.S2β))*AP.C1^5*AP.S1
		  '5 return (-(10/3+8/3*AP.C2β+14/3*AP.C4β)+AP.η*(10+8*AP.C2β+14*AP.C4β))*AP.C1^6*AP.S1^2
		  '6 return 1/2*(-(1+1/3*AP.C2β)+AP.η*(3+AP.C2β))*AP.C1^6*AP.Sβ^2*AP.S1^2
		  '7 return (8/3-56/3*AP.C2β+AP.η*(56*AP.C2β-8))*AP.C1^5*AP.S2β*AP.S1^3
		  '8 return AP.η*(AP.C1*(16/3*AP.S2β+31/4*AP.C2*AP.S2β+1/4*AP.C4*AP.S2β-19/16*AP.S4β)-7/8*AP.C3*AP.S4β-7/16*AP.C5*AP.S4β)*AP.S1^3+(AP.C1*(-6*AP.S2β-31/12*AP.C2*AP.S2β-1/12*AP.C4*AP.S2β+19/48*AP.S4β)+7/24*AP.C3*AP.S4β+7/48*AP.C5*AP.S4β)*AP.S1^3
		  '9 return (59/16+5/2*AP.C2β-3/16*AP.C4β-(5/24-11/6*AP.C2β+7/24*AP.C4β)*AP.C2-(5/48+1/12*AP.C2β+7/48*AP.C4β)*AP.C4)*AP.S1^4+AP.η*(-25/16-13/3*AP.C2β+9/16*AP.C4β+(5/8-11/2*AP.C2β+7/8*AP.C4β)*AP.C2+(5/16+1/4*AP.C2β+7/16*AP.C4β)*AP.C4)*AP.S1^4
		  '10 return (56/3*AP.C2β-8/3+AP.η*(8-56*AP.C2β))*AP.C1^3*AP.S2β*AP.S1^5
		  '11 return ((5/6-1/6*AP.C2β)*AP.S2β-2/3*AP.Cβ^2*AP.C2*AP.S2β+AP.η*((-5/2+1/2*AP.C2β)*AP.S2β+2*AP.Cβ^2*AP.C2*AP.S2β))*AP.C1*AP.S1^5
		  '12 return (-(10/3+8/3*AP.C2β+14/3*AP.C4β)+AP.η*(10+8*AP.C2β+14*AP.C4β))*AP.C1^2*AP.S1^6
		  '13 return (-(1/2+1/6*AP.C2β)+AP.η*(3/2+1/2*AP.C2β))*AP.C1^2*AP.Sβ^2*AP.S1^6
		  '14 return 32*(AP.η-1/3)*AP.Cβ^3*AP.C1*AP.Sβ*AP.S1^7
		  '15 return (AP.η*(6+2*AP.C2β)-(2+2/3*AP.C2β))*AP.Sβ^2*AP.S1^8
		  '16 return 1/32*(1/3*(349-25*AP.C2β)*AP.Sβ^2-(25+35*AP.C2β)*AP.C4*AP.Sβ^2)+AP.η*((25*AP.C2β-45)*AP.Sβ^2+(25+35*AP.C2β)*AP.C4*AP.Sβ^2)*AP.S2^2
		  '17 return 1/4*(AP.η*(25+35*AP.C2β)-1/3*(25-35*AP.C2β))*AP.Sβ^2*AP.S2^4
		  '18 return AP.C1^3*(6*AP.S2β-31/12*AP.C2*AP.S2β+1/12*AP.C4*AP.S2β-19/48*AP.S4β)*AP.S1+7/24*AP.C1^3*AP.S4β*AP.S3-7/48*AP.C1^3*AP.S4β*AP.S5+AP.η*(AP.C1^3*(-16/3*AP.S2β+31/4*AP.C2*AP.S2β-1/4*AP.C4*AP.S2β+19/16*AP.S4β)*AP.S1-7/8*AP.C1^3*AP.S4β*AP.S3+7/16*AP.C1^3*AP.S4β*AP.S5)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HP2SODoCase(NCase As Integer)
		  'AddTerm(AddressOf GetA1, 1, 1, False)
		  'AddTerm(AddressOf GetA2, 1, -1, False)
		  'AddTerm(AddressOf GetA3, 1, -1, True)
		  'AddTerm(AddressOf GetA4, 0,1, True)
		  'AddTerm(AddressOf GetA5, 1, 1, True)
		  'AddTerm(AddressOf GetA6, 1, 1, False)
		  'AddTerm(AddressOf GetA7, 1, -1, False)
		  'AddTerm(AddressOf GetA8, 1, -1, True)
		  'AddTerm(AddressOf GetA9, 0, 1, True)
		  'AddTerm(AddressOf GetA10,1, 1, True)
		  '
		  '1 Return AP.χax*AP.Cβ*AP.C1^2-AP.χaz*AP.C1^2*AP.Sβ
		  '2 Return AP.χax*(AP.Cβ/2-AP.Cβ*AP.C2/2)-AP.χaz*AP.Sβ*AP.S1^2
		  '3 Return -AP.χay*AP.Cβ*AP.S1^2
		  '4 Return -AP.χay*AP.Sβ*AP.S2
		  '5 Return -AP.χay*AP.Cβ*AP.C1^2
		  '6 Return Parameters.δ*(AP.χsx*AP.Cβ*AP.C1^2-AP.χsz*AP.C1^2*AP.Sβ)
		  '7 Return Parameters.δ*(AP.χsx*(AP.Cβ/2-AP.Cβ*AP.C2/2)-AP.χsz*AP.Sβ*AP.S1^2)
		  '8 Return -Parameters.δ*(AP.χsy*AP.Cβ*AP.S1^2)
		  '9 Return Parameters.δ*(AP.χsy*AP.Sβ*AP.S2)
		  '10 Return -Parameters.δ*(AP.χsy*AP.Cβ*AP.S1^2)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HP3DoCase(NCase As Integer)
		  '
		  'AddTerm(AddressOf GetA1, 2, 2, False)
		  'AddTerm(AddressOf GetA2, 1, 2, False)
		  'AddTerm(AddressOf GetA3, 1, -2, False)
		  'AddTerm(AddressOf GetA4, 2, -2, False)
		  'AddTerm(AddressOf GetA5, 0, 2, False)
		  'AddTerm(AddressOf GetA6, 5, 5, False)
		  'AddTerm(AddressOf GetA7, 1, 1, False)
		  'AddTerm(AddressOf GetA8, 3, 3, False)
		  'AddTerm(AddressOf GetA9, 4, 5, False)
		  'AddTerm(AddressOf GetA10,4, 3, False)
		  'AddTerm(AddressOf GetA11, 5, 3, False)
		  'AddTerm(AddressOf GetA12, 1, -1, False)
		  'AddTerm(AddressOf GetA13, 3, 1, False)
		  'AddTerm(AddressOf GetA14, 3, 5, False)
		  'AddTerm(AddressOf GetA15, 1, 3, False)
		  'AddTerm(AddressOf GetA16, 2, 5, False)
		  'AddTerm(AddressOf GetA17, 4, 1, False)
		  'AddTerm(AddressOf GetA18, 5, 1, False)
		  'AddTerm(AddressOf GetA19, 3, -1, False)
		  'AddTerm(AddressOf GetA20, 1, 5, False)
		  'AddTerm(AddressOf GetA21, 1, -3, False)
		  'AddTerm(AddressOf GetA22, 4, -1, False)
		  'AddTerm(AddressOf GetA23, 5, -1, False)
		  'AddTerm(AddressOf GetA24, 3, -3, False)
		  'AddTerm(AddressOf GetA25, 1, -5, False)
		  'AddTerm(AddressOf GetA26, 2, -5, False)
		  'AddTerm(AddressOf GetA27, 4, -3, False)
		  'AddTerm(AddressOf GetA28, 5, -3, False)
		  'AddTerm(AddressOf GetA29, 3, -5, False)
		  'AddTerm(AddressOf GetA30, 4, -5, False)
		  'AddTerm(AddressOf GetA31, 5, -5, False)
		  'AddTerm(AddressOf GetA32, 0, 3, False)
		  'AddTerm(AddressOf GetA33, 0, 5, False)
		  'AddTerm(AddressOf GetA34, 2, 3, False)
		  'AddTerm(AddressOf GetA35, 2, -3, False)
		  'AddTerm(AddressOf GetA36, 2, 1, False)
		  'AddTerm(AddressOf GetA37, 2, -1, False)
		  'AddTerm(AddressOf GetA38, 0, 1, False)
		  '
		  '1 Return -(3*π+π*AP.C2β)*AP.C1^4
		  '2 Return -4*π*AP.C1^3*AP.S2β*AP.S1
		  '3 Return 4*π*AP.C1*AP.S2β*AP.S1^3
		  '4 Return -(3*π+π*AP.C2β)*AP.S1^4
		  '5 Return -3*π*AP.Sβ^2*AP.S2^2
		  '6 Return Parameters.δ*(AP.η*(625/128+625/384*AP.C2β)-(625/256+625/768*AP.C2β))*AP.C1^10*AP.Sβ^3
		  '7 Return Parameters.δ*(AP.η*AP.C1^2*(-7449/16384*AP.Sβ-331/32768*AP.S3β+AP.C4*(337/12288*AP.Sβ-47/8192*AP.S3β-21/8192*AP.S5β)+AP.C8*(7/49152*AP.Sβ+7/32768*AP.S3β-35/32768*AP.S5β)+AP.C6*(-59/6144*AP.Cβ-91/4096*AP.S3β+7/4096*AP.S5β)+AP.C2*(1873/2048*AP.Sβ+19/4096*AP.S3β+35/12288*AP.S5β)-155/98304*AP.S5β)+AP.C1^2*(43723/98304*AP.Sβ-9653/65536*AP.S3β+AP.C2*(-10675/12288*AP.Sβ+1901/8192*AP.S3β-35/24576*AP.S5β)+AP.C6*(59/12288*AP.Sβ+91/8192*AP.S3β-7/8192*AP.S5β)+AP.C8*(-7/98304*AP.Sβ-7/65536*AP.S3β+35/65536*AP.S5β)+AP.C4*(1103/24576*AP.Sβ-2833/16384*AP.S3β+21/16384*AP.S5β)+155/196608*AP.S5β))
		  '8 Return Parameters.δ*(AP.C1^6*(39249/8192*AP.Sβ+38331/16384*AP.S3β-AP.C4*(1701/8192*AP.Sβ+3159/16384*AP.S3β+3645/16384*AP.S5β)+AP.C2*(2403/2048*AP.Sβ-6399/4096*AP.S3β+2187/4096*AP.S5β)-5751/16384*AP.S5β)+AP.η*AP.C1^6*(-4689/4096*AP.Sβ-24507/8192*AP.S3β+AP.C2*(-2403/1024*AP.Sβ+6399/2048*AP.S3β-2187/2048*AP.S5β)+AP.C4*(1701/4096*AP.Sβ+3159/8192*AP.S3β+3645/8192*AP.S5β)+5751/8192*AP.S5β))
		  '9 Return Parameters.δ*((11875/768*AP.Cβ+3125/768*AP.C3β-AP.η*(11875/384*AP.Cβ+3125/384*AP.C3β))*AP.C1^9*AP.Sβ^2*AP.S1)
		  '10 Return Parameters.δ*(((-351/256*AP.Cβ+243/256*AP.Cβ*AP.C2β)*AP.Sβ^2-(567/256*AP.Cβ+405/256*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2+AP.η*((351/128*AP.Cβ-243/128*AP.Cβ*AP.C2β)*AP.Sβ^2+(567/128*AP.Cβ+405/128*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2))*AP.C1^7*AP.S1)
		  '11 Return Parameters.δ*((AP.η*(243/128+81/128*AP.C2β)-(243/256+81/256*AP.C2β))*AP.C1^8*AP.Sβ^3*AP.S1^2)
		  '12 Return Parameters.δ*((-43723/98304*AP.Sβ+9653/65536*AP.S3β+AP.C2*(-10675/12288*AP.Sβ+1901/8192*AP.S3β-35/24576*AP.S5β)+AP.C4*(-1103/24576*AP.Sβ+2833/16384*AP.S3β-21/16384*AP.S5β)+AP.C6*(59/12288*AP.Sβ+91/8192*AP.S3β-7/8192*AP.S5β)+AP.C8*(7/98304*AP.Sβ+7/65536*AP.S3β-35/65536*AP.S5β)-155/196608*AP.S5β)*AP.S1^2+AP.η*(7449/16384*AP.Sβ+331/32768*AP.S3β+AP.C8*(-7/49152*AP.Sβ-7/32768*AP.S3β+35/32768*AP.S5β)+AP.C6*(-59/6144*AP.Sβ-91/4096*AP.S3β+7/4096*AP.S5β)+AP.C4*(-337/12288*AP.Sβ+47/8192*AP.S3β+21/8192*AP.S5β)+AP.C2*(1873/2048*AP.Sβ+19/4096*AP.S3β+35/12288*AP.S5β)+155/98304*AP.S5β)*AP.S1^2)
		  '13 Return Parameters.δ*(AP.C1^4*(1675/4096*AP.Sβ+825/8192*AP.S3β-AP.C4*(7/4096*AP.Sβ+13/8192*AP.S3β+15/8192*AP.S5β)+AP.C2*(27/1024*AP.Sβ-151/2048*AP.S3β+3/2048*AP.S5β)-13/8192*AP.S5β)*AP.S1^2+AP.η*AP.C1^4*(245/2048*AP.Sβ-57/4096*AP.S3β+AP.C2*(-27/512*AP.Sβ+151/1024*AP.S3β-3/1024*AP.S5β)+AP.C4*(7/2048*AP.Sβ+13/4096*AP.S3β+15/4096*AP.S5β)+13/4096*AP.S5β)*AP.S1^2)
		  '14 Return Parameters.δ*((AP.η*(4375/512*AP.Sβ+8125/1024*AP.S3β+9375/1024*AP.S5β)-(4375/1024*AP.Sβ+8125/2048*AP.S3β+9375/2048*AP.S5β))*AP.C1^8*AP.S1^2)
		  '15 Return Parameters.δ*(AP.C1^4*(20475/4096*AP.Sβ-149391/8192*AP.S3β+AP.C2*(2187/1024*AP.Sβ+10017/2048*AP.S3β-1701/2048*AP.S5β)+7371/8192*AP.S5β+AP.C4*(-567/4096*AP.Sβ-1701/8192*AP.S3β+8505/8192*AP.S5β))*AP.S1^2+AP.η*AP.C1^4*(-3195/2048*AP.Sβ+45711/4096*AP.S3β+AP.C4*(567/2048*AP.Sβ+1701/4096*AP.S3β-8505/4096*AP.S5β)-7371/4096*Ap.S5β+AP.C2*(-2187/512*AP.Sβ-10017/1024*AP.S3β+1701/1024*AP.S5β))*AP.S1^2)
		  '16 Return Parameters.δ*((4375/384*AP.Cβ+625/256*AP.C3β+3125/256*AP.C5β-AP.η*(4375/192*AP.Cβ+625/128*AP.C3β+3125/128*AP.C5β))*AP.C1^7*AP.S1^3)
		  '17 Return Parameters.δ*(AP.C1^5*((-37/384*AP.Cβ+1/384*AP.Cβ*AP.C2β)*AP.Sβ^2-(7/384*AP.Cβ+5/384*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^3+AP.η*AP.C1^5*((37/192*AP.Cβ-1/192*AP.Cβ*AP.C2β)*AP.Sβ^2+(7/192*AP.Cβ+5/192*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^3)
		  '18 Return Parameters.δ*((AP.η*(1/64+1/192*AP.C2β)-(1/128+1/384*AP.C2β))*AP.C1^6*AP.Sβ^3*AP.S1^4)
		  '19 Return Parameters.δ*(AP.η*AP.C1^2*(-245/2048*AP.Sβ+57/4096*AP.S3β-AP.C4*(7/2048*AP.Sβ+13/4096*AP.S3β+15/4096*AP.S5β)+AP.C2*(-27/512*AP.Sβ+151/1024*AP.S3β-3/1024*AP.S5β)-13/4096*AP.S5β)*AP.S1^4+AP.C1^2*(-1675/4096*AP.Sβ-825/8192*AP.S3β+AP.C2*(27/1024*AP.Sβ-151/2048*AP.S3β+3/2048*AP.S5β)+AP.C4/4096*(7*AP.Sβ+13*AP.S3β+15*AP.S5β)*AP.S1^4))
		  '20 Return Parameters.δ*(4375*AP.η*AP.C1^6*(1/768*AP.Sβ+1/512*AP.S3β-5/512*AP.S5β)*AP.S1^4+4375*AP.C1^6*(-1/1536*AP.Sβ-1/1024*AP.S3β+5/1024*AP.S5β)*AP.S1^4)
		  '21 Return Parameters.δ*(AP.C1^2*(-20475/4096*AP.Sβ+149391/8192*AP.S3β+AP.C4/4096*(567*AP.Sβ+1701/2*AP.S3β-8505/2*AP.S5β)+AP.C2/2048*(4374*AP.Sβ+10017*AP.S3β-1701*AP.S5β)-7371/8192*AP.S5β)*AP.S1^4+AP.η*AP.C1^2*(3195/2048*AP.Sβ-45711/4096*AP.S3β+7371/4096*AP.S5β+AP.C2*(-2187/512*AP.Sβ-10017/1024*AP.S3β+1701/1024*AP.S5β)+AP.C4*(-567/2048*AP.Sβ-1701/4096*AP.S3β+8505/4096*AP.S5β))*AP.S1^4)
		  '22 Return Parameters.δ*(AP.η*AP.C1^3*((37/192*AP.Cβ-AP.Cβ*AP.C2β/192)*AP.Sβ^2-(7/192+5/192*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5+AP.C1^3*((-37/384*AP.Cβ+AP.Cβ*AP.C2β/384)*AP.Sβ^2+(7/384*AP.Cβ+5/384*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5)
		  '23 Return Parameters.δ*((1/128+1/384*AP.C2β-AP.η*(1/64+1/192*AP.C2β))*AP.C1^4*AP.Sβ^3*AP.S1^6)
		  '24 Return Parameters.δ*(AP.η*((14067/4096+4689/1024*AP.C2β-5751/4096*AP.C4β)*AP.Sβ+(-297/1024+1053/256*AP.C2β-2187/1024*AP.C4β)*AP.C2*AP.Sβ-(5103/4096+1701/1024*AP.C2β+3645/4096*AP.C4β)*AP.C4*AP.Sβ)*AP.S1^6+((-55539/8192-8145/2048*AP.C2β+5751/8192*AP.C4β)*AP.Sβ+(297/2048-1053/512*AP.C2β+2187/2048*AP.C4β)*AP.C2*AP.Sβ+(5103/8192+1701/2048*AP.C2β+3645/8192*AP.C4β)*AP.C4*AP.Sβ)*AP.S1^6)
		  '25 Return Parameters.δ*(AP.C1^4*(4375/1536*AP.Sβ+4375/1024*AP.S3β-21875/1024*AP.S5β)*AP.S1^6+AP.η*AP.C1^4*(-4375/768*AP.Sβ-4375/512*AP.S3β+21875/512*AP.S5β)*AP.S1^6)
		  '26 Return Parameters.δ*((4375/384*AP.Cβ+625/256*AP.C3β+3125/256*AP.C5β-AP.η*(4375/192*AP.Cβ+625/128*AP.C3β+3125/128*AP.C5β))*AP.C1^3*AP.S1^7)
		  '27 Return Parameters.δ*(AP.η*AP.C1*((351/128*AP.Cβ-243/128*AP.Cβ*AP.C2β)*AP.Sβ^2-(567/128*AP.Cβ+405/128*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7+AP.C1*((-351/256*AP.Cβ+243/256*AP.Cβ*AP.C2β)*AP.Sβ^2+(567/256*AP.Cβ+405/256*AP.Cβ*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7)
		  '28 Return Parameters.δ*((243/256-81/256*AP.C2β-AP.η*(243/128+81/128*AP.C2β))*AP.C1^2*AP.Sβ^3*AP.S1^8)
		  '29 Return Parameters.δ*((4375/1024*AP.Sβ+8125/2048*AP.S3β+9375/2048*AP.S5β-AP.η*(4375/512*AP.Sβ+8125/1024*AP.S3β+9375/1024*AP.S5β))*AP.C1^2*AP.S1^8)
		  '30 Return Parameters.δ*((11875/768*AP.Cβ+3125/768*AP.C3β-AP.η*(11875/384*AP.Cβ+3125/384*AP.C3β))*AP.C1*AP.Sβ^2*AP.S1^9)
		  '31 Return Parameters.δ*((625/256+625/768*AP.C2β-AP.η*(625/128+625/384*AP.C2β))*AP.Sβ^3*AP.S1^10)
		  '32 Return Parameters.δ*(AP.η*((10197/2048*AP.Cβ-3969/2048*AP.Cβ*AP.C2β)*AP.Sβ^2-(1701/2048*AP.Cβ+5103/2048*AP.Cβ*AP.C2β)*AP.C4*AP.Sβ^2)*AP.S2^3+((-44757/4096*AP.Cβ+3969/4096*AP.Cβ*AP.C2β)*AP.Sβ^2+(1701/4096*AP.Cβ+5103/4096*AP.Cβ*AP.C2β)*AP.C4*AP.Sβ^2)*AP.S2^3)
		  '33 Return Parameters.δ*((21875/4096*AP.Cβ+13125/4096*AP.C3β-AP.η*(21875/2048*AP.Cβ+13125/2048*AP.C3β))*AP.Sβ^2*AP.S2^5)
		  '34 Return Parameters.δ*((-37071/16384*AP.Cβ*AP.C2β+AP.Cβ*(-7641/8192+567/32768*AP.C4β)-(10917/8192*AP.Cβ+2835/1024*AP.Cβ*AP.C2β)*AP.C2+(-10089/16384*AP.Cβ+135/8192*AP.Cβ*AP.C2β)*AP.C4+513/8192*AP.Cβ*AP.C6+5167/32768*AP.Cβ*AP.C8)*AP.S2-81/8192*AP.Cβ*AP.C4β*AP.S4+1053/65536*AP.Cβ*AP.C4β*AP.S6+(2565/32768*AP.C3β+729/32768*AP.C5β)*AP.S8+(243/131072*AP.C3β+1215/131072*AP.C5β)*AP.S10+AP.η*((5967/8192*AP.Cβ*AP.C2β+AP.Cβ*(2457/4096-567/16384*AP.C4β)+(4005/4096*AP.Cβ+243/512*AP.Cβ*AP.C2β)*AP.C2+(6633/8192*AP.Cβ-5319/4096*AP.Cβ*AP.C2β)*AP.C4-513/4096*AP.Cβ*AP.C6-567/16384*AP.Cβ*AP.C8)*AP.S2+81/4096*AP.Cβ*AP.C4β*AP.S4-1053/32768*AP.Cβ*AP.C4β*AP.S6-(2565/16384*AP.C3β+729/16384*AP.C5β)*AP.S8-(243/65536*AP.C3β+1215/65536*AP.C5β)*AP.S10))
		  '35 Return Parameters.δ*((-18603/8192*AP.Cβ*AP.C2β+AP.Cβ*(-20475/32768+567/32768*AP.C4β))*AP.S2+(2835/2048*AP.Cβ*AP.C2β+AP.Cβ*(5715/8192+81/8192*AP.C4β))*AP.S4+(135/16384*AP.Cβ*AP.C2β+AP.Cβ*(-20745/65536+1053/65536*AP.C4β))*AP.S6-(513/16384*AP.Cβ+2565/32768*AP.C3β+729/32768*AP.C5β)*AP.S8+(567/65536*AP.Cβ+243/131072*AP.C3β+1215/131072*AP.C5β)*AP.S10+AP.η*((5643/4096*AP.Cβ*AP.C2β+AP.Cβ*(3195/16384-567/16384*AP.C4β))*AP.S6+(513/8192*AP.Cβ+2565/16384*AP.C3β+729/16384*AP.C5β)*AP.S8-(567/32768*AP.Cβ+243/65536*AP.C3β+1215/65536*AP.C5β)*AP.S10))
		  '36 Return Parameters.δ*((319/24576*AP.Cβ*AP.C2β+AP.Cβ*(871/4096+AP.C4β/49152)+(933/4096*AP.Cβ+133/1536*AP.Cβ*AP.C2β)*AP.C2+(625/24576*AP.Cβ+211/4096*AP.Cβ*AP.C2β)*AP.C4-11/12288*AP.Cβ*AP.C6-7/49152*AP.Cβ*AP.C8)*AP.S2-AP.Cβ*AP.C4β*AP.S4/12288+AP.Cβ*AP.C4β*AP.S6/32768-(45/16384*AP.C3β+AP.C5β/16384)*AP.S8-(AP.C3β/65536+5*AP.C5β/65536)*AP.S10+AP.η*((257/12288*AP.Cβ*AP.C2β-AP.Cβ*(1493/6144+AP.C4β/24576)+(-1391/6144+11/768*AP.Cβ*AP.C2β)*AP.C2+(-49/12288*AP.Cβ+77/2048*AP.Cβ*AP.C2β)*AP.C4+11/6144*AP.Cβ*AP.C6+7/24576*AP.Cβ*AP.C8)*AP.S2+AP.Cβ*AP.C4β*AP.S4/6144-AP.Cβ*AP.C4β*AP.S6/16384+(45/8192*AP.C3β+AP.C5β/8192)*AP.S8+(AP.C3β/32768+5/32768*AP.C5β)*AP.S10))
		  '37 Return Parameters.δ*((-157/12288*AP.Cβ*AP.C2β+AP.Cβ*(9287/49152+AP.C4β/49152))*AP.S2+(-133/3072*AP.Cβ*AP.C2β+AP.Cβ*(-1405/12288+AP.C4β/12288))*AP.S4+(211/8192*AP.Cβ*AP.C2β+AP.Cβ*(419/32768+AP.C4β/32768))*AP.S6+(11/24576*AP.Cβ+45/16384*AP.C3β+AP.C5β/16384)*AP.S8-(7/98304*AP.Cβ+AP.C3β/65536+5*AP.C5β/65536)*AP.S10+AP.η*((13/6144*AP.Cβ*AP.C2β-AP.Cβ*(5923/24576+AP.C4β/24576))*AP.S2+(-11/1536*AP.Cβ*AP.C2β+AP.Cβ*(701/6144-AP.C4β/6144))*AP.S4+(77/4096*AP.Cβ*AP.C2β-AP.Cβ*(35/16384+AP.C4β/16384))*AP.S6-(11/12288*AP.Cβ+45/8192*AP.C3β+AP.C5β/8192)*AP.S8+(7/49152*AP.Cβ+AP.C3β/32768+5/32768*AP.C5β)*AP.S10))
		  '38 Return Parameters.δ*((-341/8192*AP.Cβ+AP.Cβ*AP.C2β/8192)*AP.Sβ^2*AP.S2+(-3411/16384*AP.Cβ+7/16384*AP.Cβ*AP.C2β)*AP.Sβ^2*AP.S6+(35/32768*AP.Cβ+21/32768*AP.C3β)*AP.Sβ^2*AP.S10+AP.η*((-43/4096*AP.Cβ-AP.Cβ*AP.C2β/4096)*AP.Sβ^2*AP.S2+(-429/8192*AP.Cβ+7/8192*AP.Cβ*AP.C2β)*AP.Sβ^2*AP.S6+(-35/16384*AP.Cβ-21/16384*AP.C3β)*AP.Sβ^2*AP.S10))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HP3SODoCase(NCase As Integer)
		  'AddTerm(AddressOf GetA1, 0, 0, False)
		  'AddTerm(AddressOf GetA2, 2, 2, False)
		  'AddTerm(AddressOf GetA3, 3, 2, False)
		  'AddTerm(AddressOf GetA4, 3, -2, False)
		  'AddTerm(AddressOf GetA5, 1, 2, False)
		  'AddTerm(AddressOf GetA6, 1, -2, False)
		  'AddTerm(AddressOf GetA7, 2, -2, False)
		  'AddTerm(AddressOf GetA8, 0, 0, False)
		  'AddTerm(AddressOf GetA9, 3, 0, False)
		  'AddTerm(AddressOf GetA10, 0, 2, False)
		  'AddTerm(AddressOf GetA11, 2, 0, False)
		  'AddTerm(AddressOf GetA12, 1, 0, False)
		  'AddTerm(AddressOf GetA13, 1, 0, True)
		  'AddTerm(AddressOf GetA14, 2, 0, True)
		  'AddTerm(AddressOf GetA15, 3, 0, True)
		  'AddTerm(AddressOf GetA16, 1, -2, True)
		  'AddTerm(AddressOf GetA17, 2, -2, True)
		  'AddTerm(AddressOf GetA18, 3, -2, True)
		  'AddTerm(AddressOf GetA19, 0, 2, True)
		  'AddTerm(AddressOf GetA20, 1, 2, True)
		  'AddTerm(AddressOf GetA21, 2, 2, True)
		  'AddTerm(AddressOf GetA22, 3, 2, True)
		  'AddTerm(AddressOf GetA23, 0, 0, False)
		  'AddTerm(AddressOf GetA24, 2, 2, False)
		  'AddTerm(AddressOf GetA25, 3, 2, False)
		  'AddTerm(AddressOf GetA26, 3, -2, False)
		  'AddTerm(AddressOf GetA27, 1, 2, False)
		  'AddTerm(AddressOf GetA28, 1, -2, False)
		  'AddTerm(AddressOf GetA29, 2, -2, False)
		  'AddTerm(AddressOf GetA30, 0, 0, False)
		  'AddTerm(AddressOf GetA31, 3, 0, False)
		  'AddTerm(AddressOf GetA32, 0, 2, False)
		  'AddTerm(AddressOf GetA33, 2, 0, False)
		  'AddTerm(AddressOf GetA34, 1, 0, False)
		  'AddTerm(AddressOf GetA35, 1, 0, True)
		  'AddTerm(AddressOf GetA36, 2, 0, True)
		  'AddTerm(AddressOf GetA37, 3, 0, True)
		  'AddTerm(AddressOf GetA38, 1, -2, True)
		  'AddTerm(AddressOf GetA39, 2, -2, True)
		  'AddTerm(AddressOf GetA40, 3, -2, True)
		  'AddTerm(AddressOf GetA41, 0, 2, True)
		  'AddTerm(AddressOf GetA42, 1, 2, True)
		  'AddTerm(AddressOf GetA43, 2, 2, True)
		  'AddTerm(AddressOf GetA44, 3, 2, True)
		  '
		  '1 Return AP.χsx*(2*AP.Cβ*AP.C2^2*AP.Sβ-AP.η*AP.Cβ*AP.C2^3*AP.Sβ)
		  '2 Return AP.χsz*(AP.η*AP.C1^4*(-5/2-7/2*AP.C2β+(1/2+AP.C2β/6)*AP.C2)+AP.C1^4*(-3-AP.C2β+(5+5/3*AP.C2β)*AP.C4))+AP.χsx*(AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β)-AP.η*AP.C1^4*(19/6*AP.S2β+1/3*AP.C2*AP.S2β))
		  '3 Return AP.χsx*(AP.η*(1/2+AP.C2β/6)*AP.C1^5*AP.S1+(5+5/3*AP.C2β)*AP.C1^5*AP.S1)
		  '4 Return AP.χsx*(AP.η*(1/2+AP.C2β/6)*AP.C1*AP.S1^5+(5+5/3*AP.C2β)*AP.C1*AP.S1^5)
		  '5 Return AP.χsx*(AP.η*AP.C1^3*(-17/4+79/12*AP.C2β+(-1/4+7/12*AP.C2β)*AP.C2)*AP.S1+AP.C1^3*(3/2-13/6*AP.C2β+(-5/2+35/6*AP.C2β)*AP.C2)*AP.S1)+AP.χsz*(AP.η*AP.C1^3*(-7*AP.S2β+2/3*AP.C2*AP.S2β)*AP.S1+AP.C1^3*(-2*AP.S2β+20/3*AP.C2*AP.S2β)*AP.S1)
		  '6 Return AP.χsx*(AP.C1*(3/2-13/6*AP.C2β+(5/2-35/6*AP.C2β)*AP.C2)*AP.S1^3+AP.η*AP.C1*(-17/4+79/12*AP.C2β+(1/4-7/12*AP.C2β)*AP.C2)*AP.S1^3)+AP.χsz*(-AP.C1*(2*AP.S2β+20/3*AP.C2*AP.S2β)*AP.S1^3-AP.η*AP.C1*(7*AP.S2β+2/3*AP.C2*AP.S2β)*AP.S1^3)
		  '7 Return AP.χsz*(AP.η*(5/2+7/2*AP.C2β+(1/2+AP.C2β/6)*AP.C2)*AP.S1^4+(3+AP.C2β+(5+5/3*AP.C2β)*AP.C2)*AP.S1^4)+AP.χsx*(-(7/3*AP.S2β+10/3*AP.C2*AP.S2β)*AP.S1^4+AP.η*(19/6*AP.S2β-1/3*AP.C2*AP.S2β)*AP.S1^4)
		  '8 Return AP.χsz*(-3+3/2*AP.η)*AP.C2*AP.Sβ^2*AP.S2^2
		  '9 Return AP.χsx*(3/4+AP.C2β/4-AP.η*(3/8+AP.C2β/8))*AP.S2^3
		  '10 Return AP.χsx*(10/3+1/3*AP.η)*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2+AP.χsz*(5+AP.η/2)*AP.C2*AP.Sβ^2*AP.S2^2
		  '11 Return AP.χsz*(3/2+AP.C2β/2-AP.η*(3/4+AP.C2β/4))*AP.C2*AP.S2^2+AP.χsx*(AP.η/2-1)*AP.C2*AP.S2β*AP.S2^2
		  '12 Return AP.χsx*(-11/16*AP.C2β*AP.S2-3/4*AP.S2^3-7/16*AP.C2β*AP.S6+AP.η*(11/32*AP.C2β*AP.S2+3/8*AP.S2^3+7/32*AP.C2β*AP.S6))+AP.χsz*(AP.S2β*AP.S2/2-AP.S2β*AP.S6/2+AP.η*(-1/4*AP.S2β*AP.S2+1/4*AP.S2β*AP.S6))
		  '13 Return AP.χsy*((15/8-3/8*AP.C2β+(9/8-5/8*AP.C2β)*AP.C4)*AP.S2+AP.η*(-15/16+3/16*AP.C2β+(-9/16+5/16*AP.C2β)*AP.C4)*AP.S2)
		  '14 Return AP.χsy*(AP.η-2)*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2
		  '15 Return AP.χsy*(3/4+AP.C2β/4-AP.η*(3/8+AP.C2β/8))*AP.S2^3
		  '16 Return AP.χsy*(AP.C1*(5/2-11/6*AP.C2β+(15/2-25/6*AP.C2β)*AP.C2)*AP.S1^3+AP.η*AP.C1*(1/4-31/12*AP.C2β+(3/4-5/12*AP.C2β)*AP.C2)*AP.S1^3)
		  '17 Return AP.χsy*(-(7/3*AP.S2β+10/3*AP.C2*AP.S2β)*AP.S1^4-AP.η*(5/6*AP.S2β+1/3*AP.C2*AP.S2β)*AP.S1^4)
		  '18 Return AP.χsy*(5+5/3*AP.C2β+AP.η*(1/2+AP.C2β/6))*AP.C1*AP.S1^5
		  '19 Return -AP.χsy*(1/3+11/6*AP.η)*AP.Cβ*AP.Sβ*AP.S2^2
		  '20 Return AP.χsy*(AP.η*AP.C1^3*(1/4-31/12*AP.C2β)+(-3/4+5/12*AP.C2β)*AP.C2)*AP.S1+AP.C1^3*(5/2-11/6*AP.C2β+(-15/2+25/6*AP.C2β)*AP.C2*AP.S1)
		  '21 Return AP.χsy*(AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β)+AP.η*AP.C1^4*(5/6*AP.S2β-1/3*AP.C2*AP.S2β))
		  '22 Return AP.χsy*(AP.η*(1/2+AP.C2β/6)+5+5/3*AP.C2β)*AP.C1^5*AP.S1
		  '23 Return 2*Parameters.δ*AP.χax*AP.Cβ*AP.C2^3*AP.Sβ
		  '24 Return Parameters.δ*(AP.χaz*AP.C1^4*(-3-AP.C2β+(5+5/3*AP.C2β)*AP.C2)+AP.χax*AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β))
		  '25 Return Parameters.δ*AP.χax*(5+5/3*AP.C2β)*AP.C1^5*AP.S1
		  '26 Return Parameters.δ*AP.χax*(5+5/3*AP.C2β)*AP.C1*AP.S1^5
		  '27 Return Parameters.δ*(AP.χax*(3/2-13/6*AP.C2β+(-5/2+35/6*AP.C2β)*AP.C2)+AP.χaz*(-2*AP.S2β+20/3*AP.C2*AP.S2β))*AP.C1^3*AP.S1
		  '28 Return Parameters.δ*(AP.χax*(3/2-13/6*AP.C2β+(5/2-35/6*AP.C2β)*AP.C2)+AP.χaz*(-2*AP.S2β-20/3*AP.C2*AP.S2β))*AP.C1*AP.S1^3
		  '29 Return Parameters.δ*(AP.χaz*(3+AP.C2β+(5+5/3*AP.C2β)*AP.C2)*AP.S1^4-AP.χax*(7/2*AP.S2β+10/3*AP.C2*AP.S2β)*AP.S1^4)
		  '30 Return -3*Parameters.δ*AP.χaz*AP.C2*AP.Sβ^2*AP.S2^2
		  '31 Return Parameters.δ*AP.χax*(3/4+AP.C2β/4)*AP.S2^3
		  '32 Return Parameters.δ*(10/3*AP.χax*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2+5*AP.χaz*AP.C2*AP.Sβ^2*AP.S2^2)
		  '33 Return Parameters.δ*AP.χaz*(3/2*AP.C2β/2)*AP.C2*AP.S2^2-AP.χax*AP.C2*AP.S2β*AP.S2^2
		  '34 Return Parameters.δ*(AP.χax*(-11/16*AP.C2β*AP.S2-3/4*AP.S2^3-7/16*AP.C2β*AP.S6)+AP.χaz*(AP.S2β*AP.S2/2-AP.S2β*AP.S6/2))
		  '35 Return Parameters.δ*(AP.χay*(15/8-3/8*AP.C2β+(9/8-5/8*AP.C2β)*AP.C4)*AP.S2)
		  '36 Return -2*Parameters.δ*AP.χay*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2
		  '37 Return Parameters.δ*AP.χay*(3/4+AP.C2β/4)*AP.S2^3
		  '38 Return Parameters.δ*AP.χay*AP.C1*(5/2-11/6*AP.C2β+(15/2-25/6*AP.C2β)*AP.C2)*AP.S1^3
		  '39 Return Parameters.δ*AP.χay*(-7/3*AP.S2β-10/3*AP.C2*AP.S2β)*AP.S1^4
		  '40 Return Parameters.δ*AP.χay*(5+5/3*AP.C2β)*AP.C1*AP.S1^5
		  '41 Return -1/3*Parameters.δ*AP.χay*AP.Cβ*AP.Sβ*AP.S2^2
		  '42 Return Parameters.δ*AP.χay*AP.C1^3*(5/2-11/6*AP.C2β+(-15/2+25/6*AP.C2β)*AP.C2)*AP.S1
		  '43 Return Parameters.δ*AP.χay*AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β)
		  '44 Return Parameters.δ*AP.χay*(5+5/3*AP.C2β)*AP.C1^5*AP.S1
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HX0DoCase(NCase As Integer)
		  Var AP As AmplitudeParameters = APSet(NCase)
		  Var C1 As Double = AP.C1
		  Var C1p3 As Double = C1*C1*C1
		  Var C1p4 As Double = C1p3*C1
		  Var S1 As Double = AP.S1
		  Var S1p3 As Double = S1*S1*S1
		  Var S1p4 As Double = S1p3*S1
		  Var Cβ As Double = AP.Cβ
		  Var Sβ As Double = AP.Sβ
		  
		  // Set up the amplitude array
		  A(NCase,1) = 4*C1*Sβ*S1p3
		  A(NCase,2) = -2*Cβ*S1p4
		  A(NCase,3) =  -4*C1p3*Sβ*S1
		  A(NCase,4) = -2*Cβ*C1p4
		  NTerms = 4 // this is how many terms we have
		  
		  If NCase = 0 Then // If this is the main case
		    // Set up the α array
		    Nα(1) = 1
		    Nα(2) = 2
		    Nα(3) = 1
		    Nα(4) = 2
		    
		    // Set up the Ψr array
		    NΨ(1) = -2
		    NΨ(2) = -2
		    NΨ(3) = 2
		    NΨ(4) = 2
		    
		    // Set up the Cosine Versus Sine Array
		    // (All wave terms here are sine)
		    NCvS(1) = 1
		    NCvS(2) = 1
		    NCvS(3) = 1
		    NCvS(4) = 1
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HX1DoCase(NCase As Integer)
		  'AddTerm(AddressOf GetA1, 1, -3, True)
		  'AddTerm(AddressOf GetA2, 2, -3, True)
		  'AddTerm(AddressOf GetA3, 3, -3, True)
		  'AddTerm(AddressOf GetA4, 1, -1, True)
		  'AddTerm(AddressOf GetA5, 2, -1, True)
		  'AddTerm(AddressOf GetA6, 3, -1, True)
		  'AddTerm(AddressOf GetA7, 0, 1, True)
		  'AddTerm(AddressOf GetA8, 1, 1, True)
		  'AddTerm(AddressOf GetA9, 2, 1, True)
		  'AddTerm(AddressOf GetA10, 3, 1, True)
		  'AddTerm(AddressOf GetA11, 1, 3, True)
		  'AddTerm(AddressOf GetA12, 2, 3, True)
		  'AddTerm(AddressOf GetA13, 3, 3, True)
		  '
		  '1 return Parameters.δ*(-45/8)*AP.C1^2*AP.S2β*AP.S1^4
		  '2 return Parameters.δ*(9/2)*AP.C2β*AP.C1*AP.S1^5
		  '3 return Parameters.δ*(9/8)*AP.S2β*AP.S1^6
		  '4 return Parameters.δ*(-1/64*AP.Cβ*AP.Sβ+43/128*AP.Cβ*AP.C2*AP.Sβ-23/128*AP.C4*AP.S2β+5/256*Ap.C6*AP.S2β)
		  '5 return Parameters.δ*((-1-AP.C2β/4)*AP.C1+1/4*AP.C2β*AP.C1*AP.C2)*AP.S1^3 
		  '6 return Parameters.δ*(1/8)*AP.C1^2*AP.S2β*AP.S1^4
		  '7 return Parameters.δ*(1/2)*AP.Sβ^2*AP.S4
		  '8 return Parameters.δ*(AP.Cβ*AP.Sβ/64+43/128*AP.Cβ*AP.C2*AP.Sβ+23/128*AP.C4*AP.S2β+5/256*AP.C6*AP.S2β)
		  '9 return Parameters.δ*AP.S1*((-1-AP.C2β/4)*AP.C1^3-1/4*AP.C2β*AP.C1^3*AP.C2)
		  '10 return Parameters.δ*(-1/8)*(AP.C1^4*AP.S2β*AP.S1^2)
		  '11 return Parameters.δ*(45/8)*(AP.C1^4)*(AP.S2β)*(AP.S1^2)
		  '12 return Parameters.δ*(9/2)*AP.C2β*AP.C1^5*AP.S1
		  '13 return Parameters.δ*(-9/8)*AP.C1^6*AP.S2β
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HX2DoCase(NCase As Integer)
		  '
		  'AddTerm(AddressOf GetA1, 2, 2, True)
		  'AddTerm(AddressOf GetA2, 4, 4, True)
		  'AddTerm(AddressOf GetA3, 3, 4, True)
		  'AddTerm(AddressOf GetA4, 3, 2, True)
		  'AddTerm(AddressOf GetA5, 2, 4, True)
		  'AddTerm(AddressOf GetA6, 4, 2, True)
		  'AddTerm(AddressOf GetA7, 1, 4, True)
		  'AddTerm(AddressOf GetA8, 1, -2, True)
		  'AddTerm(AddressOf GetA9, 2, -2, True)
		  'AddTerm(AddressOf GetA10, 1, -4, True)
		  'AddTerm(AddressOf GetA11, 3, -2, True)
		  'AddTerm(AddressOf GetA12, 2, -4, True)
		  'AddTerm(AddressOf GetA13, 4, -2, True)
		  'AddTerm(AddressOf GetA14, 3, -4, True)
		  'AddTerm(AddressOf GetA15, 4, -4, True)
		  'AddTerm(AddressOf GetA16, 0, 2, True)
		  'AddTerm(AddressOf GetA17, 0, 4, True)
		  '
		  '1 Return (4*AP.Sβ+28/3*AP.S3β-AP.η*(12*AP.Sβ+28*AP.S3β))*AP.C1^3*AP.S1^5
		  '2 Return (AP.η*(4*AP.Cβ+28*AP.C3β-(4/3*AP.Cβ+28/3*AP.C3β)))*AP.C1^2*AP.S1^6
		  '3 Return ((4/3*AP.Sβ-4*AP.S3β)+AP.η*(-4*AP.Sβ+12*AP.S3β))*AP.C1*AP.S1^7
		  '4 Return (8*AP.η-8/3)*AP.Cβ*AP.Sβ*AP.S1^8
		  '5 Return AP.C1*(-79/8*AP.Sβ+AP.C2*(3/4*AP.Sβ-19/12*AP.S3β)+AP.C4*(AP.Sβ/8+7/24*AP.S3β)-3/8*AP.S3β)*AP.S1^3+AP.η*AP.C1*(103/24*AP.Sβ-AP.C4*(3/8*AP.Sβ+7/8*AP.S3β)+9/8*AP.S3β+AP.C2*(-9/4*AP.Sβ+19/4*AP.S3β))*AP.S1^3
		  '6 Return (47/8*AP.Cβ+AP.C3β/8+(7/6*AP.Cβ+AP.C3β/6)*AP.C2-(AP.Cβ/24+7/24*AP.C3β)*AP.C4+AP.η*(-119/24*AP.Cβ-3/8*AP.C3β-(7/2*AP.Cβ+AP.C3β/2)*AP.C2+(AP.Cβ/8+7/8*AP.C3β)*AP.C4))*AP.S1^4
		  '7 Return (4/3*AP.Sβ-(1/3+AP.C2β)*AP.C2*AP.Sβ+AP.η*(-4*AP.Sβ+(1+3*AP.C2β)*AP.C2*AP.Sβ))*AP.C1*AP.S1^5
		  '8 Return (2*AP.η-2/3)*AP.Cβ*AP.C1^2*AP.Sβ^2*AP.S1^6
		  '9 Return (15/2*AP.η-5/2)*AP.Cβ*AP.C2*AP.Sβ^2*AP.S2^2
		  '10 Return AP.C1^3*(79/8*AP.Sβ+AP.C2*(3/4*AP.Sβ-19/12*AP.S3β)-AP.C4*(AP.Sβ/8+7/24*AP.S3β)+3/8*AP.S3β)*AP.S1+AP.η*AP.C1^3*(-103/24*AP.Sβ+AP.C4*(3/8*AP.Sβ+7/8*AP.S3β)-9/8*AP.S3β+AP.C2*(-9/4*AP.Sβ+19/4*AP.S3β))*AP.S1
		  '11 Return AP.C1^4*(47/8*AP.Cβ+AP.C3β/8-(7/6*AP.Cβ+AP.C3β/6)*AP.C2-(AP.Cβ/24+7/24*AP.C3β)*AP.C4)+AP.η*AP.C1^4*(-119/24*AP.Cβ-3/8*AP.C3β+(7/2*AP.Cβ+AP.C3β/2)*AP.C2+(AP.Cβ/8+7/8*AP.C3β)*AP.C4)
		  '12 Return (-4/3*AP.Sβ-(1/3+AP.C2β)*AP.C2*AP.Sβ+AP.η*(4*AP.Sβ+(1+3*AP.C2β)*AP.C2*AP.Sβ))*AP.C1^5*AP.S1
		  '13 Return (2*AP.η-2/3)*AP.Cβ*AP.C1^6*AP.Sβ^2*AP.S1^2
		  '14 Return (AP.η*(12*AP.Sβ+28*AP.S3β)-(4*AP.Sβ+28/3*AP.S3β))*AP.C1^5*AP.S1^3
		  '15 Return (AP.η*(4*AP.Cβ+28*AP.C3β)-(4/3*AP.Cβ+28/3*AP.C3β))*AP.C1^6*AP.S1^2
		  '16 Return (8/3+8*AP.C2β-AP.η*(8+24*AP.C2β))*AP.C1^7*AP.Sβ*AP.S1
		  '17 Return (8*AP.η-8/3)*AP.Cβ*AP.C1^8*AP.Sβ^2
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HX2SODoCase(NCase As Integer)
		  'AddTerm(AddressOf GetA1, 1, 1, False)
		  'AddTerm(AddressOf GetA2, 1, -1, False)
		  'AddTerm(AddressOf GetA3, 1, -1, True)
		  'AddTerm(AddressOf GetA4, 0,1, True)
		  'AddTerm(AddressOf GetA5, 1, 1, True)
		  'AddTerm(AddressOf GetA6, 1, 1, False)
		  'AddTerm(AddressOf GetA7, 1, -1, False)
		  'AddTerm(AddressOf GetA8, 1, -1, True)
		  'AddTerm(AddressOf GetA9, 0, 1, True)
		  'AddTerm(AddressOf GetA10,1, 1, True)
		  '
		  '1 Return AP.χay*(1/2+AP.C2/2)
		  '2 Return AP.χay*AP.S1^2
		  '3 Return AP.χax*(AP.Cβ^2/2-AP.Cβ^2*AP.C2/2)+AP.χaz*(-AP.Cβ*AP.Sβ/2+AP.Cβ*AP.C2*AP.Sβ/2)
		  '4 Return AP.χax*AP.Cβ*AP.Sβ*AP.S2-AP.χaz*AP.Sβ^2*AP.S2
		  '5 Return AP.χax*(AP.Cβ^2/2+AP.Cβ^2*AP.C2/2)+AP.χaz*(-AP.Cβ*AP.Sβ/2-AP.Cβ*AP.C2*AP.Sβ/2)
		  '6 Return Parameters.δ*(AP.χsy*(1/2+AP.C2/2))
		  '7 Return Parameters.δ*(AP.χsy*AP.S1^2)
		  '8 Return Parameters.δ*(AP.χsx*(AP.Cβ^2/2-AP.Cβ^2*AP.C2/2)+AP.χsz*(-AP.Cβ*AP.Sβ/2+AP.Cβ*AP.C2*AP.Sβ/2))
		  '9 Return Parameters.δ*(AP.χsx*AP.Cβ*AP.Sβ*AP.S2-AP.χsz*AP.Sβ^2*AP.S2)
		  '10 Return Parameters.δ*(AP.χsx*(AP.Cβ^2/2+AP.Cβ^2*AP.C2/2)+AP.χsz*(-AP.Cβ*AP.Sβ/2-AP.Cβ*AP.C2*AP.Sβ/2))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HX3DoCase(NCase As Integer)
		  '
		  'AddTerm(AddressOf GetA1, 1, -2, True)
		  'AddTerm(AddressOf GetA2, 2, -2, True)
		  'AddTerm(AddressOf GetA3, 1, 2, True)
		  'AddTerm(AddressOf GetA4, 2, 2, True)
		  'AddTerm(AddressOf GetA5, 1, -5, True)
		  'AddTerm(AddressOf GetA6, 2, -5, True)
		  'AddTerm(AddressOf GetA7, 3, -5, True)
		  'AddTerm(AddressOf GetA8, 4, -5, True)
		  'AddTerm(AddressOf GetA9, 5, -5, True)
		  'AddTerm(AddressOf GetA10, 1, -3, True)
		  'AddTerm(AddressOf GetA11, 2, -3, True)
		  'AddTerm(AddressOf GetA12, 3, -3, True)
		  'AddTerm(AddressOf GetA13, 4, -3, True)
		  'AddTerm(AddressOf GetA14, 5, -3, True)
		  'AddTerm(AddressOf GetA15, 1, -1, True)
		  'AddTerm(AddressOf GetA16, 2, -1, True)
		  'AddTerm(AddressOf GetA17, 3, -1, True)
		  'AddTerm(AddressOf GetA18, 4, -1, True)
		  'AddTerm(AddressOf GetA19, 5, -1, True)
		  'AddTerm(AddressOf GetA20, 0, 1, True)
		  'AddTerm(AddressOf GetA21, 0, 3, True)
		  'AddTerm(AddressOf GetA22, 1, 1, True)
		  'AddTerm(AddressOf GetA23, 2, 1, True)
		  'AddTerm(AddressOf GetA24, 1, 3, True)
		  'AddTerm(AddressOf GetA25, 2, 3, True)
		  'AddTerm(AddressOf GetA26, 3, 3, True)
		  'AddTerm(AddressOf GetA27, 4, 3, True)
		  'AddTerm(AddressOf GetA28, 5, 3, True)
		  'AddTerm(AddressOf GetA29, 1, 5, True)
		  'AddTerm(AddressOf GetA30, 2, 5, True)
		  'AddTerm(AddressOf GetA31, 3, 5, True)
		  'AddTerm(AddressOf GetA32, 4,5, True)
		  'AddTerm(AddressOf GetA33, 5, 5 , True)
		  '
		  '1 Return 8*π*AP.C1*AP.Sβ*AP.S1^3
		  '2 Return -4*π*AP.Cβ*AP.S1^4
		  '3 Return -8*π*AP.C1^3*AP.Sβ*AP.S1
		  '4 Return -4*π*AP.Cβ*AP.C1^4
		  '5 Return Parameters.δ*(AP.C1^4*(-4375/384*AP.S2β-4375/256*AP.S4β)*AP.S1^6+AP.η*AP.C1^4*(4375/192*AP.S2β+4375/128*AP.S4β)*AP.S1^6)
		  '6 Return Parameters.δ*(625/96*AP.C2β+625/32*AP.C4β-AP.η*(625/48*AP.C2β+625/16*AP.C4β))*AP.C1^3*AP.S1^7
		  '7 Return Parameters.δ*(-625/256*AP.S2β+5625/512*AP.S4β+AP.η*(625/48*AP.S2β-5625/256*AP.S4β))*AP.C1^2*AP.S1^8
		  '8 Return Parameters.δ*(625/96+625/48*AP.C2β-AP.η*(625/48+625/24*AP.C2β))*AP.C1*AP.Sβ^2*AP.S1^9
		  '9 Return Parameters.δ*(625/192-625/96*AP.η)*AP.Cβ*AP.Sβ^3*AP.S1^10
		  '10 Return Parameters.δ*(AP.η*AP.C1^2*(-4923/512*AP.S2β+AP.C2*(459/128*AP.S2β-2079/256*AP.S4β)-945/1024*AP.S4β+AP.C4*(567/512*AP.S2β+1701/1024*AP.S4β))*AP.S1^4+AP.C1^2*(22203/1024*AP.S2β-AP.C4*(567/1024*AP.S2β+1701/2048*AP.S4β)+945/2048*AP.S4β+AP.C2*(-459/256*AP.S2β+2079/512*AP.S4β))*AP.S1^4)
		  '11 Return Parameters.δ*(AP.η*AP.C1*(27/16+1233/128*AP.C2β+27/128*AP.C4β+(27/8+27/16*AP.C2β+27/16*AP.C4β)*AP.C2-(81/128*AP.C2β+243/128*AP.C4β)*AP.C4)*AP.S1^5+AP.C1*(-27/32-4689/256*AP.C2β-27/256*AP.C4β-(27/16+27/32*AP.C2β+27/32*AP.C4β)*AP.C2+(81/256*AP.C2β+243/256*AP.C4β)*AP.C4)*AP.S1^5)
		  '12 Return Parameters.δ*(AP.η*((4761/1024-1377/1024*AP.C2β)*AP.S2β+(837/256-621/256*AP.C2β)*AP.C2*AP.S2β+(243/1024-2187/1024*AP.C2β)*AP.C4*AP.S2β)*AP.S1^6+((-11673/2048+1377/2048*AP.C2β)*AP.S2β+(-837/512+621/512*AP.C2β)*AP.C2*AP.S2β+(-243/2048+2187/2048*AP.C2β)*AP.C4*AP.S2β)*AP.S1^6)
		  '13 Return Parameters.δ*(AP.η*AP.C1*((81/32-27/16*AP.C2β)*AP.Sβ^2-(81/32+81/16*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7+AP.C1*((-81/64+27/32*AP.C2β)*AP.Sβ^2+(81/64+81/32*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^7)
		  '14 Return Parameters.δ*(81/64-81/32*AP.η)*AP.Cβ*AP.C1^2*AP.Sβ^3*AP.S1^8
		  '15 Return Parameters.δ*(683/16384*AP.Cβ*AP.Sβ+(557/4096-11/12288*AP.C2β)*AP.C4*AP.S2β+(-1719/32768+91/32768*AP.C2β)*AP.C6*AP.S2β-1/16384*AP.Cβ*AP.S3β+AP.C2*(-10511/49152*AP.Cβ*AP.Sβ+173/49152*AP.Cβ*AP.S3β)+AP.η*(85/8192*AP.Cβ*AP.Sβ+(-679/6144+11/6144*AP.C2β)*AP.C4*AP.S2β-(201/16384+91/16384*AP.C2β)*AP.C6*AP.S2β+1/8192*AP.Cβ*AP.S3β+AP.C2*(6031/24576*AP.Cβ*AP.Sβ-173/24576*AP.Cβ*AP.S3β)-AP.C10*(7/49152*AP.S2β+7/32768*AP.S4β)+AP.C8*(-37/24576*AP.S2β+91/16384*AP.S4β))+AP.C8*(37/49152*AP.S2β-91/32768*AP.S4β)+AP.C10*(7/98304*AP.S2β+7/65536*AP.S4β))
		  '16 Return Parameters.δ*(AP.η*(19/512*AP.C4β*AP.C3+9/512*AP.C4β*AP.C5+AP.C1*(-11/16-35/128*AP.C2β+79/1536*AP.C4β+(1/32-37/256*AP.C2β)*AP.C2+(1/32+3/128*AP.C2β)*AP.C4-1/768*AP.C2β*AP.C6)-1/512*AP.C4β*AP.C7)*AP.S1^3+(-19/1024*AP.C4β*AP.C3-9/1024*AP.C4β*AP.C5+AP.C1*(19/32-23/768*AP.C2β-79/3072*AP.C4β-(1/64+347/512*AP.C2β)*AP.C2-(1/64+3/256*AP.C2β)*AP.C4+1/1536*AP.C2β*AP.C6)+1/1024*AP.C4β*AP.C7)*AP.S1^3)
		  '17 Return Parameters.δ*(AP.C1^2*(-355/1024*AP.S2β-AP.C2*(13/256*AP.S2β+11/512*AP.S4β)+AP.C4*(-1/1024*AP.S2β+9/2048*AP.S4β)-5/2048*AP.S4β)*AP.S1^4+AP.η*AP.C1^2*(-29/512*AP.S2β+AP.C4*(1/512*AP.S2β-9/1024*AP.S4β)+AP.C2*(13/128*AP.S2β+11/256*AP.S4β)+5/1024*AP.S4β)*AP.S1^4)
		  '18 Return Parameters.δ*(AP.η*AP.C1^3*((7/48+1/24*AP.C2β)*AP.Sβ^2-(1/48+1/24*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5+AP.C1^3*(-(7/96+1/48*AP.C2β)*AP.Sβ^2+(1/96+1/48*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1^5)
		  '19 Return Parameters.δ*(1/96-1/48*AP.η)*AP.Cβ*AP.C1^4*AP.Sβ^3*AP.S1^6
		  '20 Return Parameters.δ*((-77/256+1/256*AP.Cβ)*AP.Sβ^2*AP.S4+(5/512+7/512*AP.C2β)*AP.Sβ^2*AP.S8+AP.η*((45/128-1/128*AP.C2β)*AP.Sβ^2*AP.S4-(5/256-7/256*AP.C2β)*AP.Sβ^2*AP.S8))
		  '21 Return Parameters.δ* (135/64+189/64*AP.C2β-AP.η*(135/32+189/32*AP.C2β))*AP.C2*AP.Sβ^2*AP.S2^3
		  '22 Return Parameters.δ* (-683/16384*AP.Cβ*AP.Sβ+(-557/4096+11/12288*AP.C2β)*AP.C4*AP.S2β+(-1719/32768+91/32768*AP.C2β)*AP.C6*AP.S2β+AP.Cβ*AP.Sβ/16384+AP.C2*(-10511/49152*AP.Cβ*AP.Sβ+173/49152*AP.Cβ*AP.S3β)+AP.η*(-85/8192*(AP.Cβ)*AP.Sβ+(679/6144-11/6144*AP.C2β)*AP.C4*AP.S2β-(201/16384+91/16384*AP.C2β)*AP.C6*AP.S2β-AP.Cβ*AP.S3β/8192+AP.C2*(6031/24576*AP.Cβ*AP.Sβ-173/24576*AP.Cβ*AP.S3β)+AP.C8*(37/24576*AP.S2β-91/16384*AP.S4β)-AP.C10*(7/49152*AP.S2β+7/32768*AP.S4β))+AP.C10*(7/98304*AP.S2β+7/65536*AP.S4β)+AP.C8*(-37/49152*AP.S2β+91/32768*AP.S4β))
		  '23 Return Parameters.δ*(AP.C1^3*(19/32-23/768*AP.C2β-79/3072*AP.C4β+(1/64+347/512*AP.C2β)*AP.C2-(1/64+3/256*AP.C2β)*AP.C4-AP.C2β*AP.C6/1536)*AP.S1+19*AP.C4β*AP.C1^3*AP.S3/1024-9*AP.C4β*AP.C1^3*AP.S5/1024-AP.C4β*AP.C1^3*AP.S7/1024+AP.η*(AP.C1^3*(-11/16-35/128*AP.C2β+79/1536*AP.C4β+(-1/32+37/256*AP.C2β)*AP.C2+(1/32+3/128*AP.C2β)*AP.C4+1/768*AP.C2β*AP.C6)*AP.S1-19/512*AP.C4β*AP.C1^3*AP.S3+9/512*AP.C4β*AP.C1^3*AP.S5+1/512*AP.C4β*AP.C1^3*AP.S7))
		  '24 Return Parameters.δ*(AP.η*AP.C1^4*(4923/512*AP.S2β+AP.C4*(567/1024*AP.S2β+1701/2048*AP.S4β)-945/2048*AP.S4β+AP.C2*(-459/256*AP.S2β+2079/512*AP.S4β))*AP.S1^2)
		  '25 Return Parameters.δ*(AP.η*AP.C1^5*(27/16+1233/128*AP.C2β+27/128*AP.C4β-(27/8+27/16*AP.C2β+27/16*AP.C4β)*AP.C2-(81/128*AP.C2β+243/128*AP.C4β)*AP.C4)*AP.S1+AP.C1^5*(-27/32-4689/256*AP.C2β-27/256*AP.C4β+(27/16+27/32*AP.C2β+27/32*AP.C4β)*AP.C2+(81/256*AP.C2β+243/256*AP.C4β)*AP.C4)*AP.S1)
		  '26 Return Parameters.δ*(AP.C1^6*(11673/2048*AP.S2β+AP.C4*(243/2048*AP.S2β-2187/4096*AP.S4β)+AP.C2*(-837/512*AP.S2β+621/1024*AP.S4β)-1377/4096*AP.S4β)+AP.η*AP.C1^6*(-4761/1024*AP.S2β+AP.C2*(837/256*AP.S2β-621/512*AP.S4β)+1377/2048*AP.S4β+AP.C4*(-243/1024*AP.S2β+2187/2048*AP.S4β)))
		  '27 Return Parameters.δ*(AP.C1^7*((-81/64+27/32*AP.C2β)*AP.Sβ^2-(81/64+81/32*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1+AP.η*AP.C1^7*((81/32-27/16*AP.C2β)*AP.Sβ^2+(81/32+81/16*AP.C2β)*AP.C2*AP.Sβ^2)*AP.S1)
		  '28 Return Parameters.δ*(81/32*AP.η-81/64)*AP.Cβ*AP.C1^8*AP.Sβ^3*AP.S1^2
		  '29 Return Parameters.δ*(4375/384*AP.S2β+4375/256*AP.S4β-AP.η*(4375/192*AP.S2β+4375/128*AP.S4β))*AP.C1^6*AP.S1^4
		  '30 Return Parameters.δ*(625/96*AP.C2β+625/32*AP.C4β-AP.η*(625/48*AP.C2β+625/16*AP.C4β))*AP.C1^7*AP.S1^3
		  '31 Return Parameters.δ*(625/256*AP.S2β-5625/512*AP.S4β+AP.η*(-625/128*AP.S2β+5625/256*AP.S4β))*AP.C1^8*AP.S1^2
		  '32 Return Parameters.δ*(625/96+625/48*AP.C2β-AP.η*(625/48+625/24*AP.C2β))*AP.C1^9*AP.Sβ^2*AP.S1
		  '33 Return Parameters.δ*(625/96*AP.η-625/192)*AP.Cβ*AP.C1^10*AP.Sβ^3
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub HX3SODoCase(NCase As Integer)
		  'AddTerm(AddressOf GetA1, 0, 0, False)
		  'AddTerm(AddressOf GetA2, 2, 2, False)
		  'AddTerm(AddressOf GetA3, 3, 2, False)
		  'AddTerm(AddressOf GetA4, 1, 2, False)
		  'AddTerm(AddressOf GetA5, 1, -2, False)
		  'AddTerm(AddressOf GetA6, 2, -2, False)
		  'AddTerm(AddressOf GetA7, 3, -2, False)
		  'AddTerm(AddressOf GetA8, 2, 0, False)
		  'AddTerm(AddressOf GetA9, 0, 2, False)
		  'AddTerm(AddressOf GetA10, 3, 0, False)
		  'AddTerm(AddressOf GetA11, 1, 0, False)
		  'AddTerm(AddressOf GetA12, 1, 0, True)
		  'AddTerm(AddressOf GetA13, 2, 0, True)
		  'AddTerm(AddressOf GetA14, 3, 0, True)
		  'AddTerm(AddressOf GetA15, 1, -2, True)
		  'AddTerm(AddressOf GetA16, 2, -2, True)
		  'AddTerm(AddressOf GetA17, 3, -2, True)
		  'AddTerm(AddressOf GetA18, 0, 2, True)
		  'AddTerm(AddressOf GetA19, 1, 2, True)
		  'AddTerm(AddressOf GetA20, 2, 2, True)
		  'AddTerm(AddressOf GetA21, 3, 2, True)
		  'AddTerm(AddressOf GetA22, 0, 0, False)
		  'AddTerm(AddressOf GetA23, 2, 2, False)
		  'AddTerm(AddressOf GetA24, 1, 2, False)
		  'AddTerm(AddressOf GetA25, 3, 2, False)
		  'AddTerm(AddressOf GetA26, 1, -2, False)
		  'AddTerm(AddressOf GetA27, 2, -2, False)
		  'AddTerm(AddressOf GetA28, 3, -2, False)
		  'AddTerm(AddressOf GetA29, 2, 0, False)
		  'AddTerm(AddressOf GetA30, 0, 2, False)
		  'AddTerm(AddressOf GetA31, 3, 0, False)
		  'AddTerm(AddressOf GetA32, 1, 0, False)
		  'AddTerm(AddressOf GetA33, 1, 0, True)
		  'AddTerm(AddressOf GetA34, 2, 0, True)
		  'AddTerm(AddressOf GetA35, 3, 0, True)
		  'AddTerm(AddressOf GetA36, 1, -2, True)
		  'AddTerm(AddressOf GetA37, 2, -2, True)
		  'AddTerm(AddressOf GetA38, 3, -2, True)
		  'AddTerm(AddressOf GetA39, 0, 2, True)
		  'AddTerm(AddressOf GetA40, 1, 2, True)
		  'AddTerm(AddressOf GetA41, 2, 2, True)
		  'AddTerm(AddressOf GetA42, 3, 2, True)
		  '
		  '1 Return AP.χsy*(2*AP.C2^3*AP.Sβ-AP.η*AP.C2^3*AP.Sβ)
		  '2 Return AP.χsy*(AP.η*AP.C1^4*(-5*AP.Sβ/3 + 2*AP.C2*AP.Sβ/3)+AP.C1^4*(-14*AP.Sβ/3+20*AP.C2*AP.Sβ/3))
		  '3 Return AP.χsy*(-20/3*AP.Cβ*AP.C1^5*AP.S1-2/3*AP.η*AP.Cβ*AP.C1^5*AP.S1)
		  '4 Return AP.χsy*(AP.η*AP.C1^3*(7*AP.Cβ/3+AP.Cβ*AP.C2/3)*AP.S1+AP.C1^3*(-2*AP.Cβ/3+10*AP.Cβ*AP.C2/3)*AP.S1)
		  '5 Return AP.χsy*(AP.η*AP.C1*(7*AP.Cβ/3 - AP.Cβ*AP.C2/3)*AP.S1^3 + AP.C1*(-2*AP.Cβ/3-10*AP.Cβ*AP.C2/3)*AP.S1^3)
		  '6 Return AP.χsy*(AP.η*(5*AP.Sβ/3+2*AP.C2*AP.Sβ/3)*AP.S1^4+(14*AP.Sβ/3+20*AP.C2*AP.Sβ/3)*AP.S1^4)
		  '7 Return AP.χsy*(-20/3*AP.Cβ*AP.C1*AP.S1^5-2/3*AP.η*AP.Cβ*AP.C1*AP.S1^5)
		  '8 Return AP.χsy*(2*AP.C2*AP.Sβ*AP.S2^2-AP.η*AP.C2*AP.Sβ*AP.S2^2)
		  '9 Return AP.χsy*(10/3*AP.C2*AP.Sβ*AP.S2^2+1/3*AP.η*AP.C2*AP.Sβ*AP.S2^2)
		  '10 Return AP.χsy*(-AP.Cβ*AP.S2^3+1/2*AP.η*AP.Cβ*AP.S2^3)
		  '11 Return AP.χsy*(-5/4*AP.Cβ*AP.S2-1/4*AP.Cβ*AP.S6+AP.η*(5*AP.Cβ*AP.S2/8+1/8*AP.Cβ*AP.S6))
		  '12 Return AP.χsx*((-3*AP.Cβ/2-1/2*AP.Cβ*AP.C4)*AP.S2+AP.η*(3*AP.Cβ+1/4*AP.Cβ*AP.C4)*AP.S2)+AP.χsz*(-2*AP.C4*AP.Sβ*AP.S2+AP.η*AP.C4*AP.Sβ*AP.S2)
		  '13 Return AP.χsz*(2*AP.Cβ*AP.C2*AP.S2^2-AP.η*AP.Cβ*AP.C2*AP.S2^2)+AP.χsx*(-2*AP.C2*AP.Sβ*AP.S2^2+AP.η*AP.C2*AP.Sβ*AP.S2^2)
		  '14 Return AP.χsx*(AP.Cβ*AP.S2^3-1/2*AP.η*AP.Cβ*AP.S2^3)
		  '15 Return AP.χsx*(AP.C1*(-2*AP.Cβ/3-10*AP.Cβ*AP.C2/3)*AP.S1^3+AP.η*AP.C1*(-5/3*AP.Cβ+4*AP.C3β-AP.Cβ*AP.C2/3)*AP.S1^3)+AP.χsz*(AP.C1*(-4*AP.Sβ-40*AP.C2*AP.Sβ/3)*AP.S1^3+AP.η*AP.C1*(-2*AP.Sβ-4*AP.C2*AP.Sβ/3-4*AP.S3β)*AP.S1^3)
		  '16 Return AP.χsz*(AP.η*(5*AP.Cβ+AP.C3β+2*AP.Cβ*AP.C2/3)*AP.S1^4+(4*AP.Cβ+20*AP.Cβ*AP.C2/3)*AP.S1^4)+AP.χsx*((-14*AP.Sβ/3-20*AP.C2*AP.Sβ/3)*AP.S1^4+AP.η*(10*AP.Sβ/3-2*AP.C2*AP.Sβ/3+AP.S3β)*AP.S1^4)
		  '17 Return AP.χsx*(20/3*AP.Cβ*AP.C1*AP.S1^5+2/3*AP.η*AP.Cβ*AP.C1*AP.S1^5)
		  '18 Return -6*AP.β*AP.χsz*AP.Cβ*AP.Sβ^2*AP.S2^2 + AP.χsx*(1/3*AP.Sβ*AP.S2^2+AP.η*(-7/6+3*AP.C2β)*AP.Sβ*AP.S2^2)
		  '19 Return AP.χsx*(AP.η*AP.C1^3*(-5*AP.Cβ/3+4*AP.C3β+AP.Cβ*AP.C2/3)*AP.S1+AP.C1^3*(-2*AP.Cβ/3+10*AP.Cβ*AP.C2/3)*AP.S1)+AP.χsz*(AP.C1^3*(-4*AP.Sβ+40*AP.C2*AP.Sβ/3)*AP.S1+AP.η*AP.C1^3*(-2*AP.Sβ+4*AP.C2*AP.Sβ/3-4*AP.S3β)*AP.S1)
		  '20 Return AP.χsz*(AP.η*AP.C1^4*(-5*AP.Cβ-AP.C3β+2*AP.Cβ*AP.C2/3)+AP.C1^4*(-4*AP.Cβ+20*AP.Cβ*AP.C2/3))+AP.χsx*(AP.C1^4*(14*AP.Sβ/3-20*AP.C2*AP.Sβ/3)+AP.η*AP.C1^4*(-10*AP.Sβ/3-2*AP.C2*AP.Sβ/3-AP.S3β))
		  '21 Return AP.χsx*(20/3*AP.Cβ*AP.C1^5*AP.S1+2/3*AP.η*AP.Cβ*AP.C1^5*AP.S1)
		  '22 Return Parameters.δ*(2*AP.χay*AP.C2^3*AP.Sβ)
		  '23 Return Parameters.δ*(AP.χay*AP.C1^4*(-14/3*AP.Sβ+20/3*AP.C2*AP.Sβ))
		  '24 Return Parameters.δ*(AP.χay*AP.C1^3*(-2/3*AP.Cβ+10/3*AP.Cβ*AP.C2)*AP.S1)
		  '25 Return Parameters.δ*(-20/3*AP.χay*AP.Cβ*AP.C1^5*AP.S1)
		  '26 Return Parameters.δ*(AP.χay*AP.C1*(-2/3*AP.Cβ-10/3*AP.Cβ*AP.C2)*AP.S1^3)
		  '27 Return Parameters.δ*(AP.χay*(14/3*AP.Sβ+20/3*AP.C2*AP.Sβ)*AP.S1^4)
		  '29 Return Parameters.δ*(2*AP.χay*AP.C2*AP.Sβ*AP.S2^2)
		  '30 Return Parameters.δ*(10/3*AP.χay*AP.C2*AP.Sβ*AP.S2^2)
		  '31 Return Parameters.δ*(-AP.χay*AP.Cβ*AP.S1^3)
		  '32 Return Parameters.δ*(AP.χay*(-5/4*AP.Cβ*AP.S2-1/4*AP.Cβ*AP.S6))
		  '33 Return Parameters.δ*(AP.χax*(-3/2*AP.Cβ-AP.Cβ*AP.C4/2)*AP.S2-2*AP.χaz*AP.C4*AP.Sβ*AP.S2)
		  '34 Return Parameters.δ*(2*AP.χaz*AP.Cβ*AP.C2*AP.S2^2-2*AP.χax*AP.C2*AP.Sβ*AP.S2^2)
		  '35 Return Parameters.δ*(AP.χax*AP.Cβ*AP.S2^3)
		  '36 Return Parameters.δ*(AP.χax*AP.C1*(-2/3*AP.Cβ-10/3*AP.Cβ*AP.C2)*AP.S1^3+AP.χaz*AP.C1*(-4*AP.Sβ-40/3*AP.C2*AP.Sβ)*AP.S1^3)
		  '37 Return Parameters.δ*(AP.χaz*(4*AP.Cβ+20/3*AP.Cβ*AP.C2)*AP.S1^4-AP.χax*(14/3*AP.Sβ+20/3*AP.C2*AP.Sβ)*AP.S1^4)
		  '38 Return Parameters.δ*(20/3*AP.χax*AP.Cβ*AP.C1*AP.S1^5)
		  '39 Return Parameters.δ*(1/3*AP.χax*AP.Sβ*AP.S2^2)
		  '40 Return Parameters.δ*(AP.χax*AP.C1^3*(-2/3*AP.Cβ+10/3*AP.Cβ*AP.C2)*AP.S1+AP.χaz*AP.C1^3*(-4*AP.Sβ+40/3*AP.C2*AP.Sβ)*AP.S1)
		  '41 Return Parameters.δ*(AP.χaz*AP.C1^4*(-4*AP.Cβ+20/3*AP.Cβ*AP.C2)+AP.χax*AP.C1^4*(14/3*AP.Sβ-20/3*AP.C2*AP.Sβ))
		  '42 Return Parameters.δ*(20/3*AP.χax*AP.Cβ*AP.C1^5*AP.S1)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateSinCosArrays()
		  // Create some local variables for the center case angular functions
		  Var Cα As Double = APSet(0).Cα
		  Var C2α As Double = APSet(0).C2α
		  Var C3α As Double = APSet(0).C3α
		  Var C4α As Double = APSet(0).C4α
		  Var C5α As Double = APSet(0).C5α
		  Var Sα As Double = APSet(0).Sα
		  Var S2α As Double = APSet(0).S2α
		  Var S3α As Double = APSet(0).S3α
		  Var S4α As Double = APSet(0).S4α
		  Var S5α As Double = APSet(0).S5α
		  Var CΨ As Double = APSet(0).CΨ
		  Var C2Ψ As Double = APSet(0).C2Ψ
		  Var C3Ψ As Double = APSet(0).C3Ψ
		  Var C4Ψ As Double = APSet(0).C4Ψ
		  Var C5Ψ As Double = APSet(0).C5Ψ
		  Var SΨ As Double = APSet(0).SΨ
		  Var S2Ψ As Double = APSet(0).S2Ψ
		  Var S3Ψ As Double = APSet(0).S3Ψ
		  Var S4Ψ As Double = APSet(0).S4Ψ
		  Var S5Ψ As Double = APSet(0).S5Ψ
		  
		  // Store all possible cosines for linear combinations of α and Ψr
		  WaveFor(0,0,0) = C5Ψ
		  WaveFor(0,0,1) = C4Ψ
		  WaveFor(0,0,2) = C3Ψ
		  WaveFor(0,0,3) = C2Ψ
		  WaveFor(0,0,4) = CΨ
		  WaveFor(0,0,5) = 1.0
		  WaveFor(0,0,6) = CΨ
		  WaveFor(0,0,7) = C2Ψ
		  WaveFor(0,0,8) = C3Ψ
		  WaveFor(0,0,9) = C4Ψ
		  WaveFor(0,0,10) = C5Ψ
		  WaveFor(0,1,0) = Cα*C5Ψ + Sα*S5Ψ
		  WaveFor(0,1,1) = Cα*C4Ψ + Sα*S4Ψ
		  WaveFor(0,1,2) = Cα*C3Ψ + Sα*S3Ψ
		  WaveFor(0,1,3) = Cα*C2Ψ + Sα*S2Ψ
		  WaveFor(0,1,4) = Cα*CΨ + Sα*SΨ
		  WaveFor(0,1,5) = Cα
		  WaveFor(0,1,6) = Cα*CΨ - Sα*SΨ
		  WaveFor(0,1,7) = Cα*C2Ψ - Sα*S2Ψ
		  WaveFor(0,1,8) = Cα*C3Ψ - Sα*S3Ψ
		  WaveFor(0,1,9) = Cα*C4Ψ - Sα*S4Ψ
		  WaveFor(0,1,10) = Cα*C5Ψ - Sα*S5Ψ
		  WaveFor(0,2,0) = C2α*C5Ψ + S2α*S5Ψ
		  WaveFor(0,2,1) = C2α*C4Ψ + S2α*S4Ψ
		  WaveFor(0,2,2) = C2α*C3Ψ + S2α*S3Ψ
		  WaveFor(0,2,3) = C2α*C2Ψ + S2α*S2Ψ
		  WaveFor(0,2,4) = C2α*CΨ + S2α*SΨ
		  WaveFor(0,2,5) = C2α
		  WaveFor(0,2,6) = C2α*CΨ - S2α*SΨ
		  WaveFor(0,2,7) = C2α*C2Ψ - S2α*S2Ψ
		  WaveFor(0,2,8) = C2α*C3Ψ - S2α*S3Ψ
		  WaveFor(0,2,9) = C2α*C4Ψ - S2α*S4Ψ
		  WaveFor(0,2,10) = C2α*C5Ψ - S2α*S5Ψ
		  WaveFor(0,3,0) = C3α*C5Ψ + S3α*S5Ψ
		  WaveFor(0,3,1) = C3α*C4Ψ + S3α*S4Ψ
		  WaveFor(0,3,2) = C3α*C3Ψ + S3α*S3Ψ
		  WaveFor(0,3,3) = C3α*C2Ψ + S3α*S2Ψ
		  WaveFor(0,3,4) = C3α*CΨ + S3α*SΨ
		  WaveFor(0,3,5) = C3α
		  WaveFor(0,3,6) = C3α*CΨ - S3α*SΨ
		  WaveFor(0,3,7) = C3α*C2Ψ - S3α*S2Ψ
		  WaveFor(0,3,8) = C3α*C3Ψ - S3α*S3Ψ
		  WaveFor(0,3,9) = C3α*C4Ψ - S3α*S4Ψ
		  WaveFor(0,3,10) = C3α*C5Ψ - S3α*S5Ψ
		  WaveFor(0,4,0) = C4α*C5Ψ + S4α*S5Ψ
		  WaveFor(0,4,1) = C4α*C4Ψ + S4α*S4Ψ
		  WaveFor(0,4,2) = C4α*C3Ψ + S4α*S3Ψ
		  WaveFor(0,4,3) = C4α*C2Ψ + S4α*S2Ψ
		  WaveFor(0,4,4) = C4α*CΨ + S4α*SΨ
		  WaveFor(0,4,5) = C4α
		  WaveFor(0,4,6) = C4α*CΨ - S4α*SΨ
		  WaveFor(0,4,7) = C4α*C2Ψ - S4α*S2Ψ
		  WaveFor(0,4,8) = C4α*C3Ψ - S4α*S3Ψ
		  WaveFor(0,4,9) = C4α*C4Ψ - S4α*S4Ψ
		  WaveFor(0,4,10) = C4α*C5Ψ - S4α*S5Ψ
		  WaveFor(0,5,0) = C5α*C5Ψ + S5α*S5Ψ
		  WaveFor(0,5,1) = C5α*C4Ψ + S5α*S4Ψ
		  WaveFor(0,5,2) = C5α*C3Ψ + S5α*S3Ψ
		  WaveFor(0,5,3) = C5α*C2Ψ + S5α*S2Ψ
		  WaveFor(0,5,4) = C5α*CΨ + S5α*SΨ
		  WaveFor(0,5,5) = C5α
		  WaveFor(0,5,6) = C5α*CΨ - S5α*SΨ
		  WaveFor(0,5,7) = C5α*C2Ψ - S5α*S2Ψ
		  WaveFor(0,5,8) = C5α*C3Ψ - S5α*S3Ψ
		  WaveFor(0,5,9) = C5α*C4Ψ - S5α*S4Ψ
		  WaveFor(0,5,10) = C5α*C5Ψ - S5α*S5Ψ
		  
		  // Do the same for sines
		  WaveFor(1,0,0) = -S5Ψ
		  WaveFor(1,0,1) = -S4Ψ
		  WaveFor(1,0,2) = -S3Ψ
		  WaveFor(1,0,3) = -S2Ψ
		  WaveFor(1,0,4) = -SΨ
		  WaveFor(1,0,5) = 1.0
		  WaveFor(1,0,6) = SΨ
		  WaveFor(1,0,7) = S2Ψ
		  WaveFor(1,0,8) = S3Ψ
		  WaveFor(1,0,9) = S4Ψ
		  WaveFor(1,0,10) = S5Ψ
		  WaveFor(1,1,0) = Sα*C5Ψ - Cα*S5Ψ
		  WaveFor(1,1,1) = Sα*C4Ψ - Cα*S4Ψ
		  WaveFor(1,1,2) = Sα*C3Ψ - Cα*S3Ψ
		  WaveFor(1,1,3) = Sα*C2Ψ - Cα*S2Ψ
		  WaveFor(1,1,4) = Sα*CΨ - Cα*SΨ
		  WaveFor(1,1,5) = Sα
		  WaveFor(1,1,6) = Sα*CΨ + Cα*SΨ
		  WaveFor(1,1,7) = Sα*C2Ψ + Cα*S2Ψ
		  WaveFor(1,1,8) = Sα*C3Ψ + Cα*S3Ψ
		  WaveFor(1,1,9) = Sα*C4Ψ + Cα*S4Ψ
		  WaveFor(1,1,10) = Sα*C5Ψ + Cα*S5Ψ
		  WaveFor(1,2,0) = S2α*C5Ψ - C2α*S5Ψ
		  WaveFor(1,2,1) = S2α*C4Ψ - C2α*S4Ψ
		  WaveFor(1,2,2) = S2α*C3Ψ - C2α*S3Ψ
		  WaveFor(1,2,3) = S2α*C2Ψ - C2α*S2Ψ
		  WaveFor(1,2,4) = S2α*CΨ - C2α*SΨ
		  WaveFor(1,2,5) = S2α
		  WaveFor(1,2,6) = S2α*CΨ + C2α*SΨ
		  WaveFor(1,2,7) = S2α*C2Ψ + C2α*S2Ψ
		  WaveFor(1,2,8) = S2α*C3Ψ + C2α*S3Ψ
		  WaveFor(1,2,9) = S2α*C4Ψ + C2α*S4Ψ
		  WaveFor(1,2,10) = S2α*C5Ψ + C2α*S5Ψ
		  WaveFor(1,3,0) = S3α*C5Ψ - C3α*S5Ψ
		  WaveFor(1,3,1) = S3α*C4Ψ - C3α*S4Ψ
		  WaveFor(1,3,2) = S3α*C3Ψ - C3α*S3Ψ
		  WaveFor(1,3,3) = S3α*C2Ψ - C3α*S2Ψ
		  WaveFor(1,3,4) = S3α*CΨ - C3α*SΨ
		  WaveFor(1,3,5) = S3α
		  WaveFor(1,3,6) = S3α*CΨ + C3α*SΨ
		  WaveFor(1,3,7) = S3α*C2Ψ + C3α*S2Ψ
		  WaveFor(1,3,8) = S3α*C3Ψ + C3α*S3Ψ
		  WaveFor(1,3,9) = S3α*C4Ψ + C3α*S4Ψ
		  WaveFor(1,3,10) = S3α*C5Ψ + C3α*S5Ψ
		  WaveFor(1,4,0) = S4α*C5Ψ - C4α*S5Ψ
		  WaveFor(1,4,1) = S4α*C4Ψ - C4α*S4Ψ
		  WaveFor(1,4,2) = S4α*C3Ψ - C4α*S3Ψ
		  WaveFor(1,4,3) = S4α*C2Ψ - C4α*S2Ψ
		  WaveFor(1,4,4) = S4α*CΨ - C4α*SΨ
		  WaveFor(1,4,5) = S4α
		  WaveFor(1,4,6) = S4α*CΨ + C4α*SΨ
		  WaveFor(1,4,7) = S4α*C2Ψ + C4α*S2Ψ
		  WaveFor(1,4,8) = S4α*C3Ψ + C4α*S3Ψ
		  WaveFor(1,4,9) = S4α*C4Ψ + C4α*S4Ψ
		  WaveFor(1,4,10) = S4α*C5Ψ + C4α*S5Ψ
		  WaveFor(1,5,0) = S5α*C5Ψ - C5α*S5Ψ
		  WaveFor(1,5,1) = S5α*C4Ψ - C5α*S4Ψ
		  WaveFor(1,5,2) = S5α*C3Ψ - C5α*S3Ψ
		  WaveFor(1,5,3) = S5α*C2Ψ - C5α*S2Ψ
		  WaveFor(1,5,4) = S5α*CΨ - C5α*SΨ
		  WaveFor(1,5,5) = S5α
		  WaveFor(1,5,6) = S5α*CΨ + C5α*SΨ
		  WaveFor(1,5,7) = S5α*C2Ψ + C5α*S2Ψ
		  WaveFor(1,5,8) = S5α*C3Ψ + C5α*S3Ψ
		  WaveFor(1,5,9) = S5α*C4Ψ + C5α*S4Ψ
		  WaveFor(1,5,10) = S5α*C5Ψ + C5α*S5Ψ
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		A(18,44) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		APSet(18) As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		DArray(14) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DC1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DC2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DC2σ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DC2σ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DC2Φ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DC3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCCos2Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCCosΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCCosσ1x9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCCosσ2x9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCHalfCos2ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCHalfSin2ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCSin2Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCSinΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCSinσ1x9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCSinσ2x9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Derivs As CurrentDerivativesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		DH As DerivativeSet
	#tag EndProperty

	#tag Property, Flags = &h0
		DHP As DerivativeSet
	#tag EndProperty

	#tag Property, Flags = &h0
		DHX As DerivativeSet
	#tag EndProperty

	#tag Property, Flags = &h0
		H0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		H0V2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IndexForι As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		IndexForβ As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		IndexForδ As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		IndexForχax As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		IndexForχay As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		IndexForχaz As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		IndexForχsx As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		IndexForχsy As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		IndexForχsz As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Inverse2ει As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Inverse2εβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Inverse2εδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Inverse2εχax As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Inverse2εχay As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Inverse2εχaz As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Inverse2εχsx As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Inverse2εχsy As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Inverse2εχsz As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		NCvS(44) As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Noise As NoiseClass
	#tag EndProperty

	#tag Property, Flags = &h0
		NTerms As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Nα(44) As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		NΨ(44) As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Sn2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SnR(6) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Values As CurrentValuesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		WaveFor(1,5,10) As Double
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
			Name="H0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="H0V2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="V3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DC2σ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DC2σ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCSinσ1x9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCSinσ2x9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCHalfSin2ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCHalfCos2ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DC3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DC1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DC2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCSinΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCSin2Θ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCCos2Θ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCCosΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCCosσ2x9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCCosσ1x9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DC2Φ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sn2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="NTerms"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DDp1DΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DDp1DΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DDp2DΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DDp2DΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DDx1DΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DDx1DΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DDx2DΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DDx2DΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C364R3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C164R3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C132R3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C316"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C38"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DPlus1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DPlus2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCross1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCross2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SinΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sin2Θ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Cos2Θ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CosΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Cos2σ2Minus2Φx9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Cos2σ1Minus2Φx9"
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
			Name="HPAdjusted"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="HXAdjusted"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
