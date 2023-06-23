#tag Class
Protected Class CaseSupervisorClass
	#tag Method, Flags = &h0
		Sub Constructor(myM As Double, myδ As Double, myf0 As Double, myR As Double, myβ As Double, myψangle As Double, myλ0 As Double, myΘ As Double, myΦ As Double, χ10 As Vector, χ20 As Vector, myPNOrder As Integer, myDet As Integer, mydτ0 As Double, myK As Double)
		  '// The Main class is the overhead of the program. It takes and stores the initial values from the main window. It then initializes 
		  '// all necessary parameters, and creates an instance of the EvolverClass class to handle the phase evolution. Then, it performs steps
		  '// of constant size, requesting values from the EvolverClass class after each step. Also at each step, it handles the waveform amplitude
		  '// and derivatives, and it loads these derivatives into the ATA matrix. This matrix is the final product of the class.
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
		  'Var χ1 As Vector = χ10
		  'Var χ2 As Vector = χ20
		  ''z = myz
		  'PNOrder = myPNOrder
		  'nDetectors = myDet
		  'dτ0 = mydτ0              
		  'K = myK  // constant necessary for the detector functions
		  '
		  'If χ1.x = 0 And χ1.y = 0 And χ1.z = 0 Then
		  'noSpin1 = True
		  'End If
		  '
		  'If χ2.x = 0 And χ2.y = 0 And χ2.z = 0 Then
		  'noSpin2 = True
		  'End If
		  '
		  'Evolver = New EvolverClass(M, δ, f0, R, β, ψangle, λ0, Θ, Φ, χ1, χ2, dτ0)      //Create the new instance of the EvolverClass class, giving it the necessary parameters from initial conditions
		  '
		  'π = 3.141592653589793238
		  'tpot = 2*π/3
		  'f2 = 2*sqrt(0.75) // constant for detector functions
		  'η = (1-δ^2)/4
		  'v0 = (myf0* 1e-3 * M * 2 * π) ^ (1/3)
		  '
		  '// Some trig quantities needed for the waveform derivatives
		  'cosβ = Cos(β)
		  'sinβ = Sin(β)
		  'cosΘ = Cos(Θ)
		  'sinΘ = Sin(Θ)
		  '
		  '// Some partial derivatives needed for chain rules
		  'dχ1ℓdχ10x = Evolver.dχ1ℓdχ10x
		  'dχ1ℓdχ10y = Evolver.dχ1ℓdχ10y
		  'dχ1ℓdχ10z = Evolver.dχ1ℓdχ10z
		  'dχ1ℓdχ20x = Evolver.dχ1ℓdχ20x
		  'dχ1ℓdχ20y = Evolver.dχ1ℓdχ20y
		  'dχ1ℓdχ20z = Evolver.dχ1ℓdχ20z
		  '
		  'dχ2ℓdχ10x = Evolver.dχ2ℓdχ10x
		  'dχ2ℓdχ10y = Evolver.dχ2ℓdχ10y
		  'dχ2ℓdχ10z = Evolver.dχ2ℓdχ10z
		  'dχ2ℓdχ20x = Evolver.dχ2ℓdχ20x
		  'dχ2ℓdχ20y = Evolver.dχ2ℓdχ20y
		  'dχ2ℓdχ20z = Evolver.dχ2ℓdχ20z
		  '
		  '//Also initialize clock counters
		  'τMain = 0             //τMainN is a "clock" storing current time in the main program 
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DetectorFunctions(hdrvtvs() As Double)
		  '// This method takes an array of h-derivatives from hDrvtvs and performs the calculations of the detector functions to produce
		  '// our received waveform and derivatives. It then loads these derivatives into the ATA matrix, which is later inverted to calculate 
		  '// uncertainties. 
		  '
		  '// Store the array values in variables
		  'hp = hdrvtvs(0)
		  'Var dhpdM As  Double = hdrvtvs(1)
		  'Var dhpdδ As Double = hdrvtvs(2)
		  'Var dhpdf0 As Double = hdrvtvs(3)
		  'Var dhpdR As Double = hdrvtvs(4)
		  'Var dhpdβ As Double = hdrvtvs(5)
		  'Var dhpdλ0 As Double = hdrvtvs(6)
		  'Var dhpdΘ As Double = hdrvtvs(7)
		  'Var dhpdΦ As Double = hdrvtvs(8)
		  'Var dhpdχ1ℓ As Double = hdrvtvs(9)
		  'Var dhpdχ2ℓ As Double = hdrvtvs(10)
		  '
		  'hc = hdrvtvs(11)
		  'Var dhcdM As Double = hdrvtvs(12)
		  'Var dhcdδ As Double = hdrvtvs(13)
		  'Var dhcdf0 As Double = hdrvtvs(14)
		  'Var dhcdR As Double = hdrvtvs(15)
		  'Var dhcdβ As Double = hdrvtvs(16)
		  'Var dhcdλ0 As Double = hdrvtvs(17)
		  'Var dhcdΘ As Double = hdrvtvs(18)
		  'Var dhcdΦ As Double = hdrvtvs(19)
		  'Var dhcdχ1ℓ As Double = hdrvtvs(20)
		  'Var dhcdχ2ℓ As Double = hdrvtvs(21)
		  '
		  '// Calculate the q-functions, which will help to simplify later calculations
		  'Var c2ψ As Double = 0.5*Cos(2*oldψangle)
		  'Var s2ψ As Double = 0.5* Sin(2*oldψangle)
		  '
		  'Var qp As Double = hp*c2ψ + hc*s2ψ
		  'Var qc As Double = -hp*s2ψ + hc*c2ψ 
		  '
		  'Var qpM As Double = dhpdM*c2ψ + dhpdM*s2ψ
		  'Var qcM As Double = -dhcdM*s2ψ + dhcdM*c2ψ 
		  'Var qpδ As Double = dhpdδ*c2ψ + dhpdδ*s2ψ
		  'Var qcδ As Double = -dhcdδ*s2ψ + dhcdδ*c2ψ 
		  'Var qpf0 As Double = dhpdf0*c2ψ + dhpdf0*s2ψ
		  'Var qcf0 As Double = -dhcdf0*s2ψ + dhcdf0*c2ψ 
		  'Var qpR As Double = dhpdR*c2ψ + dhpdR*s2ψ
		  'Var qcR As Double = -dhcdR*s2ψ + dhcdR*c2ψ 
		  'Var qpβ As Double = dhpdβ*c2ψ + dhpdβ*s2ψ
		  'Var qcβ As Double = -dhcdβ*s2ψ + dhcdβ*c2ψ 
		  'Var qpλ0 As Double = dhpdλ0*c2ψ + dhpdλ0*s2ψ
		  'Var qcλ0 As Double = -dhcdλ0*s2ψ + dhcdλ0*c2ψ 
		  'Var qpΘ As Double = dhpdΘ*c2ψ + dhpdΘ*s2ψ
		  'Var qcΘ As Double = -dhcdΘ*s2ψ + dhcdΘ*c2ψ 
		  'Var qpΦ As Double = dhpdΦ*c2ψ + dhpdΦ*s2ψ
		  'Var qcΦ As Double = -dhcdΦ*s2ψ + dhcdΦ*c2ψ 
		  'Var qpχ1ℓ As Double = dhpdχ1ℓ*c2ψ + dhpdχ1ℓ*s2ψ
		  'Var qcχ1ℓ As Double = -dhcdχ1ℓ*s2ψ + dhcdχ1ℓ*c2ψ 
		  'Var qpχ2ℓ As Double = dhpdχ2ℓ*c2ψ + dhpdχ2ℓ*s2ψ
		  'Var qcχ2ℓ As Double = -dhcdχ2ℓ*s2ψ + dhcdχ2ℓ*c2ψ 
		  '
		  '// calculate time-dependent quantities (alpha, alambda, phidet)
		  'Var αDet As Double = Ω*τMain + K // Note that instead of assuming K is zero (as in the detector function document), we are assuming the alpha0 constant is zero 
		  '// and instead are adding K to this quantity (these angles are only different by a constant amount, so we can define that constant
		  '// amount wherever we want to. Defining it here simplifies the detector loop.          
		  'Var λDet As Double = tpot // 2*π/3
		  'Var ΦDet As Double = Φ
		  '
		  '// loop through the detectors. For the first detector, we can define lambda = 0
		  'For k As Integer = 1 to nDetectors
		  '
		  '// calculate the necessary sin and cos terms
		  'Var s2α2λ As Double = Sin(2*αDet)
		  'Var c2α2λ As Double = Cos(2*αDet)
		  'Var s2λ2Φ As Double = Sin(2*ΦDet)
		  'Var c2λ2Φ As Double = Cos(2*ΦDet)
		  'Var s4α2λ2Φ As Double = Sin(4*αDet - 2*ΦDet)
		  'Var CaptionForα2λ2Φ As Double = Cos(4*αDet - 2*ΦDet)
		  'Var s3α2λ1Φ As Double = Sin(3*αDet - ΦDet)
		  'Var c3α2λ1Φ As Double = Cos(3*αDet - ΦDet)
		  'Var s1α2λ1Φ As Double = Sin(αDet + ΦDet)
		  'Var c1α2λ1Φ As Double = Cos(αDet + ΦDet)
		  '
		  '// simplification terms to condense the detector functions
		  'Var st1 as Double = 18*s2α2λ - 9*s2λ2Φ - s4α2λ2Φ
		  'Var st2 as Double = 3*s1α2λ1Φ - s3α2λ1Φ
		  'Var st3 as Double = 9*s2λ2Φ + s4α2λ2Φ + 6*s2α2λ
		  'Var st4 as Double = -9*c2λ2Φ + CaptionForα2λ2Φ
		  'Var st5 as Double = 3*c1α2λ1Φ + c3α2λ1Φ
		  '
		  '// calculate D-functions
		  'Var dp As Double = (f2/64)*(cosΘ*cosΘ - sinΘ*sinΘ)*st1 + (3/8)*cosΘ*sinΘ*st2 - 3*(f2/64)*st3
		  'Var dc As Double = (f2/16)*cosΘ*(9*c2λ2Φ - CaptionForα2λ2Φ) - (3/8)*sinΘ*(c3α2λ1Φ + 3*c1α2λ1Φ)
		  'Var dpΘ As Double = -(f2/16)*cosΘ*sinΘ*st1 + (3/8)*(cosΘ*cosΘ - sinΘ*sinΘ)*st2
		  'Var dpΦ As Double = (f2/32)*(cosΘ*cosΘ - sinΘ*sinΘ)*st4 + (3/8)*cosΘ*sinΘ*st5 + 3*(f2/32)*st4
		  'Var dcΘ As Double = (f2/16)*sinΘ*st4 - (3/8)*cosΘ*st5
		  'Var dcΦ As Double = -(f2/8)*cosΘ*(9*s2λ2Φ + s4α2λ2Φ) + (3/8)*sinΘ*st2
		  '
		  '// compute the detected waveform
		  'h = qp*dp + qc*dc
		  '
		  '// compute the partials of the detected wave
		  'Var sqsn2 As Double = sqrt(sn2)
		  'Var dzdM As Double = qpM*dp + qcM*dc
		  'dzdM = (dzdM*M)/(sqsn2) // normalize to percent uncertainty
		  'Var dzdδ As Double = (qpδ*dp + qcδ*dc)/sqsn2
		  'Var dzdf0 As Double = (qpf0*dp + qcf0*dc)/sqsn2
		  'Var dzdR As Double = qpr*dp + qcr*dc
		  'dzdR = (dzdR*R)/sqsn2 // normalize to percent uncertainty
		  'Var dzdβ As Double = (qpβ*dp + qcβ*dc)/sqsn2
		  'Var dzdψ As Double = (2*qc*dp - 2*qp*dc)/sqsn2 // based on the equations for F in the detector function document. 
		  '// Note: our psi is different from that document's psi by 90º, but that is handled in polarization and derivatives should remain the same
		  'Var dzdλ0 As Double = (qpλ0*dp + qcλ0*dc)/sqsn2
		  'Var dzdΘ As Double = (qpΘ*dp + qp*dpΘ + qcΘ*dc + qc*dcΘ)/sqsn2
		  'Var dzdΦ As Double = (qpΦ*dp + qp*dpΦ + qcΦ*dc + qc*dcΦ)/sqsn2
		  '// find the χ1ℓ and χ2ℓ derivatives . . .
		  'Var dzdχ1ℓ As Double = (qpχ1ℓ*dp + qcχ1ℓ*dc)/sqsn2
		  'Var dzdχ2ℓ As Double = (qpχ2ℓ*dp + qcχ2ℓ*dc)/sqsn2
		  '// . . . and convert them to χ10x, etc. derivatives
		  'Var dzdχ10x As Double = dzdχ1ℓ*dχ1ℓdχ10x
		  'Var dzdχ10y As Double = dzdχ1ℓ*dχ1ℓdχ10y
		  'Var dzdχ10z As Double = dzdχ1ℓ*dχ1ℓdχ10z
		  'Var dzdχ20x As Double = dzdχ2ℓ*dχ2ℓdχ20x
		  'Var dzdχ20y As Double = dzdχ2ℓ*dχ2ℓdχ20y
		  'Var dzdχ20z As Double = dzdχ2ℓ*dχ2ℓdχ20z
		  '
		  '// Ali does "stopcheck" stuff here--necessary when we add error handling
		  '
		  '// load the derivatives into an array
		  'dzd(0) = dzdM
		  'dzd(1) = dzdδ
		  'dzd(2) = dzdf0
		  'dzd(3) = dzdR
		  'dzd(4) = dzdβ
		  'dzd(5) = dzdψ
		  'dzd(6) = dzdλ0
		  'dzd(7) = dzdΘ
		  'dzd(8) = dzdΦ
		  'dzd(9) = dzdχ10x
		  'dzd(10) = dzdχ10y
		  'dzd(11) = dzdχ10z
		  'dzd(12) = dzdχ20x
		  'dzd(13) = dzdχ20y
		  'dzd(14) = dzdχ20z
		  '
		  '// and add these to the ATA matrix (the slight change in the matrix for each step)
		  'For i As Integer = 0 to 14
		  'For j As Integer = i to 14 // note that we are only filling in the diagonal and upper right
		  'ATA(i,j) = ATA(i,j) + dzd(i)*dzd(j)
		  'Next
		  'Next
		  '
		  '// finish the loop by subtracting lambda (will introduce a lambda term for the second detector)
		  'αDet = αDet - λDet
		  'ΦDet = ΦDet - λDet
		  '
		  'Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoMainStep()
		  '// This function performs a time step in the main stepping timeline
		  '
		  'τMain = τMain + dτ0  // perform the step
		  '
		  'GetSpinValues  // get the values from EvolverClass
		  '
		  'DetectorFunctions(hDrvtvs(dτ0))  // use these values to compute the derivatives and load those derivatives into the ATA matrix
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetSpinValues()
		  '// This method is called when we need the phase values from the EvolverClass class. It calls EvolverClass's ReturnValues function and
		  '// stores these returned values to properties.
		  '
		  'Var spinValues() As Double = Evolver.ReturnValues(τMain)
		  '
		  'vF = spinValues(0)
		  'dvdv0F = spinValues(1)
		  'dvdδF = spinValues(2)
		  'dvdχ1ℓF = spinValues(3)
		  'dvdχ2ℓF = spinValues(4)
		  'ιF = spinValues(5)
		  'αF = spinValues(6)
		  'dιdv0F = spinValues(7)
		  'dαdv0F = spinValues(8)
		  'dιdδF = spinValues(9)
		  'dαdδF = spinValues(10)
		  'dιdχ1ℓF = spinValues(11)
		  'dιdχ2ℓF = spinValues (12)
		  'dαdχ1ℓF = spinValues(13)
		  'dαdχ2ℓF = spinValues(14)
		  'ψRF= spinValues(15)
		  'dψRdv0F = spinValues(16)
		  'dψRdδF = spinValues(17)
		  'dψRdΘF = spinValues(18)
		  'dψRdΦF = spinValues(19)
		  'dψRdχ1ℓF = spinValues(20)
		  'dψRdχ2ℓF = spinValues(21)
		  'oldψangle = spinValues(22) // note that this is the older version of psi (not our psi), which we need for these computations
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function hDrvtvs(dτ As Double) As Double()
		  '// Calculates the derivatives of the waveform (h) functions. The results of these calculations is fed to the 
		  '// DetectorFunctions method, which then leads to loading up the ATA matrix. This function takes the current
		  '// time step to make the calculation.
		  '
		  '// Calculate noise. Note: when doing higher order terms, will need to calculate other noise terms here
		  'Var f As Double = (vF^3)/(2*π*M)  // Check this value!
		  'Var fn As Double = 0.5*f
		  'Var fn2Noise As New Noise(2*fn, dτ0)
		  'sn2 = fn2Noise.sn
		  '
		  '// calculate phase sin and cos
		  'Var sp2 As Double = Sin(2*ψRF)
		  'Var cp2 As Double = Cos(2*ψRF)
		  '
		  '// Calculate y terms (here is where more y terms would be calculated for higher order)
		  'Var y1 As Double = 2*M*η*(vF^2) / R
		  '
		  '// Store the variables cos/sin(ι), cos/sin(α)
		  'Var cosι As Double = Cos(ιF)
		  'Var sinι As Double = Sin(ιF)
		  'var cosα As Double = Cos(αF)
		  'var sinα As Double = Sin(αF)
		  '
		  '// calculate cos(ζ) (cos of the inclination) and its squared value
		  'Var c1 As Double = sinι*cosα*sinβ - cosι*cosβ
		  'Var c2 As Double = c1*c1
		  'ζ = ACos(c1)  // the inclination
		  '
		  '// calculate derivatives of cos(ζ)
		  'Var dcdM As Double = cosι*(dιdv0F*v0/(3*M))*cosα*sinβ - sinι*sinα*(dαdv0F*v0/(3*M))*sinβ - sinι*(dιdv0F*v0/(3*M))*cosβ
		  'Var dcdδ As Double = cosι*(dιdδF)*cosα*sinβ - sinι*sinα*(dαdδF)*sinβ - sinι*(dιdδF)*cosβ
		  'Var dcdf0 As Double = cosι*(dιdv0F*v0/(3*f0))*cosα*sinβ - sinι*sinα*(dαdv0F*v0/(3*f0))*sinβ - sinι*(dιdv0F*v0/(3*f0))*cosβ
		  'Var dcdβ As Double = sinι*cosα*cosβ - cosι*sinβ
		  'Var dcdχ1ℓ As Double = cosι*(dιdχ1ℓF)*cosα*sinβ - sinι*sinα*(dαdχ1ℓF)*sinβ - sinι*(dιdχ1ℓF)*cosβ
		  'Var dcdχ2ℓ As Double = cosι*(dιdχ2ℓF)*cosα*sinβ - sinι*sinα*(dαdχ2ℓF)*sinβ - sinι*(dιdχ2ℓF)*cosβ
		  '
		  '// Calculate 0-th order (plus and cross) waveform terms
		  'Var h0p As Double = -(1 + c2)*cp2
		  'Var dh0pdM As Double = -2*c1*dcdM*cp2 + (1 + c2)*sp2*2*dψRdv0F*v0/(3*M)
		  'Var dh0pdδ As Double = -2*c1*dcdδ*cp2 + (1 + c2)*sp2*2*dψRdδF
		  'Var dh0pdf0 As Double = -2*c1*dcdf0*cp2 + (1 + c2)*sp2*2*dψRdv0F*v0/(3*f0)
		  'Var dh0pdβ As Double = -2*c1*dcdβ*cp2
		  'Var dh0pdλ0 As Double = (1 + c2)*sp2*2
		  'Var dh0pdΘ As Double = (1 + c2)*sp2*2*dψRdΘF
		  'Var dh0pdΦ As Double = (1 + c2)*sp2*2*dψRdΦF
		  'Var dh0pdχ1ℓ As Double = -2*c1*dcdχ1ℓ*cp2 + (1 + c2)*sp2*2*dψRdχ1ℓF
		  'Var dh0pdχ2ℓ As Double = -2*c1*dcdχ2ℓ*cp2 + (1 + c2)*sp2*2*dψRdχ2ℓF
		  '
		  'Var h0c As Double = -2*c1*sp2
		  'Var dh0cdM As Double = -2*dcdM*sp2 - 2*c1*cp2*2*dψRdv0F*v0/(3*M)
		  'Var dh0cdδ As Double = -2*dcdδ*sp2 - 2*c1*cp2*2*dψRdδF
		  'Var dh0cdf0 As Double = -2*dcdf0*sp2 - 2*c1*cp2*2*dψRdv0F*v0/(3*f0)
		  'Var dh0cdβ As Double = -2*dcdβ*sp2
		  'Var dh0cdλ0 As Double = -2*c1*cp2*2
		  'Var dh0cdΘ As Double = -2*c1*cp2*2*dψRdΘF
		  'Var dh0cdΦ As Double = -2*c1*cp2*2*dψRdΦF
		  'Var dh0cdχ1ℓ As Double = -2*dcdχ1ℓ*sp2 - 2*c1*cp2*2*dψRdχ1ℓF
		  'Var dh0cdχ2ℓ As Double = -2*dcdχ2ℓ*sp2 - 2*c1*cp2*2*dψRdχ2ℓF
		  '
		  '// Here is where higher order terms would be calculated
		  '
		  '// Calculate the full plus and cross polarizations and store them in a return array
		  'Var hdrvtvs(21) As Double
		  'hdrvtvs(0) = y1*h0p //hp
		  'hdrvtvs(1) = (y1/M)*h0p + (2*y1/vF)*(dvdv0F*v0/(3*M))*h0p + y1*dh0pdM //dhpdM
		  'hdrvtvs(2) = (y1/η)*(-δ/2)*h0p + (2*y1/vF)*(dvdδF)*h0p + y1*dh0pdδ //dhpdδ
		  'hdrvtvs(3) = (2*y1/vF)*(dvdv0F*v0/(3*f0))*h0p + y1*dh0pdf0 //dhpdf0
		  'hdrvtvs(4) = (-y1/R)*h0p //dhpdR
		  'hdrvtvs(5) = y1*dh0pdβ //dhpdβ
		  'hdrvtvs(6) = y1*dh0pdλ0 //dhpdλ0
		  'hdrvtvs(7) = y1*dh0pdΘ //dhpdΘ
		  'hdrvtvs(8) = y1*dh0pdΦ //dhpdΦ
		  'hdrvtvs(9) = (2*y1/vF)*dvdχ1ℓF*h0p + y1*dh0pdχ1ℓ //dhpdχ1ℓ
		  'hdrvtvs(10) = (2*y1/vF)*dvdχ2ℓF*h0p + y1*dh0pdχ2ℓ //dhpdχ2ℓ
		  '
		  'hdrvtvs(11) = y1*h0c //hc
		  'hdrvtvs(12) = (y1/M)*h0c + (2*y1/vF)*(dvdv0F*v0/(3*M))*h0c + y1*dh0cdM //dhcdM
		  'hdrvtvs(13) = (y1/η)*(-δ/2)*h0c + (2*y1/vF)*(dvdδF)*h0c + y1*dh0cdδ //dhcdδ
		  'hdrvtvs(14) = (2*y1/vF)*(dvdv0F*v0/(3*f0))*h0c + y1*dh0cdf0 //dhcdf0
		  'hdrvtvs(15) = (-y1/R)*h0c //dhcdR
		  'hdrvtvs(16) = y1*dh0cdβ //dhcdβ
		  'hdrvtvs(17) = y1*dh0cdλ0 //dhcdλ0
		  'hdrvtvs(18) = y1*dh0cdΘ //dhcdΘ
		  'hdrvtvs(19) = y1*dh0cdΦ //dhcdΦ
		  'hdrvtvs(20) = (2*y1/vF)*dvdχ1ℓF*h0c + y1*dh0cdχ1ℓ //dhcdχ1ℓ
		  'hdrvtvs(21) = (2*y1/vF)*dvdχ2ℓF*h0c + y1*dh0cdχ2ℓ //dhcdχ2ℓ
		  '
		  'return hdrvtvs
		  
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function UpdateProgress() As Integer
		  '// This function is called by the timer in the main window to update the progress of the simulation. It computes the progress fraction
		  '// by dividing the passed time by a year.
		  '
		  'Var t As Double = τMain*M 
		  '
		  'Var ProgressFraction As Integer = Round((t/yr)*100)
		  '
		  'Return ProgressFraction
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		ATA As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		CaseParameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		dτ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτIdeal As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		dτReal As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Evolver As EvolverClass
	#tag EndProperty

	#tag Property, Flags = &h0
		HValues As HValuesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		OrbitValues As OrbitValuesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PhaseValues As PhaseValuesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		VValues As VValuesClass
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
			Name="dτ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dτIdeal"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="dτReal"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
