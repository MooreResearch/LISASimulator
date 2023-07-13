#tag Class
Protected Class HCalculatorClass
	#tag Method, Flags = &h0
		Sub Add2ATAMatrix(TheATA As Matrix)
		  Var InverseNoise As Double = 1.0/HP0Calculator.Sn2
		  // Load the derivatives into an array
		  Var DArray(14) As Double
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
		Sub CalcDetectorFunctions()
		  // Set up some useful local values so that we don't need to
		  // calculate them multiple times
		  Var ρ As Double = Parameters.GMΩe*Values.τr
		  Var twoρ As Double = 2*ρ
		  Var threeρ As Double = 3*ρ
		  Var fourρ As Double = 4*ρ
		  Var Φ As Double = Parameters.Φ
		  Var twoΦ As Double = 2*Φ
		  
		  // Set up the sine and cosines for functions for detector 1
		  Var arg22 As Double = twoρ-twoσ1
		  Var arg422 As Double = fourρ-twoσ1-twoΦ
		  Var arg321 As Double = threeρ-twoσ1-Φ
		  Var arg121 As Double = ρ-twoσ1+Φ
		  Var sin22 As Double = Sin(arg22)
		  Var sin422 As Double = Sin(arg422)
		  Var sin321 As Double = Sin(arg321)
		  Var sin121 As Double = Sin(arg121)
		  Var cos22 As Double = Cos(arg22)
		  Var cos422 As Double = Cos(arg422)
		  Var cos321 As Double = Cos(arg321)
		  Var cos121 As Double = Cos(arg121)
		  // Calculate the D+ factor and its derivative with respect to Θ
		  Var Term1 As Double = C364R3*(-6.0*sin22 + Sin2σ1Minus2Φx9 - sin422)
		  Var Term2 As Double = C164R3*(18.0*sin22 + Sin2σ1Minus2Φx9 + sin422)
		  Var Term3 As Double = -C316*(sin321 - 3.0*sin121)
		  DPlus1 = Term1 + Cos2Θ*Term2 + Sin2Θ*Term3
		  DDp1DΘ = 2*(-Sin2Θ*Term2 + Cos2Θ*Term3)
		  // Calculate the derivative of D+ with respect to Φ
		  Term1 = -2*C364R3*(Cos2σ1Minus2Φx9 - cos422)
		  Term2 = Term1/3.0
		  Term3 = C316*(cos321 + 3.0*cos121)
		  DDp1DΦ = Term1 + Cos2Θ*Term2 + Sin2Θ*Term3
		  // Calculate the Dx factor and its derivative with respect to Θ
		  Term1 = 4*C164R3*(Cos2σ1Minus2Φx9 - cos422)
		  Term2 = C316*(cos321 - 3*cos121)
		  DCross1 = CosΘ*Term1 + SinΘ*Term2
		  DDx1DΘ = -SinΘ*Term1 + CosΘ*Term2
		  // Calculate the derivative of Dx with respect to Φ
		  Term1 = -8*C164R3*(Sin2σ1Minus2Φx9 - sin422)
		  Term2 = -C316*(sin321 + 3*sin121)
		  DDx1DΦ = CosΘ*Term1 + SinΘ*Term2
		  // Finally, Calculate the F+ and Fx factors for Detector 1
		  FPlus1 = HalfCos2ψ*DPlus1 - HalfSin2ψ*DCross1
		  FCross1 = HalfSin2ψ*DPlus1 + HalfCos2ψ*DCross1
		  
		  // Now repeat the whole thing with σ2 replacing σ1
		  arg22 = twoρ-twoσ2
		  arg422 = fourρ-twoσ2-twoΦ
		  arg321 = threeρ-twoσ2-Φ
		  arg121 = ρ-twoσ2+Φ
		  sin22 = Sin(arg22)
		  sin422 = Sin(arg422)
		  sin321 = Sin(arg321)
		  sin121 = Sin(arg121)
		  cos22 = Cos(arg22)
		  cos422 = Cos(arg422)
		  cos321 = Cos(arg321)
		  cos121 = Cos(arg121)
		  // Calculate the D+ factor and its derivative with respect to Θ
		  Term1 = C364R3*(-6.0*sin22 + Sin2σ2Minus2Φx9 - sin422)
		  Term2 = C164R3*(18.0*sin22 + Sin2σ2Minus2Φx9 + sin422)
		  Term3 = -C316*(sin321 - 3.0*sin121)
		  DPlus2= Term1 + Cos2Θ*Term2 + Sin2Θ*Term3
		  DDp2DΘ = 2*(-Sin2Θ*Term2 + Cos2Θ*Term3)
		  // Calculate the derivative of D+ with respect to Φ
		  Term1 = -2*C364R3*(Cos2σ2Minus2Φx9 - cos422)
		  Term2 = Term1/3.0
		  Term3 = C316*(cos321 + 3.0*cos121)
		  DDp2DΦ = Term1 + Cos2Θ*Term2 + Sin2Θ*Term3
		  // Calculate the Dx factor and its derivative with respect to Θ
		  Term1 = 4*C164R3*(Cos2σ2Minus2Φx9 - cos422)
		  Term2 = C316*(cos321 - 3*cos121)
		  DCross2 = CosΘ*Term1 + SinΘ*Term2
		  DDx2DΘ = -SinΘ*Term1 + CosΘ*Term2
		  // Calculate the derivative of Dx with respect to Φ
		  Term1 = -8*C164R3*(Sin2σ2Minus2Φx9 - sin422)
		  Term2 = -C316*(sin321 + 3*sin121)
		  DDx2DΦ = CosΘ*Term1 + SinΘ*Term2
		  // Finally, Calculate the F+ and Fx factors for Detector 1
		  FPlus2 = HalfCos2ψ*DPlus2 - HalfSin2ψ*DCross2
		  FCross2 = HalfSin2ψ*DPlus2 + HalfCos2ψ*DCross2
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Calculate(TheValues As CurrentValuesClass, TheDerivs As CurrentDerivativesClass, ATAMatrix As Matrix)
		  Values = TheValues
		  Derivs = TheDerivs
		  GetSubclassTerms
		  V = Values.V
		  V2 = V*V
		  V3 = V2*V
		  H0 = Parameters.H0
		  H0V2 = H0*V2
		  CalculatePlus
		  CalculateCross
		  CalculateDhpDχ
		  CalculateDhxDχ
		  CalcDetectorFunctions
		  CalculateHTotal
		  Add2ATAMatrix(ATAMatrix)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateCross()
		  // Calculate the total value of hx and its non-spin derivatives
		  HX = H0V2*(HX0Calculator.h + V*(HX1Calculator.h + HX1SOCalculator.h) _
		  + V2*(HX2Calculator.h + HX2SOCalculator.h) _
		  + V3*(HX3Calculator.h + HX3SOCalculator.h))
		  HXAdjusted = H0V2*(HX0Calculator.HAdjusted + V*(HX1Calculator.HAdjusted + HX1SOCalculator.HAdjusted) _
		  + V2*(HX2Calculator.HAdjusted + HX2SOCalculator.HAdjusted) _
		  + V3*(HX3Calculator.HAdjusted + HX3SOCalculator.HAdjusted))
		  DhxDV0 = H0V2*(HX0Calculator.DhDV0 _
		  + V*(HX1Calculator.DhDV0 + HX1SOCalculator.DhDV0) _
		  + V2*(HX2Calculator.DhDV0 + HX2SOCalculator.DhDV0) _
		  + V3*(HX3Calculator.DhDV0 + HX3SOCalculator.DhDV0))
		  DhxDZ = H0V2*(HX0Calculator.DhDZ _
		  + V*(HX1Calculator.DhDZ + HX1SOCalculator.DhDZ) _
		  + V2*(HX2Calculator.DhDZ + HX2SOCalculator.DhDZ) _
		  + V3*(HX3Calculator.DhDZ + HX3SOCalculator.DhDZ))
		  DhxDβ = H0V2*(HX0Calculator.DhDβ _
		  + V*(HX1Calculator.DhDβ + HX1SOCalculator.DhDβ) _
		  + V2*(HX2Calculator.DhDβ + HX2SOCalculator.DhDβ) _
		  + V3*(HX3Calculator.DhDβ + HX3SOCalculator.DhDβ))
		  DhxDδ = H0V2*(HX0Calculator.DhDδ _
		  + V*(HX1Calculator.DhDδ + HX1SOCalculator.DhDδ) _
		  + V2*(HX2Calculator.DhDδ + HX2SOCalculator.DhDδ) _
		  + V3*(HX3Calculator.DhDδ + HX3SOCalculator.DhDδ))
		  DhxDΘ = H0V2*(HX0Calculator.DhDΘ _
		  + V*(HX1Calculator.DhDΘ + HX1SOCalculator.DhDΘ) _
		  + V2*(HX2Calculator.DhDΘ + HX2SOCalculator.DhDΘ) _
		  + V3*(HX3Calculator.DhDΘ + HX3SOCalculator.DhDΘ))
		  DhxDλ0 = H0V2*(HX0Calculator.DhDλ0 _
		  + V*(HX1Calculator.DhDλ0 + HX1SOCalculator.DhDλ0) _
		  + V2*(HX2Calculator.DhDλ0 + HX2SOCalculator.DhDλ0) _
		  + V3*(HX3Calculator.DhDλ0 + HX3SOCalculator.DhDλ0))
		  DHXDΦ = H0V2*(HX0Calculator.DhDΦ _
		  + V*(HX1Calculator.DhDΦ + HX1SOCalculator.DhDΦ) _
		  + V2*(HX2Calculator.DhDΦ + HX2SOCalculator.DhDΦ) _
		  + V3*(HX3Calculator.DhDΦ + HX3SOCalculator.DhDΦ))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhpDχ()
		  // Calculate the spin derivatives for hp
		  DhpDχ10x = H0V2*(HP0Calculator.DhDχ10x _
		  + V*(HP1Calculator.DhDχ10x + HP1SOCalculator.DhDχ10x) _
		  + V2*(HP2Calculator.DhDχ10x + HP2SOCalculator.DhDχ10x) _
		  + V3*(HP3Calculator.DhDχ10x + HP3SOCalculator.DhDχ10x))
		  DhpDχ10y = H0V2*(HP0Calculator.DhDχ10y _
		  + V*(HP1Calculator.DhDχ10y + HP1SOCalculator.DhDχ10y) _
		  + V2*(HP2Calculator.DhDχ10y + HP2SOCalculator.DhDχ10y) _
		  + V3*(HP3Calculator.DhDχ10y + HP3SOCalculator.DhDχ10y))
		  DhpDχ10z = H0V2*(HP0Calculator.DhDχ10z _
		  + V*(HP1Calculator.DhDχ10z + HP1SOCalculator.DhDχ10z) _
		  + V2*(HP2Calculator.DhDχ10z + HP2SOCalculator.DhDχ10z) _
		  + V3*(HP3Calculator.DhDχ10z + HP3SOCalculator.DhDχ10z))
		  DhpDχ20x = H0V2*(HP0Calculator.DhDχ20x _
		  + V*(HP1Calculator.DhDχ20x + HP1SOCalculator.DhDχ20x) _
		  + V2*(HP2Calculator.DhDχ20x + HP2SOCalculator.DhDχ20x) _
		  + V3*(HP3Calculator.DhDχ20x + HP3SOCalculator.DhDχ20x))
		  DhpDχ20y = H0V2*(HP0Calculator.DhDχ20y _
		  + V*(HP1Calculator.DhDχ20y + HP1SOCalculator.DhDχ20y) _
		  + V2*(HP2Calculator.DhDχ20y + HP2SOCalculator.DhDχ20y) _
		  + V3*(HP3Calculator.DhDχ20y + HP3SOCalculator.DhDχ20y))
		  DhpDχ20z = H0V2*(HP0Calculator.DhDχ20z _
		  + V*(HP1Calculator.DhDχ20z + HP1SOCalculator.DhDχ20z) _
		  + V2*(HP2Calculator.DhDχ20z + HP2SOCalculator.DhDχ20z) _
		  + V3*(HP3Calculator.DhDχ20z + HP3SOCalculator.DhDχ20z))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhxDχ()
		  // Calculate the spin derivatives for hx
		  DhxDχ10x = H0V2*(HX0Calculator.DhDχ10x _
		  + V*(HX1Calculator.DhDχ10x + HX1SOCalculator.DhDχ10x) _
		  + V2*(HX2Calculator.DhDχ10x + HX2SOCalculator.DhDχ10x) _
		  + V3*(HX3Calculator.DhDχ10x + HX3SOCalculator.DhDχ10x))
		  DhxDχ10y = H0V2*(HX0Calculator.DhDχ10y _
		  + V*(HX1Calculator.DhDχ10y + HX1SOCalculator.DhDχ10y) _
		  + V2*(HX2Calculator.DhDχ10y + HX2SOCalculator.DhDχ10y) _
		  + V3*(HX3Calculator.DhDχ10y + HX3SOCalculator.DhDχ10y))
		  DhxDχ10z = H0V2*(HX0Calculator.DhDχ10z _
		  + V*(HX1Calculator.DhDχ10z + HX1SOCalculator.DhDχ10z) _
		  + V2*(HX2Calculator.DhDχ10z + HX2SOCalculator.DhDχ10z) _
		  + V3*(HX3Calculator.DhDχ10z + HX3SOCalculator.DhDχ10z))
		  DhxDχ20x = H0V2*(HX0Calculator.DhDχ20x _
		  + V*(HX1Calculator.DhDχ20x + HX1SOCalculator.DhDχ20x) _
		  + V2*(HX2Calculator.DhDχ20x + HX2SOCalculator.DhDχ20x) _
		  + V3*(HX3Calculator.DhDχ20x + HX3SOCalculator.DhDχ20x))
		  DhxDχ20y = H0V2*(HX0Calculator.DhDχ20y _
		  + V*(HX1Calculator.DhDχ20y + HX1SOCalculator.DhDχ20y) _
		  + V2*(HX2Calculator.DhDχ20y + HX2SOCalculator.DhDχ20y) _
		  + V3*(HX3Calculator.DhDχ20y + HX3SOCalculator.DhDχ20y))
		  DhxDχ20z = H0V2*(HX0Calculator.DhDχ20z _
		  + V*(HX1Calculator.DhDχ20z + HX1SOCalculator.DhDχ20z) _
		  + V2*(HX2Calculator.DhDχ20z + HX2SOCalculator.DhDχ20z) _
		  + V3*(HX2Calculator.DhDχ20z + HX2SOCalculator.DhDχ20z))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateHTotal()
		  // This will calculate the total signal H plus all its derivatives
		  Var FPlus As Double = FPlus1 + FPlus2
		  Var FCross As Double = FCross1 + FCross2
		  H = FPlus*HP + FCross*HX
		  DH.Value = H
		  DH.Dh0 = (FPlus*HPAdjusted + FCross*HXAdjusted)/H0
		  DH.Dψ = 2*(-FCross*HPAdjusted + FPlus*HXAdjusted)
		  Var DFpDq As Double = HalfCos2ψ*(DDp1DΘ + DDp2DΘ) - HalfSin2ψ*(DDx1DΘ + DDx2DΘ)
		  Var DFxDq As Double = HalfSin2ψ*(DDp1DΘ + DDp2DΘ) + HalfCos2ψ*(DDx1DΘ + DDx2DΘ)
		  DH.DΘ = DFpDq*HP + DFxDq*HX + FPlus*DhpDΘ + FCross*DhxDΘ
		  DFpDq = HalfCos2ψ*(DDp1DΦ + DDp2DΦ ) - HalfSin2ψ*(DDx1DΦ  + DDx2DΦ )
		  DFxDq = HalfSin2ψ*(DDp1DΦ  + DDp2DΦ ) + HalfCos2ψ*(DDx1DΦ  + DDx2DΦ)
		  DH.DΦ = DFpDq*HP + DFxDq*HX + FPlus*DhpDΦ + FCross*DhxDΦ
		  DH.Dβ = FPlus*DhpDβ + FCross*DhxDβ
		  DH.DV0 = FPlus*DhpDV0 + FCross*DhxDV0
		  DH.Dz = FPlus*DhpDZ+ FCross*DhxDZ
		  DH.Dδ = FPlus*DhpDδ+ FCross*DhxDδ
		  DH.Dλ0 = FPlus*DhpDλ0+ FCross*DhxDλ0
		  DH.Dχ10x = FPlus*DhpDχ10x+ FCross*DhxDχ10x
		  DH.Dχ10y = FPlus*DhpDχ10y+ FCross*DhxDχ10y
		  DH.Dχ10z = FPlus*DhpDχ10z+ FCross*DhxDχ10z
		  DH.Dχ20x = FPlus*DhpDχ20x+ FCross*DhxDχ20x
		  DH.Dχ20y = FPlus*DhpDχ20y+ FCross*DhxDχ20y
		  DH.Dχ20z = FPlus*DhpDχ20z+ FCross*DhxDχ20z
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculatePlus()
		  // Calculate hp and its non-spin derivatives
		  HP = H0V2*(HP0Calculator.h + V*(HP1Calculator.h + HP1SOCalculator.h) _
		  + V2*(HP2Calculator.h + HP2SOCalculator.h) _
		  + V3*(HP3Calculator.h + HP3SOCalculator.h))
		  HPAdjusted = H0V2*(HP0Calculator.HAdjusted+ V*(HP1Calculator.HAdjusted + HP1SOCalculator.HAdjusted) _
		  + V2*(HP2Calculator.HAdjusted + HP2SOCalculator.HAdjusted) _
		  + V3*(HP3Calculator.HAdjusted + HP3SOCalculator.HAdjusted))
		  DhpDV0 = H0V2*(HP0Calculator.DhDV0 _
		  + V*(HP1Calculator.DhDV0 + HP1SOCalculator.DhDV0) _
		  + V2*(HP2Calculator.DhDV0 + HP2SOCalculator.DhDV0) _
		  + V3*(HP3Calculator.DhDV0 + HP3SOCalculator.DhDV0))
		  DhpDZ = H0V2*(HP0Calculator.DhDZ _
		  + V*(HP1Calculator.DhDZ + HP1SOCalculator.DhDZ) _
		  + V2*(HP2Calculator.DhDZ + HP2SOCalculator.DhDZ) _
		  + V3*(HP3Calculator.DhDZ + HP3SOCalculator.DhDZ))
		  DhpDβ = H0V2*(HP0Calculator.DhDβ _
		  + V*(HP1Calculator.DhDβ + HP1SOCalculator.DhDβ) _
		  + V2*(HP2Calculator.DhDβ + HP2SOCalculator.DhDβ) _
		  + V3*(HP3Calculator.DhDβ + HP3SOCalculator.DhDβ))
		  DhpDδ = H0V2*(HP0Calculator.DhDδ _
		  + V*(HP1Calculator.DhDδ + HP1SOCalculator.DhDδ) _
		  + V2*(HP2Calculator.DhDδ + HP2SOCalculator.DhDδ) _
		  + V3*(HP3Calculator.DhDδ + HP3SOCalculator.DhDδ))
		  DhpDΘ = H0V2*(HP0Calculator.DhDΘ _
		  + V*(HP1Calculator.DhDΘ + HP1SOCalculator.DhDΘ) _
		  + V2*(HP2Calculator.DhDΘ + HP2SOCalculator.DhDΘ) _
		  + V3*(HP3Calculator.DhDΘ + HP3SOCalculator.DhDΘ))
		  DhpDλ0 = H0V2*(HP0Calculator.DhDλ0 _
		  + V*(HP1Calculator.DhDλ0 + HP1SOCalculator.DhDλ0) _
		  + V2*(HP2Calculator.DhDλ0 + HP2SOCalculator.DhDλ0) _
		  + V3*(HP3Calculator.DhDλ0 + HP3SOCalculator.DhDλ0))
		  DhpDΦ = H0V2*(HP0Calculator.DhDΦ _
		  + V*(HP1Calculator.DhDΦ + HP1SOCalculator.DhDΦ) _
		  + V2*(HP2Calculator.DhDΦ + HP2SOCalculator.DhDΦ) _
		  + V3*(HP3Calculator.DhDΦ + HP3SOCalculator.DhDΦ))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(CaseParameters As CaseParametersClass)
		  Parameters  = CaseParameters  // Store a reference to the case's parameters
		  // Initialize the term calculators
		  HP0Calculator = New HP0CalculatorClass(Parameters)
		  HP1Calculator = New HP1CalculatorClass(Parameters)
		  HP1SOCalculator = New HP1SOCalculatorClass(Parameters)
		  HP2Calculator = New HP2CalculatorClass(Parameters)
		  HP2SOCalculator = New HP2SOCalculatorClass(Parameters)
		  HP3Calculator = New HP3CalculatorClass(Parameters)
		  HP3SOCalculator = New HP3SOCalculatorClass(Parameters)
		  HX0Calculator = New HX0CalculatorClass(Parameters)
		  HX1Calculator = New HP1CalculatorClass(Parameters)
		  HX1SOCalculator = New HP1SOCalculatorClass(Parameters)
		  HX2Calculator = New HP2CalculatorClass(Parameters)
		  HX2SOCalculator = New HP2SOCalculatorClass(Parameters)
		  HX3Calculator = New HP3CalculatorClass(Parameters)
		  HX3SOCalculator = New HP3SOCalculatorClass(Parameters)
		  DH = New DerivativeSet
		  CosΘ = Cos(Parameters.Θ)
		  Cos2Θ = Cos(2*Parameters.Θ)
		  SinΘ = Sin(Parameters.Θ)
		  Sin2Θ = Sin(2*Parameters.Θ)
		  Twoσ1 = 2*(0.75*Parameters.π + Parameters.ρ0)
		  Twoσ2 = Twoσ1 + 4*Parameters.π/3.0
		  Sin2σ1Minus2Φx9 = 9*Sin(Twoσ1-2*Parameters.Φ)
		  Sin2σ1Minus2Φx9 = 9*Sin(Twoσ2-2*Parameters.Φ)
		  HalfSin2ψ = 0.5*Sin(2*Parameters.ψ)
		  HalfCos2ψ = 0.5*Cos(2*Parameters.ψ)
		  Var R3 As Double = Sqrt(3)
		  C132R3 = R3/32
		  C164R3 = R3/64
		  C316 = 3/16
		  C364R3 = 3*R3/64
		  C38 = 3/8
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetSubclassTerms()
		  HP0Calculator.Calculate(Values, Derivs)
		  HP1Calculator.Calculate(Values, Derivs)
		  HP1SOCalculator.Calculate(Values, Derivs)
		  HP2Calculator.Calculate(Values, Derivs)
		  HP2SOCalculator.Calculate(Values, Derivs)
		  HP3Calculator.Calculate(Values, Derivs)
		  HP3SOCalculator.Calculate(Values, Derivs)
		  HX0Calculator.Calculate(Values, Derivs)
		  HX1Calculator.Calculate(Values, Derivs)
		  HX1SOCalculator.Calculate(Values, Derivs)
		  HX2Calculator.Calculate(Values, Derivs)
		  HX2SOCalculator.Calculate(Values, Derivs)
		  HX3Calculator.Calculate(Values, Derivs)
		  HX3SOCalculator.Calculate(Values, Derivs)
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		C132R3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C164R3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C316 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C364R3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C38 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Cos2Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Cos2σ1Minus2Φx9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Cos2σ2Minus2Φx9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CosΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCross1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCross2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDp1DΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDp1DΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDp2DΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDp2DΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDx1DΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDx1DΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDx2DΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DDx2DΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Derivs As CurrentDerivativesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		DFp1DΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DFp1DΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DFp2DΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DFp2DΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DFx1DΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DFx1DΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DFx2DΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DFx2DΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DH As DerivativeSet
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDλ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhpDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDλ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhxDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DPlus1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DPlus2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FCross1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FCross2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FPlus1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FPlus2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		H As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		H0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		H0V2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		HalfCos2ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		HalfSin2ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		HP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		HP0Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP1Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP1SOCalculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP2Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP2SOCalculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP3Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HP3SOCalculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HPAdjusted As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		HX As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		HX0Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX1Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX1SOCalculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX2Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX2SOCalculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX3Calculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HX3SOCalculator As HNCalculator
	#tag EndProperty

	#tag Property, Flags = &h0
		HXAdjusted As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Sin2Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sin2σ1Minus2Φx9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sin2σ2Minus2Φx9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SinΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Twoσ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Twoσ2 As Double
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
			Name="DhpDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDλ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhpDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDλ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhxDχ20z"
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
		#tag ViewProperty
			Name="H0V2"
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
			Name="Twoσ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Twoσ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sin2σ1Minus2Φx9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sin2σ2Minus2Φx9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="HalfSin2ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="HalfCos2ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FPlus1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FCross2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FPlus2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="FCross1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DFx1DΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DFx2DΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DFp1DΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DFp2DΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DFp1DΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DFp2DΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DFx1DΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DFx2DΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
