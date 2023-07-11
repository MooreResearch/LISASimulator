#tag Class
Protected Class HNCalculator
	#tag Method, Flags = &h0
		Sub AddTerm(AMethod As AmplitudeMethod, Ak As Integer, Bk As Integer)
		  Var NewTerm As New HTermData
		  NewTerm.A = AMethod.Invoke(AP0)
		  NewTerm.ak = Ak
		  NewTerm.bk = Bk
		  NewTerm.DADcosι = (AMethod.Invoke(APCosιPlus) - AMethod.Invoke(APCosιMinus))/(2*APCosιPlus.εCosι)
		  NewTerm.DADβ = (AMethod.Invoke(APβPlus) - AMethod.Invoke(APβMinus))/(2*APβPlus.εβ)
		  NewTerm.DADδ = (AMethod.Invoke(APδPlus) - AMethod.Invoke(APδMinus))/(2*APδPlus.εδ)
		  NewTerm.DADχax = (AMethod.Invoke(APχaxPlus) - AMethod.Invoke(APχaxMinus))/(2*APχaxPlus.εχax)
		  NewTerm.DADχay = (AMethod.Invoke(APχayPlus) - AMethod.Invoke(APχayMinus))/(2*APχayPlus.εχay)
		  NewTerm.DADχaz = (AMethod.Invoke(APχazPlus) - AMethod.Invoke(APχazMinus))/(2*APχazPlus.εχaz)
		  NewTerm.DADχsx = (AMethod.Invoke(APχsxPlus) - AMethod.Invoke(APχsxMinus))/(2*APχsxPlus.εχsx)
		  NewTerm.DADχsy = (AMethod.Invoke(APχsyPlus) - AMethod.Invoke(APχsyMinus))/(2*APχsyPlus.εχsy)
		  NewTerm.DADχsz = (AMethod.Invoke(APχszPlus) - AMethod.Invoke(APχszMinus))/(2*APχszPlus.εχsz)
		  TermData.Add(NewTerm)
		End Sub
	#tag EndMethod

	#tag DelegateDeclaration, Flags = &h0
		Delegate Function AmplitudeMethod(AP As AmplitudeParameters) As Double
	#tag EndDelegateDeclaration

	#tag Method, Flags = &h0
		Sub Calculate(TheValues As CurrentValuesClass, TheDerivatives As CurrentDerivativesClass)
		  CurrentValues = TheValues
		  CurrentDerivatives = TheDerivatives
		  UpdateAmpParameters
		  // Clear the TermData array for this PN polarization term
		  TermData.RemoveAll  // Empty the array
		  GetTerms  // Request the terms from the subclass
		  // Calculate this order's contribution to h and its derivatives
		  CalculateH
		  CalculateDhDV0
		  CalculateDhDZ
		  CalculateDhDα
		  CalculateDhDβ
		  CalculateDhDδ
		  CalculateDhDΘ
		  CalculateDhDΦ
		  CalculateDhDχ10x
		  CalculateDhDχ10y
		  CalculateDhDχ10z
		  CalculateDhDχ20x
		  CalculateDhDχ20y
		  CalculateDhDχ20z
		  CalculateDhDΨr
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDV0()
		  Var sum As Double = h*(PNOrder+2.0)/CurrentValues.V*CurrentDerivatives.DvDV0
		  sum = sum + DhDα*CurrentDerivatives.DαDV0
		  sum = sum + DhDΨr*CurrentDerivatives.DΨrDV0
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    Var totalDADV0 As Double = term.DADcosι*CurrentDerivatives.DCosιDV0 _
		    + term.DADχsx*CurrentDerivatives.DχsxDV0 _
		    + term.DADχsy*CurrentDerivatives.DχsyDV0 _
		    + term.DADχsz*CurrentDerivatives.DχszDV0 _
		    + term.DADχax*CurrentDerivatives.DχaxDV0 _
		    + term.DADχay*CurrentDerivatives.DχayDV0 _
		    + term.DADχaz*CurrentDerivatives.DχazDV0
		    If Cross Then
		      sum = sum + totalDADV0*sin(term.ak*α + term.bk*Ψr)
		    Else
		      sum = sum + totalDADV0*cos(term.ak*α + term.bk*Ψr)
		    End If
		  Next
		  DhDV0 = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDZ()
		  Var sum As Double = h*(PNOrder+2.0)/CurrentValues.V*CurrentDerivatives.DvDZ
		  sum = sum + DhDα*CurrentDerivatives.DαDZ
		  sum = sum + DhDΨr*CurrentDerivatives.DΨrDZ
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    Var totalDADZ As Double = term.DADcosι*CurrentDerivatives.DCosιDZ _
		    + term.DADχsx*CurrentDerivatives.DχsxDZ _
		    + term.DADχsy*CurrentDerivatives.DχsyDZ _
		    + term.DADχsz*CurrentDerivatives.DχszDZ _
		    + term.DADχax*CurrentDerivatives.DχaxDZ _
		    + term.DADχay*CurrentDerivatives.DχayDZ _
		    + term.DADχaz*CurrentDerivatives.DχazDZ
		    If Cross Then
		      sum = sum + totalDADZ*sin(term.ak*α + term.bk*Ψr)
		    Else
		      sum = sum + totalDADZ*cos(term.ak*α + term.bk*Ψr)
		    End If
		  Next
		  DhDZ = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDα()
		  Var sum As Double
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    If Cross Then
		      sum = sum + term.A*cos(term.ak*α + term.bk*Ψr)*term.ak
		    Else
		      sum = sum - term.A*sin(term.ak*α + term.bk*Ψr)*term.bk
		    End If
		  Next
		  DhDα = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDβ()
		  Var sum As Double
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    If Cross Then
		      sum = sum + term.DADβ*sin(term.ak*α + term.bk*Ψr)
		    Else
		      sum = sum + term.DADβ*cos(term.ak*α + term.bk*Ψr)
		    End If
		  Next
		  DhDβ = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDδ()
		  Var sum As Double = h*(PNOrder+2.0)/CurrentValues.V*CurrentDerivatives.DvDδ
		  sum = sum + DhDα*CurrentDerivatives.DαDδ
		  sum = sum + DhDΨr*CurrentDerivatives.DΨrDδ
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    Var totalDADδ As Double = term.DADδ _
		    + term.DADcosι*CurrentDerivatives.DCosιDδ _
		    + term.DADχsx*CurrentDerivatives.DχsxDδ _
		    + term.DADχsy*CurrentDerivatives.DχsyDδ _
		    + term.DADχsz*CurrentDerivatives.DχszDδ _
		    + term.DADχax*CurrentDerivatives.DχaxDδ _
		    + term.DADχay*CurrentDerivatives.DχayDδ _
		    + term.DADχaz*CurrentDerivatives.DχazDδ
		    If Cross Then
		      sum = sum + totalDADδ*sin(term.ak*α + term.bk*Ψr)
		    Else
		      sum = sum + totalDADδ*cos(term.ak*α + term.bk*Ψr)
		    End If
		  Next
		  DhDδ = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDΘ()
		  DhDΘ = DhDΨr*CurrentDerivatives.DΨrDΘ
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDΦ()
		  DhDΦ = DhDΨr*CurrentDerivatives.DΨrDΦ
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDχ10x()
		  Var sum As Double = h*(PNOrder+2.0)/CurrentValues.V*CurrentDerivatives.DvDχ10x
		  sum = sum + DhDα*CurrentDerivatives.DαDχ10x
		  sum = sum + DhDΨr*CurrentDerivatives.DΨrDχ10x
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    Var totalDADχ10x As Double = term.DADcosι*CurrentDerivatives.DCosιDχ10x _
		    + term.DADχsx*CurrentDerivatives.DχsxDχ10x _
		    + term.DADχsy*CurrentDerivatives.DχsyDχ10x _
		    + term.DADχsz*CurrentDerivatives.DχszDχ10x _
		    + term.DADχax*CurrentDerivatives.DχaxDχ10x _
		    + term.DADχay*CurrentDerivatives.DχayDχ10x _
		    + term.DADχaz*CurrentDerivatives.DχazDχ10x
		    If Cross Then
		      sum = sum + totalDADχ10x*sin(term.ak*α + term.bk*Ψr)
		    Else
		      sum = sum + totalDADχ10x*cos(term.ak*α + term.bk*Ψr)
		    End If
		  Next
		  DhDχ10x = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDχ10y()
		  Var sum As Double = h*(PNOrder+2.0)/CurrentValues.V*CurrentDerivatives.DvDχ10y
		  sum = sum + DhDα*CurrentDerivatives.DαDχ10y
		  sum = sum + DhDΨr*CurrentDerivatives.DΨrDχ10y
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    Var totalDADχ10y As Double = term.DADcosι*CurrentDerivatives.DCosιDχ10y _
		    + term.DADχsx*CurrentDerivatives.DχsxDχ10y _
		    + term.DADχsy*CurrentDerivatives.DχsyDχ10y _
		    + term.DADχsz*CurrentDerivatives.DχszDχ10y _
		    + term.DADχax*CurrentDerivatives.DχaxDχ10y _
		    + term.DADχay*CurrentDerivatives.DχayDχ10y _
		    + term.DADχaz*CurrentDerivatives.DχazDχ10y
		    If Cross Then
		      sum = sum + totalDADχ10y*sin(term.ak*α + term.bk*Ψr)
		    Else
		      sum = sum + totalDADχ10y*cos(term.ak*α + term.bk*Ψr)
		    End If
		  Next
		  DhDχ10y = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDχ10z()
		  Var sum As Double = h*(PNOrder+2.0)/CurrentValues.V*CurrentDerivatives.DvDχ10z
		  sum = sum + DhDα*CurrentDerivatives.DαDχ10z
		  sum = sum + DhDΨr*CurrentDerivatives.DΨrDχ10z
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    Var totalDADχ10z As Double = term.DADcosι*CurrentDerivatives.DCosιDχ10z _
		    + term.DADχsx*CurrentDerivatives.DχsxDχ10z _
		    + term.DADχsy*CurrentDerivatives.DχsyDχ10z _
		    + term.DADχsz*CurrentDerivatives.DχszDχ10z _
		    + term.DADχax*CurrentDerivatives.DχaxDχ10z _
		    + term.DADχay*CurrentDerivatives.DχayDχ10z _
		    + term.DADχaz*CurrentDerivatives.DχazDχ10z
		    If Cross Then
		      sum = sum + totalDADχ10z*sin(term.ak*α + term.bk*Ψr)
		    Else
		      sum = sum + totalDADχ10z*cos(term.ak*α + term.bk*Ψr)
		    End If
		  Next
		  DhDχ10z = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDχ20x()
		  Var sum As Double = h*(PNOrder+2.0)/CurrentValues.V*CurrentDerivatives.DvDχ20x
		  sum = sum + DhDα*CurrentDerivatives.DαDχ20x
		  sum = sum + DhDΨr*CurrentDerivatives.DΨrDχ20x
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    Var totalDADχ20x As Double = term.DADcosι*CurrentDerivatives.DCosιDχ20x _
		    + term.DADχsx*CurrentDerivatives.DχsxDχ20x _
		    + term.DADχsy*CurrentDerivatives.DχsyDχ20x _
		    + term.DADχsz*CurrentDerivatives.DχszDχ20x _
		    + term.DADχax*CurrentDerivatives.DχaxDχ20x _
		    + term.DADχay*CurrentDerivatives.DχayDχ20x _
		    + term.DADχaz*CurrentDerivatives.DχazDχ20x
		    If Cross Then
		      sum = sum + totalDADχ20x*sin(term.ak*α + term.bk*Ψr)
		    Else
		      sum = sum + totalDADχ20x*cos(term.ak*α + term.bk*Ψr)
		    End If
		  Next
		  DhDχ20x = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDχ20y()
		  Var sum As Double = h*(PNOrder+2.0)/CurrentValues.V*CurrentDerivatives.DvDχ20y
		  sum = sum + DhDα*CurrentDerivatives.DαDχ20y
		  sum = sum + DhDΨr*CurrentDerivatives.DΨrDχ20y
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    Var totalDADχ20y As Double = term.DADcosι*CurrentDerivatives.DCosιDχ20y _
		    + term.DADχsx*CurrentDerivatives.DχsxDχ20y _
		    + term.DADχsy*CurrentDerivatives.DχsyDχ20y _
		    + term.DADχsz*CurrentDerivatives.DχszDχ20y _
		    + term.DADχax*CurrentDerivatives.DχaxDχ20y _
		    + term.DADχay*CurrentDerivatives.DχayDχ20y _
		    + term.DADχaz*CurrentDerivatives.DχazDχ20y
		    If Cross Then
		      sum = sum + totalDADχ20y*sin(term.ak*α + term.bk*Ψr)
		    Else
		      sum = sum + totalDADχ20y*cos(term.ak*α + term.bk*Ψr)
		    End If
		  Next
		  DhDχ20y = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDχ20z()
		  Var sum As Double = h*(PNOrder+2.0)/CurrentValues.V*CurrentDerivatives.DvDχ20z
		  sum = sum + DhDα*CurrentDerivatives.DαDχ20z
		  sum = sum + DhDΨr*CurrentDerivatives.DΨrDχ20z
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    Var totalDADχ20z As Double = term.DADcosι*CurrentDerivatives.DCosιDχ20z _
		    + term.DADχsx*CurrentDerivatives.DχsxDχ20z _
		    + term.DADχsy*CurrentDerivatives.DχsyDχ20z _
		    + term.DADχsz*CurrentDerivatives.DχszDχ20z _
		    + term.DADχax*CurrentDerivatives.DχaxDχ20z _
		    + term.DADχay*CurrentDerivatives.DχayDχ20z _
		    + term.DADχaz*CurrentDerivatives.DχazDχ20z
		    If Cross Then
		      sum = sum + totalDADχ20z*sin(term.ak*α + term.bk*Ψr)
		    Else
		      sum = sum + totalDADχ20z*cos(term.ak*α + term.bk*Ψr)
		    End If
		  Next
		  DhDχ20z = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateDhDΨr()
		  Var sum As Double
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    If Cross Then
		      sum = sum + term.A*cos(term.ak*α + term.bk*Ψr)*term.bk
		    Else
		      sum = sum - term.A*sin(term.ak*α + term.bk*Ψr)*term.bk
		    End If
		  Next
		  DhDΨr = sum
		  DhDλ0 = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateH()
		  Var sum As Double
		  Var α As Double = CurrentValues.α
		  Var Ψr As Double = CurrentValues.Ψr
		  For Each term As HTermData in TermData
		    If Cross Then
		      sum = sum + term.A*sin(term.ak*α + term.bk*Ψr)
		    Else
		      sum = sum + term.A*cos(term.ak*α + term.bk*Ψr)
		    End If
		  Next
		  h = sum
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  // Save a reference to this case's list of parameters
		  Parameters = MyParameters
		  // Set properties for various parameters that we commonly need
		  // so they can be accessed more quickly
		  π = Parameters.π
		  // Initialize all the side-case AmplitudeParameters
		  AP0 = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.None, 0)
		  APCosιPlus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.Cosι, 1.0e-6)
		  APCosιMinus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.Cosι, -1.0e-6)
		  APβPlus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.β, 1.0e-6)
		  APβMinus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.β, -1.0e-6)
		  APδPlus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.δ, 1.0e-6)
		  APδMinus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.δ, -1.0e-6)
		  APχaxPlus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χax, 1.0e-6)
		  APχaxMinus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χax, -1.0e-6)
		  APχayPlus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χay, 1.0e-6)
		  APχayMinus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χay, -1.0e-6)
		  APχazPlus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χaz, 1.0e-6)
		  APχazMinus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χaz, -1.0e-6)
		  APχsxPlus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χsx, 1.0e-6)
		  APχsxMinus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χsx, -1.0e-6)
		  APχsyPlus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χsy, 1.0e-6)
		  APχsyMinus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χsy, -1.0e-6)
		  APχszPlus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χsz, 1.0e-6)
		  APχszMinus = New AmplitudeParameters(MyParameters, AmplitudeParameters.Item.χsz, -1.0e-6)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdateAmpParameters()
		  Var Cosι As Double = CurrentValues.Cosι
		  Var χa As New Vector(CurrentValues.χax, CurrentValues.χay, CurrentValues.χaz)
		  Var χs As New Vector(CurrentValues.χsx, CurrentValues.χsy, CurrentValues.χsz)
		  AP0.Update(Cosι, χs, χa)
		  APCosιMinus.Update(Cosι, χs, χa)
		  APCosιPlus.Update(Cosι, χs, χa)
		  APβMinus.Update(Cosι, χs, χa)
		  APβPlus.Update(Cosι, χs, χa)
		  APδMinus.Update(Cosι, χs, χa)
		  APδPlus.Update(Cosι, χs, χa)
		  APχaxMinus.Update(Cosι, χs, χa)
		  APχaxPlus.Update(Cosι, χs, χa)
		  APχayMinus.Update(Cosι, χs, χa)
		  APχayPlus.Update(Cosι, χs, χa)
		  APχazMinus.Update(Cosι, χs, χa)
		  APχazPlus.Update(Cosι, χs, χa)
		  APχsxMinus.Update(Cosι, χs, χa)
		  APχsxPlus.Update(Cosι, χs, χa)
		  APχsyMinus.Update(Cosι, χs, χa)
		  APχsyPlus.Update(Cosι, χs, χa)
		  APχszMinus.Update(Cosι, χs, χa)
		  APχszPlus.Update(Cosι, χs, χa)
		  
		End Sub
	#tag EndMethod


	#tag Hook, Flags = &h0
		Event GetTerms()
	#tag EndHook


	#tag Property, Flags = &h0
		AP0 As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APCosιMinus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APCosιPlus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APβMinus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APβPlus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APδMinus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APδPlus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχaxMinus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχaxPlus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχayMinus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχayPlus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχazMinus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχazPlus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχsxMinus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχsxPlus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχsyMinus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχsyPlus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχszMinus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		APχszPlus As AmplitudeParameters
	#tag EndProperty

	#tag Property, Flags = &h0
		Cross As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		CurrentDerivatives As CurrentDerivativesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		CurrentValues As CurrentValuesClass
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDα As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDλ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DhDΨr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		h As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		PNOrder As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		TermData() As HTermData
	#tag EndProperty

	#tag Property, Flags = &h0
		π As Double
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
			Name="π"
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
			Name="DhDα"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDΨr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDλ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DhDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Cross"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
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
	#tag EndViewBehavior
End Class
#tag EndClass
