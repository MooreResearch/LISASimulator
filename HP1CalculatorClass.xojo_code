#tag Class
Protected Class HP1CalculatorClass
Inherits HNCalculator
	#tag Event
		Sub GetTerms()
		  AddTerm(AddressOf GetA1, 2, 2)
		  AddTerm(AddressOf GetA2, 1, 2)
		  AddTerm(AddressOf GetA3, 1, -2)
		  AddTerm(AddressOf GetA4, 2,-2)
		  AddTerm(AddressOf GetA5, 1, 2)
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  Super.Constructor(MyParameters) // Call the superclass
		  Super.Constructor(MyParameters) // Call the superclass
		  Cross = False  // This class is plus polarization
		  PNOrder = 1 // and for zeroth order
		  // This part of the constructor should set up any constants that the class might need
		  // to calculate the wave and its derivatives. Be sure to define the constants as
		  // properties of this particular subclass.
		  // This part of the constructor should set up any constants that the class might need
		  // to calculate the wave and its derivatives. Be sure to define the constants as
		  // properties of this particular subclass.
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA1(AP As AmplitudeParameters) As Double
		  return Parameters.δ*(AP.C1^6)*(-45/32*AP.Sβ-9/32*AP.S3β)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA10(AP As AmplitudeParameters) As Double
		  return Parameters.δ*((-85/256*AP.Cβ-AP.Cβ*AP.C2β/128-AP.Cβ*AP.C2β*AP.C2/32-3/128*AP.Cβ*AP.C2β*AP.C4)*AP.S2-11/64*AP.Cβ*AP.S4-AP.Cβ*AP.S6/256)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA11(AP As AmplitudeParameters) As Double
		  return Parameters.δ*((45/256*AP.Cβ+AP.Cβ*AP.C2β*81/128+AP.Cβ*AP.C2β*AP.C2*27/32+27/128*AP.Cβ*AP.C2β*AP.C4)*AP.S2+9/64*AP.Cβ*AP.S4+AP.Cβ*AP.S6*9/256)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA12(AP As AmplitudeParameters) As Double
		  return Parameters.δ*((-85/256*AP.Cβ+AP.Cβ*AP.C2β*1/256)*AP.S2+(11/64*AP.Cβ+1/64*AP.Cβ*AP.C2β)*AP.S4-(1/256*AP.Cβ+3/256*AP.Cβ*AP.C2β)*AP.S6)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA13(AP As AmplitudeParameters) As Double
		  return Parameters.δ*((45/256*AP.Cβ+AP.Cβ*AP.C2β*135/256)*AP.S2-(9/64*AP.Cβ+27/64*AP.Cβ*AP.C2β)*AP.S4+(9/256*AP.Cβ+27/256*AP.Cβ*AP.C2β)*AP.S6)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA14(AP As AmplitudeParameters) As Double
		  return Parameters.δ*(1/64*AP.Cβ*AP.Sβ^2*AP.S2+5/64*AP.Cβ*AP.Sβ*AP.S6)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA2(AP As AmplitudeParameters) As Double
		  return Parameters.δ*(AP.C1^2)*(-175/256*AP.Sβ+AP.C2*(87/64*AP.Sβ-5/64*AP.S3β)+AP.C4*(-5/256*AP.Sβ+15/256*AP.S3β)+13/256*AP.S3β)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA3(AP As AmplitudeParameters) As Double
		  return Parameters.δ*(AP.S1^2)*(175/256*AP.Sβ+AP.C2*(87/64*AP.Sβ-5/64*AP.S3β)+AP.C4*(5/256*AP.Sβ-15/256*AP.S3β)-13/256*AP.S3β)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA4(AP As AmplitudeParameters) As Double
		  return Parameters.δ*(AP.C1^4)*(AP.S1^2)*(-5/32*AP.Sβ-AP.S3β/32)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA5(AP As AmplitudeParameters) As Double
		  return Parameters.δ*(AP.C1^4)*(AP.S1^2)*(-45/32*AP.Sβ+AP.S3β*135/32)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA6(AP As AmplitudeParameters) As Double
		  return Parameters.δ*(AP.C1^2)*(AP.S1^4)*(45/32*AP.Sβ-AP.S3β*135/32)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA7(AP As AmplitudeParameters) As Double
		  return Parameters.δ*(AP.C1^2)*(AP.S1^4)*(5/32*AP.Sβ+AP.S3β/32)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA8(AP As AmplitudeParameters) As Double
		  return Parameters.δ*(AP.S1^6)*AP.Sβ*(27/16+9/16*AP.C2β)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA9(AP As AmplitudeParameters) As Double
		  return Parameters.δ*(45/16)*(AP.S2^3)*(AP.Cβ)*(AP.Sβ^2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetAllTerms(TheValues As CurrentValuesClass, TheDerivatives As CurrentDerivativesClass) As HTermData()
		  // Provide references to the current values and current derivatives. "CurrentValues" and
		  // "CurrentDerivatives" are properties of the superclass. These classes contain (as properties)
		  // all the information needed to calculate the wave and its derivatives.
		  CurrentValues = TheValues
		  CurrentDerivatives = TheDerivatives
		  
		  // Clear the TermData array for this PN polarization term
		  TermData.RemoveAll
		  Var AP As AmplitudeParameters = new AmplitudeParameters(Parameters)
		  // The calculation of all subterms in this polarization term should appear here. Write a method
		  // to calculate each subterm, then append the result to the TermData array. Something like
		  // TermData.Add(GetTermK), where GetTermK (K = 0, 1, 2, ...) that returns an HTermData instance.
		  // The GetTermK method in turn should call a separate method GetAK(AP As AmplitudeParameters)
		  // that calculates the amplitude alone (since we will need to call this multiple times to handle side cases)
		  // with tweaked values of the the parameters. The rest of the code in GetTermK should create a new
		  // instance of HTermData and set its properties appropriately (ignore the derivatives for the moment).
		  TermData.Add(GetTerm1(AP))
		  TermData.Add(GetTerm2(AP))
		  TermData.Add(GetTerm3(AP))
		  TermData.Add(GetTerm4(AP))
		  TermData.Add(GetTerm5(AP))
		  TermData.Add(GetTerm6(AP))
		  TermData.Add(GetTerm7(AP))
		  TermData.Add(GetTerm8(AP))
		  TermData.Add(GetTerm9(AP))
		  TermData.Add(GetTerm10(AP))
		  TermData.Add(GetTerm11(AP))
		  TermData.Add(GetTerm12(AP))
		  TermData.Add(GetTerm13(AP))
		  TermData.Add(GetTerm14(AP))
		  
		  // At the end, return the array you have created.
		  Return TermData
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm1(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA1(AP), 3, 3)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm10(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA10(AP), 2, 1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm11(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA11(AP), 2, 3)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm12(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA12(AP), 2, -1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm13(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA13(AP), 2, -3)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm14(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA14(AP), 0, 1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm2(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA2(AP), 1, 1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm3(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA3(AP), 1, -1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm4(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA4(AP), 3, 1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm5(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA5(AP), 1, 3)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm6(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA6(AP), 1, -3)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm7(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA7(AP), 3, -1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm8(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA8(AP), 3, -3)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm9(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA9(AP), 0, 3)
		End Function
	#tag EndMethod


	#tag ViewBehavior
		#tag ViewProperty
			Name="π"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
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
	#tag EndViewBehavior
End Class
#tag EndClass
