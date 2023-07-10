#tag Class
Protected Class HX0CalculatorClass
Inherits HNCalculator
	#tag Event
		Sub GetTerms()
		  AddTerm(AddressOf GetA1(AP), 1, -2)
		  AddTerm(AddressOf GetA2(AP), 2, -2)
		  AddTerm(AddressOf GetA3(AP), 1, 2)
		  AddTerm(AddressOf GetA4(AP), 2, 2)
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  Super.Constructor(MyParameters) // Call the superclass
		  Cross = True  // This class is plus polarization
		  PNOrder = 0 // and for zeroth order
		  // This part of the constructor should set up any constants that the class might need
		  // to calculate the wave and its derivatives. Be sure to define the constants as
		  // properties of this particular subclass.
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA1(AP As AmplitudeParameters) As Double
		  return 4.0*AP.C1*AP.Sβ*AP.S1^3
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA2(AP As AmplitudeParameters) As Double
		  return -2.0*AP.Cβ*AP.S1^4
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA3(AP As AmplitudeParameters) As Double
		  return -4.0*AP.C1^3*AP.Sβ*AP.S1
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA4(AP As AmplitudeParameters) As Double
		  return -2*AP.Cβ*AP.C1^4
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
		  // At the end, return the array you have created.
		  Return TermData
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm1(AP As AmplitudeParameters) As HTermData
		  
		  return new HTermData(GetA1(AP), 1, -2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm2(AP As AmplitudeParameters) As HTermData
		  
		  return new HTermData(GetA2(AP), 2, -2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm3(AP As AmplitudeParameters) As HTermData
		  
		  return new HTermData(GetA3(AP), 1, 2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm4(AP As AmplitudeParameters) As HTermData
		  
		  return new HTermData(GetA4(AP), 2, 2)
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
