#tag Class
Protected Class HP2CalculatorClass
Inherits HNCalculator
	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  Super.Constructor(MyParameters) // Call the superclass
		  Cross = False  // This class is plus polarization
		  PNOrder = 2 // and for zeroth order
		  // This part of the constructor should set up any constants that the class might need
		  // to calculate the wave and its derivatives. Be sure to define the constants as
		  // properties of this particular subclass.
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA1(AP As AmplitudeParameters) As Double
		  return (59/16+5/2*AP.C2β-3/16*AP.C4β+(5/24-11/6*AP.C2β+7/24*AP.C4β)*AP.C2-(5/48+1/12*AP.C2β+7/48*AP.C4β)*AP.C4)*AP.C1^4+(-25/16-13/3*AP.C2β+9/16*AP.C4β+(-5/8+11/2*AP.C2β-7/8*AP.C4β)*AP.C2+(5/16+1/4*AP.C2β+7/16*AP.C4β)*AP.C4)*AP.η*AP.C1^4
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA10(AP As AmplitudeParameters) As Double
		  return (56/3*AP.C2β-8/3+AP.η*(8-56*AP.C2β))*AP.C1^3*AP.S2β*AP.S1^5
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA11(AP As AmplitudeParameters) As Double
		  return ((5/6-1/6*AP.C2β)*AP.S2β-2/3*AP.Cβ^2*AP.C2*AP.S2β+AP.η*((-5/2+1/2*AP.C2β)*AP.S2β+2*AP.Cβ^2*AP.C2*AP.S2β))*AP.C1*AP.S1^5
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA12(AP As AmplitudeParameters) As Double
		  return (-(10/3+8/3*AP.C2β+14/3*AP.C4β)+AP.η*(10+8*AP.C2β+14*AP.C4β))*AP.C1^2*AP.S1^6
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA13(AP As AmplitudeParameters) As Double
		  return (-(1/2+1/6*AP.C2β)+AP.η*(3/2+1/2*AP.C2β))*AP.C1^2*AP.Sβ^2*AP.S1^6
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA14(AP As AmplitudeParameters) As Double
		  return 32*(AP.η-1/3)*AP.Cβ^3*AP.C1*AP.Sβ*AP.S1^7
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA15(AP As AmplitudeParameters) As Double
		  return (AP.η*(6+2*AP.C2β)-(2+2/3*AP.C2β))*AP.Sβ^2*AP.S1^8
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA16(AP As AmplitudeParameters) As Double
		  return 1/32*(1/3*(349-25*AP.C2β)*AP.Sβ^2-(25+35*AP.C2β)*AP.C4*AP.Sβ^2)+AP.η*((25*AP.C2β-45)*AP.Sβ^2+(25+35*AP.C2β)*AP.C4*AP.Sβ^2)*AP.S2^2
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA17(AP As AmplitudeParameters) As Double
		  return 1/4*(AP.η*(25+35*AP.C2β)-1/3*(25-35*AP.C2β))*AP.Sβ^2*AP.S2^4
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA18(AP As AmplitudeParameters) As Double
		  return AP.C1^3*(6*AP.S2β-31/12*AP.C2*AP.S2β+1/12*AP.C4*AP.S2β-19/48*AP.S4β)*AP.S1+7/24*AP.C1^3*AP.S4β*AP.S3-7/48*AP.C1^3*AP.S4β*AP.S5+AP.η*(AP.C1^3*(-16/3*AP.S2β+31/4*AP.C2*AP.S2β-1/4*AP.C4*AP.S2β+19/16*AP.S4β)*AP.S1-7/8*AP.C1^3*AP.S4β*AP.S3+7/16*AP.C1^3*AP.S4β*AP.S5)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA2(AP As AmplitudeParameters) As Double
		  return (6+2*AP.C2β)*AP.η*AP.C1^8*AP.Sβ^2-(2+2/3*AP.C2β)*AP.C1^8*AP.Sβ^2
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA3(AP As AmplitudeParameters) As Double
		  return 32*(1/3-AP.η)*AP.Cβ^3*AP.C1^7*AP.Sβ*AP.S1
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA4(AP As AmplitudeParameters) As Double
		  return ((1/6*AP.C2β-5/6)*AP.S2β-2/3*AP.Cβ^2*AP.C2*AP.S2β+AP.η*((5/2-1/2*AP.C2β)*AP.S2β+2*AP.Cβ^2*AP.C2*AP.S2β))*AP.C1^5*AP.S1
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA5(AP As AmplitudeParameters) As Double
		  return (-(10/3+8/3*AP.C2β+14/3*AP.C4β)+AP.η*(10+8*AP.C2β+14*AP.C4β))*AP.C1^6*AP.S1^2
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA6(AP As AmplitudeParameters) As Double
		  return 1/2*(-(1+1/3*AP.C2β)+AP.η*(3+AP.C2β))*AP.C1^6*AP.Sβ^2*AP.S1^2
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA7(AP As AmplitudeParameters) As Double
		  return (8/3-56/3*AP.C2β+AP.η*(56*AP.C2β-8))*AP.C1^5*AP.S2β*AP.S1^3
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA8(AP As AmplitudeParameters) As Double
		  return AP.η*(AP.C1*(16/3*AP.S2β+31/4*AP.C2*AP.S2β+1/4*AP.C4*AP.S2β-19/16*AP.S4β)-7/8*AP.C3*AP.S4β-7/16*AP.C5*AP.S4β)*AP.S1^3+(AP.C1*(-6*AP.S2β-31/12*AP.C2*AP.S2β-1/12*AP.C4*AP.S2β+19/48*AP.S4β)+7/24*AP.C3*AP.S4β+7/48*AP.C5*AP.S4β)*AP.S1^3
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA9(AP As AmplitudeParameters) As Double
		  return (59/16+5/2*AP.C2β-3/16*AP.C4β-(5/24-11/6*AP.C2β+7/24*AP.C4β)*AP.C2-(5/48+1/12*AP.C2β+7/48*AP.C4β)*AP.C4)*AP.S1^4+AP.η*(-25/16-13/3*AP.C2β+9/16*AP.C4β+(5/8-11/2*AP.C2β+7/8*AP.C4β)*AP.C2+(5/16+1/4*AP.C2β+7/16*AP.C4β)*AP.C4)*AP.S1^4
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
		  TermData.Add(GetTerm15(AP))
		  TermData.Add(GetTerm16(AP))
		  TermData.Add(GetTerm17(AP))
		  TermData.Add(GetTerm18(AP))
		  
		  // At the end, return the array you have created.
		  Return TermData
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm1(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA1(AP), 2, 2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm10(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA10(AP), 1, -4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm11(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA11(AP), 3, -2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm12(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA12(AP), 2, -4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm13(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA13(AP), 4, -2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm14(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA14(AP), 3, -4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm15(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA15(AP), 4, -4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm16(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA16(AP), 0, 2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm17(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA17(AP), 0, 4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm18(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA18(AP), 1, 2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm2(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA2(AP), 4, 4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm3(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA3(AP), 3, 4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm4(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA4(AP), 3, 2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm5(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA5(AP), 2, 4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm6(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA6(AP), 4, 2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm7(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA7(AP), 1, 4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm8(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA8(AP), 1, -2)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTerm9(AP As AmplitudeParameters) As HTermData
		  return new HTermData(GetA9(AP), 2, -2)
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
