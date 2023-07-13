#tag Class
Protected Class HX2CalculatorClass
Inherits HNCalculator
	#tag Event
		Sub GetTerms()
		  AddTerm(AddressOf GetA1, 2, 2, True)
		  AddTerm(AddressOf GetA2, 4, 4, True)
		  AddTerm(AddressOf GetA3, 3, 4, True)
		  AddTerm(AddressOf GetA4, 3, 2, True)
		  AddTerm(AddressOf GetA5, 2, 4, True)
		  AddTerm(AddressOf GetA6, 4, 2, True)
		  AddTerm(AddressOf GetA7, 1, 4, True)
		  AddTerm(AddressOf GetA8, 1, -2, True)
		  AddTerm(AddressOf GetA9, 2, -2, True)
		  AddTerm(AddressOf GetA10, 1, -4, True)
		  AddTerm(AddressOf GetA11, 3, -2, True)
		  AddTerm(AddressOf GetA12, 2, -4, True)
		  AddTerm(AddressOf GetA13, 4, -2, True)
		  AddTerm(AddressOf GetA14, 3, -4, True)
		  AddTerm(AddressOf GetA15, 4, -4, True)
		  AddTerm(AddressOf GetA16, 0, 2, True)
		  AddTerm(AddressOf GetA17, 0, 4, True)
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  Super.Constructor(MyParameters) // Call the superclass
		  Cross = True  // This class is cross polarization
		  PNOrder = 2 // and for zeroth order
		  // This part of the constructor should set up any constants that the class might need
		  // to calculate the wave and its derivatives. Be sure to define the constants as
		  // properties of this particular subclass.
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA1(AP As AmplitudeParameters) As Double
		  Return (4*AP.Sβ+28/3*AP.S3β-AP.η*(12*AP.Sβ+28*AP.S3β))*AP.C1^3*AP.S1^5
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA10(AP As AmplitudeParameters) As Double
		  Return AP.C1^3*(79/8*AP.Sβ+AP.C2*(3/4*AP.Sβ-19/12*AP.S3β)-AP.C4*(AP.Sβ/8+7/24*AP.S3β)+3/8*AP.S3β)*AP.S1+AP.η*AP.C1^3*(-103/24*AP.Sβ+AP.C4*(3/8*AP.Sβ+7/8*AP.S3β)-9/8*AP.S3β+AP.C2*(-9/4*AP.Sβ+19/4*AP.S3β))*AP.S1
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA11(AP As AmplitudeParameters) As Double
		  Return AP.C1^4*(47/8*AP.Cβ+AP.C3β/8-(7/6*AP.Cβ+AP.C3β/6)*AP.C2-(AP.Cβ/24+7/24*AP.C3β)*AP.C4)+AP.η*AP.C1^4*(-119/24*AP.Cβ-3/8*AP.C3β+(7/2*AP.Cβ+AP.C3β/2)*AP.C2+(AP.Cβ/8+7/8*AP.C3β)*AP.C4)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA12(AP As AmplitudeParameters) As Double
		  Return (-4/3*AP.Sβ-(1/3+AP.C2β)*AP.C2*AP.Sβ+AP.η*(4*AP.Sβ+(1+3*AP.C2β)*AP.C2*AP.Sβ))*AP.C1^5*AP.S1
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA13(AP As AmplitudeParameters) As Double
		  Return (2*AP.η-2/3)*AP.Cβ*AP.C1^6*AP.Sβ^2*AP.S1^2
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA14(AP As AmplitudeParameters) As Double
		  Return (AP.η*(12*AP.Sβ+28*AP.S3β)-(4*AP.Sβ+28/3*AP.S3β))*AP.C1^5*AP.S1^3
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA15(AP As AmplitudeParameters) As Double
		  Return (AP.η*(4*AP.Cβ+28*AP.C3β)-(4/3*AP.Cβ+28/3*AP.C3β))*AP.C1^6*AP.S1^2
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA16(AP As AmplitudeParameters) As Double
		  Return (8/3+8*AP.C2β-AP.η*(8+24*AP.C2β))*AP.C1^7*AP.Sβ*AP.S1
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA17(AP As AmplitudeParameters) As Double
		  Return (8*AP.η-8/3)*AP.Cβ*AP.C1^8*AP.Sβ^2
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA2(AP As AmplitudeParameters) As Double
		  Return (AP.η*(4*AP.Cβ+28*AP.C3β-(4/3*AP.Cβ+28/3*AP.C3β)))*AP.C1^2*AP.S1^6
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA3(AP As AmplitudeParameters) As Double
		  Return ((4/3*AP.Sβ-4*AP.S3β)+AP.η*(-4*AP.Sβ+12*AP.S3β))*AP.C1*AP.S1^7
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA4(AP As AmplitudeParameters) As Double
		  Return (8*AP.η-8/3)*AP.Cβ*AP.Sβ*AP.S1^8
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA5(AP As AmplitudeParameters) As Double
		  Return AP.C1*(-79/8*AP.Sβ+AP.C2*(3/4*AP.Sβ-19/12*AP.S3β)+AP.C4*(AP.Sβ/8+7/24*AP.S3β)-3/8*AP.S3β)*AP.S1^3+AP.η*AP.C1*(103/24*AP.Sβ-AP.C4*(3/8*AP.Sβ+7/8*AP.S3β)+9/8*AP.S3β+AP.C2*(-9/4*AP.Sβ+19/4*AP.S3β))*AP.S1^3
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA6(AP As AmplitudeParameters) As Double
		  Return (47/8*AP.Cβ+AP.C3β/8+(7/6*AP.Cβ+AP.C3β/6)*AP.C2-(AP.Cβ/24+7/24*AP.C3β)*AP.C4+AP.η*(-119/24*AP.Cβ-3/8*AP.C3β-(7/2*AP.Cβ+AP.C3β/2)*AP.C2+(AP.Cβ/8+7/8*AP.C3β)*AP.C4))*AP.S1^4
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA7(AP As AmplitudeParameters) As Double
		  Return (4/3*AP.Sβ-(1/3+AP.C2β)*AP.C2*AP.Sβ+AP.η*(-4*AP.Sβ+(1+3*AP.C2β)*AP.C2*AP.Sβ))*AP.C1*AP.S1^5
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA8(AP As AmplitudeParameters) As Double
		  Return (2*AP.η-2/3)*AP.Cβ*AP.C1^2*AP.Sβ^2*AP.S1^6
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA9(AP As AmplitudeParameters) As Double
		  Return (15/2*AP.η-5/2)*AP.Cβ*AP.C2*AP.Sβ^2*AP.S2^2
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetAllTerms(TheValues As CurrentValuesClass, TheDerivatives As CurrentDerivativesClass) As HTermData()
		  '// Provide references to the current values and current derivatives. "CurrentValues" and
		  '// "CurrentDerivatives" are properties of the superclass. These classes contain (as properties)
		  '// all the information needed to calculate the wave and its derivatives.
		  'CurrentValues = TheValues
		  'CurrentDerivatives = TheDerivatives
		  '
		  '// Clear the TermData array for this PN polarization term
		  'TermData.RemoveAll
		  'Var AP As AmplitudeParameters = new AmplitudeParameters(Parameters)
		  '// The calculation of all subterms in this polarization term should appear here. Write a method
		  '// to calculate each subterm, then append the result to the TermData array. Something like
		  '// TermData.Add(GetTermK), where GetTermK (K = 0, 1, 2, ...) that returns an HTermData instance.
		  '// The GetTermK method in turn should call a separate method GetAK(AP As AmplitudeParameters)
		  '// that calculates the amplitude alone (since we will need to call this multiple times to handle side cases)
		  '// with tweaked values of the the parameters. The rest of the code in GetTermK should create a new
		  '// instance of HTermData and set its properties appropriately (ignore the derivatives for the moment).
		  '//TermData.Add(GetTerm1(AP))
		  '//TermData.Add(GetTerm2(AP))
		  '//TermData.Add(GetTerm3(AP))
		  '//TermData.Add(GetTerm4(AP))
		  '//TermData.Add(GetTerm5(AP))
		  '//TermData.Add(GetTerm6(AP))
		  '//TermData.Add(GetTerm7(AP))
		  '//TermData.Add(GetTerm8(AP))
		  '//TermData.Add(GetTerm9(AP))
		  '//TermData.Add(GetTerm10(AP))
		  ''TermData.Add(GetTerm11(AP))
		  ''TermData.Add(GetTerm12(AP))
		  ''TermData.Add(GetTerm13(AP))
		  ''TermData.Add(GetTerm14(AP))
		  ''TermData.Add(GetTerm15(AP))
		  ''TermData.Add(GetTerm16(AP))
		  ''TermData.Add(GetTerm17(AP))
		  ''TermData.Add(GetTerm18(AP))
		  '
		  '// At the end, return the array you have created.
		  'Return TermData
		  
		  
		  
		End Function
	#tag EndMethod


	#tag ViewBehavior
		#tag ViewProperty
			Name="HAdjusted"
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
