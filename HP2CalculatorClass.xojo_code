#tag Class
Protected Class HP2CalculatorClass
Inherits HNCalculator
	#tag Event
		Sub GetTerms()

		  AddTerm(AddressOf GetA1, 2, 2, False)
		  AddTerm(AddressOf GetA2, 4, 4, False)
		  AddTerm(AddressOf GetA3, 3, 4, False)
		  AddTerm(AddressOf GetA4, 3, 2, False)
		  AddTerm(AddressOf GetA5, 2, 4, False)
		  AddTerm(AddressOf GetA6, 4, 2, False)
		  AddTerm(AddressOf GetA7, 1, 4, False)
		  AddTerm(AddressOf GetA8, 1, -2, False)
		  AddTerm(AddressOf GetA9, 2, -2, False)
		  AddTerm(AddressOf GetA10, 1, -4, False)
		  AddTerm(AddressOf GetA11, 3, -2, False)
		  AddTerm(AddressOf GetA12, 2, -4, False)
		  AddTerm(AddressOf GetA13, 4, -2, False)
		  AddTerm(AddressOf GetA14, 3, -4, False)
		  AddTerm(AddressOf GetA15, 4, -4, False)
		  AddTerm(AddressOf GetA16, 0, 2, False)
		  AddTerm(AddressOf GetA17, 0, 4, False)
		  AddTerm(AddressOf GetA18, 1, 2, False)

		End Sub
	#tag EndEvent


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
