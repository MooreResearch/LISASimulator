#tag Class
Protected Class HX3SOCalculatorClass
Inherits HNCalculator
	#tag Event
		Sub GetTerms()
		  AddTerm(AddressOf GetA1, 0, 0, False)
		  AddTerm(AddressOf GetA2, 2, 2, False)
		  AddTerm(AddressOf GetA3, 3, 2, False)
		  AddTerm(AddressOf GetA4, 1, 2, False)
		  AddTerm(AddressOf GetA5, 1, -2, False)
		  AddTerm(AddressOf GetA6, 2, -2, False)
		  AddTerm(AddressOf GetA7, 3, -2, False)
		  AddTerm(AddressOf GetA8, 2, 0, False)
		  AddTerm(AddressOf GetA9, 0, 2, False)
		  AddTerm(AddressOf GetA10, 3, 0, False)
		  AddTerm(AddressOf GetA11, 1, 0, False)
		  AddTerm(AddressOf GetA12, 1, 0, True)
		  AddTerm(AddressOf GetA13, 2, 0, True)
		  AddTerm(AddressOf GetA14, 3, 0, True)
		  AddTerm(AddressOf GetA15, 1, -2, True)
		  AddTerm(AddressOf GetA16, 2, -2, True)
		  AddTerm(AddressOf GetA17, 3, -2, True)
		  AddTerm(AddressOf GetA18, 0, 2, True)
		  AddTerm(AddressOf GetA19, 1, 2, True)
		  AddTerm(AddressOf GetA20, 2, 2, True)
		  AddTerm(AddressOf GetA21, 3, 2, True)
		  AddTerm(AddressOf GetA22, 0, 0, False)
		  AddTerm(AddressOf GetA23, 2, 2, False)
		  AddTerm(AddressOf GetA24, 1, 2, False)
		  AddTerm(AddressOf GetA25, 3, 2, False)
		  AddTerm(AddressOf GetA26, 1, -2, False)
		  AddTerm(AddressOf GetA27, 2, -2, False)
		  AddTerm(AddressOf GetA28, 3, -2, False)
		  AddTerm(AddressOf GetA29, 2, 0, False)
		  AddTerm(AddressOf GetA30, 0, 2, False)
		  AddTerm(AddressOf GetA31, 3, 0, False)
		  AddTerm(AddressOf GetA32, 1, 0, False)
		  AddTerm(AddressOf GetA33, 1, 0, True)
		  AddTerm(AddressOf GetA34, 2, 0, True)
		  AddTerm(AddressOf GetA35, 3, 0, True)
		  AddTerm(AddressOf GetA36, 1, -2, True)
		  AddTerm(AddressOf GetA37, 2, -2, True)
		  AddTerm(AddressOf GetA38, 3, -2, True)
		  AddTerm(AddressOf GetA39, 0, 2, True)
		  AddTerm(AddressOf GetA40, 1, 2, True)
		  AddTerm(AddressOf GetA41, 2, 2, True)
		  AddTerm(AddressOf GetA42, 3, 2, True)
		  
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  Super.Constructor(MyParameters) // Call the superclass
		  Cross = True  // This class is cross polarization
		  PNOrder = 3 // and for third post-Newtonian order
		  // This part of the constructor should set up any constants that the class might need
		  // to calculate the wave and its derivatives. Be sure to define the constants as
		  // properties of this particular subclass.
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA1(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(2*AP.C2^3*AP.Sβ-AP.η*AP.C2^3*AP.Sβ)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA10(AP As AmplitudeParameters) As Double
		  
		  
		  Return AP.χsy*(-AP.Cβ*AP.S2^3+1/2*AP.η*AP.Cβ*AP.S2^3)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA11(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(-5/4*AP.Cβ*AP.S2-1/4*AP.Cβ*AP.S6+AP.η*(5*AP.Cβ*AP.S2/8+1/8*AP.Cβ*AP.S6))
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA12(AP As AmplitudeParameters) As Double
		  
		  
		  Return AP.χsx*((-3*AP.Cβ/2-1/2*AP.Cβ*AP.C4)*AP.S2+AP.η*(3*AP.Cβ+1/4*AP.Cβ*AP.C4)*AP.S2)+AP.χsz*(-2*AP.C4*AP.Sβ*AP.S2+AP.η*AP.C4*AP.Sβ*AP.S2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA13(AP As AmplitudeParameters) As Double
		  
		  
		  Return AP.χsz*(2*AP.Cβ*AP.C2*AP.S2^2-AP.η*AP.Cβ*AP.C2*AP.S2^2)+AP.χsx*(-2*AP.C2*AP.Sβ*AP.S2^2+AP.η*AP.C2*AP.Sβ*AP.S2^2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA14(AP As AmplitudeParameters) As Double
		  
		  
		  Return AP.χsx*(AP.Cβ*AP.S2^3-1/2*AP.η*AP.Cβ*AP.S2^3)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA15(AP As AmplitudeParameters) As Double
		  Return AP.χsx*(AP.C1*(-2*AP.Cβ/3-10*AP.Cβ*AP.C2/3)*AP.S1^3+AP.η*AP.C1*(-5/3*AP.Cβ+4*AP.C3β-AP.Cβ*AP.C2/3)*AP.S1^3)+AP.χsz*(AP.C1*(-4*AP.Sβ-40*AP.C2*AP.Sβ/3)*AP.S1^3+AP.η*AP.C1*(-2*AP.Sβ-4*AP.C2*AP.Sβ/3-4*AP.S3β)*AP.S1^3)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA16(AP As AmplitudeParameters) As Double
		  
		  
		  Return AP.χsz*(AP.η*(5*AP.Cβ+AP.C3β+2*AP.Cβ*AP.C2/3)*AP.S1^4+(4*AP.Cβ+20*AP.Cβ*AP.C2/3)*AP.S1^4)+AP.χsx*((-14*AP.Sβ/3-20*AP.C2*AP.Sβ/3)*AP.S1^4+AP.η*(10*AP.Sβ/3-2*AP.C2*AP.Sβ/3+AP.S3β)*AP.S1^4)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA17(AP As AmplitudeParameters) As Double
		  
		  
		  Return AP.χsx*(20/3*AP.Cβ*AP.C1*AP.S1^5+2/3*AP.η*AP.Cβ*AP.C1*AP.S1^5)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA18(AP As AmplitudeParameters) As Double
		  
		  
		  Return -6*AP.β*AP.χsz*AP.Cβ*AP.Sβ^2*AP.S2^2 + AP.χsx*(1/3*AP.Sβ*AP.S2^2+AP.η*(-7/6+3*AP.C2β)*AP.Sβ*AP.S2^2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA19(AP As AmplitudeParameters) As Double
		  
		  
		  Return AP.χsx*(AP.η*AP.C1^3*(-5*AP.Cβ/3+4*AP.C3β+AP.Cβ*AP.C2/3)*AP.S1+AP.C1^3*(-2*AP.Cβ/3+10*AP.Cβ*AP.C2/3)*AP.S1)+AP.χsz*(AP.C1^3*(-4*AP.Sβ+40*AP.C2*AP.Sβ/3)*AP.S1+AP.η*AP.C1^3*(-2*AP.Sβ+4*AP.C2*AP.Sβ/3-4*AP.S3β)*AP.S1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA2(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(AP.η*AP.C1^4*(-5*AP.Sβ/3 + 2*AP.C2*AP.Sβ/3)+AP.C1^4*(-14*AP.Sβ/3+20*AP.C2*AP.Sβ/3))
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA20(AP As AmplitudeParameters) As Double
		  Return AP.χsz*(AP.η*AP.C1^4*(-5*AP.Cβ-AP.C3β+2*AP.Cβ*AP.C2/3)+AP.C1^4*(-4*AP.Cβ+20*AP.Cβ*AP.C2/3))+AP.χsx*(AP.C1^4*(14*AP.Sβ/3-20*AP.C2*AP.Sβ/3)+AP.η*AP.C1^4*(-10*AP.Sβ/3-2*AP.C2*AP.Sβ/3-AP.S3β))
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA21(AP As AmplitudeParameters) As Double
		  
		  
		  Return AP.χsx*(20/3*AP.Cβ*AP.C1^5*AP.S1+2/3*AP.η*AP.Cβ*AP.C1^5*AP.S1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA22(AP As AmplitudeParameters) As Double
		  
		  Return Parameters.δ*(2*AP.χay*AP.C2^3*AP.Sβ)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA23(AP As AmplitudeParameters) As Double
		  
		  
		  Return Parameters.δ*(AP.χay*AP.C1^4*(-14/3*AP.Sβ+20/3*AP.C2*AP.Sβ))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA24(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χay*AP.C1^3*(-2/3*AP.Cβ+10/3*AP.Cβ*AP.C2)*AP.S1)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA25(AP As AmplitudeParameters) As Double
		  
		  
		  Return Parameters.δ*(-20/3*AP.χay*AP.Cβ*AP.C1^5*AP.S1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA26(AP As AmplitudeParameters) As Double
		  
		  
		  Return Parameters.δ*(AP.χay*AP.C1*(-2/3*AP.Cβ-10/3*AP.Cβ*AP.C2)*AP.S1^3)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA27(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χay*(14/3*AP.Sβ+20/3*AP.C2*AP.Sβ)*AP.S1^4)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA28(AP As AmplitudeParameters) As Double
		  
		  
		  Return Parameters.δ*(-20/3*AP.χay*AP.Cβ*AP.C1*AP.S1^5)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA29(AP As AmplitudeParameters) As Double
		  
		  Return Parameters.δ*(2*AP.χay*AP.C2*AP.Sβ*AP.S2^2)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA3(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(-20/3*AP.Cβ*AP.C1^5*AP.S1-2/3*AP.η*AP.Cβ*AP.C1^5*AP.S1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA30(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(10/3*AP.χay*AP.C2*AP.Sβ*AP.S2^2)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA31(AP As AmplitudeParameters) As Double
		  
		  
		  Return Parameters.δ*(-AP.χay*AP.Cβ*AP.S1^3)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA32(AP As AmplitudeParameters) As Double
		  
		  
		  Return Parameters.δ*(AP.χay*(-5/4*AP.Cβ*AP.S2-1/4*AP.Cβ*AP.S6))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA33(AP As AmplitudeParameters) As Double
		  
		  
		  Return Parameters.δ*(AP.χax*(-3/2*AP.Cβ-AP.Cβ*AP.C4/2)*AP.S2-2*AP.χaz*AP.C4*AP.Sβ*AP.S2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA34(AP As AmplitudeParameters) As Double
		  
		  
		  Return Parameters.δ*(2*AP.χaz*AP.Cβ*AP.C2*AP.S2^2-2*AP.χax*AP.C2*AP.Sβ*AP.S2^2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA35(AP As AmplitudeParameters) As Double
		  
		  
		  Return Parameters.δ*(AP.χax*AP.Cβ*AP.S2^3)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA36(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χax*AP.C1*(-2/3*AP.Cβ-10/3*AP.Cβ*AP.C2)*AP.S1^3+AP.χaz*AP.C1*(-4*AP.Sβ-40/3*AP.C2*AP.Sβ)*AP.S1^3)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA37(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χaz*(4*AP.Cβ+20/3*AP.Cβ*AP.C2)*AP.S1^4-AP.χax*(14/3*AP.Sβ+20/3*AP.C2*AP.Sβ)*AP.S1^4)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA38(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(20/3*AP.χax*AP.Cβ*AP.C1*AP.S1^5)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA39(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(1/3*AP.χax*AP.Sβ*AP.S2^2)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA4(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(AP.η*AP.C1^3*(7*AP.Cβ/3+AP.Cβ*AP.C2/3)*AP.S1+AP.C1^3*(-2*AP.Cβ/3+10*AP.Cβ*AP.C2/3)*AP.S1)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA40(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χax*AP.C1^3*(-2/3*AP.Cβ+10/3*AP.Cβ*AP.C2)*AP.S1+AP.χaz*AP.C1^3*(-4*AP.Sβ+40/3*AP.C2*AP.Sβ)*AP.S1)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA41(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χaz*AP.C1^4*(-4*AP.Cβ+20/3*AP.Cβ*AP.C2)+AP.χax*AP.C1^4*(14/3*AP.Sβ-20/3*AP.C2*AP.Sβ))
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA42(AP As AmplitudeParameters) As Double
		  
		  
		  Return Parameters.δ*(20/3*AP.χax*AP.Cβ*AP.C1^5*AP.S1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA5(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(AP.η*AP.C1*(7*AP.Cβ/3 - AP.Cβ*AP.C2/3)*AP.S1^3 + AP.C1*(-2*AP.Cβ/3-10*AP.Cβ*AP.C2/3)*AP.S1^3)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA6(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(AP.η*(5*AP.Sβ/3+2*AP.C2*AP.Sβ/3)*AP.S1^4+(14*AP.Sβ/3+20*AP.C2*AP.Sβ/3)*AP.S1^4)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA7(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(-20/3*AP.Cβ*AP.C1*AP.S1^5-2/3*AP.η*AP.Cβ*AP.C1*AP.S1^5)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA8(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(2*AP.C2*AP.Sβ*AP.S2^2-AP.η*AP.C2*AP.Sβ*AP.S2^2)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA9(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(10/3*AP.C2*AP.Sβ*AP.S2^2+1/3*AP.η*AP.C2*AP.Sβ*AP.S2^2)
		  
		  
		  
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
