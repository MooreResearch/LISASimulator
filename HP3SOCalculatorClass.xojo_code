#tag Class
Protected Class HP3SOCalculatorClass
Inherits HNCalculator
	#tag Event
		Sub GetTerms()
		  AddTerm(AddressOf GetA1, 0, 0, False)
		  AddTerm(AddressOf GetA2, 2, 2, False)
		  AddTerm(AddressOf GetA3, 3, 2, False)
		  AddTerm(AddressOf GetA4, 3, -2, False)
		  AddTerm(AddressOf GetA5, 1, 2, False)
		  AddTerm(AddressOf GetA6, 1, -2, False)
		  AddTerm(AddressOf GetA7, 2, -2, False)
		  AddTerm(AddressOf GetA8, 0, 0, False)
		  AddTerm(AddressOf GetA9, 3, 0, False)
		  AddTerm(AddressOf GetA10, 0, 2, False)
		  AddTerm(AddressOf GetA11, 2, 0, False)
		  AddTerm(AddressOf GetA12, 1, 0, False)
		  AddTerm(AddressOf GetA13, 1, 0, True)
		  AddTerm(AddressOf GetA14, 2, 0, True)
		  AddTerm(AddressOf GetA15, 3, 0, True)
		  AddTerm(AddressOf GetA16, 1, -2, True)
		  AddTerm(AddressOf GetA17, 2, -2, True)
		  AddTerm(AddressOf GetA18, 3, -2, True)
		  AddTerm(AddressOf GetA19, 0, 2, True)
		  AddTerm(AddressOf GetA20, 1, 2, True)
		  AddTerm(AddressOf GetA21, 2, 2, True)
		  AddTerm(AddressOf GetA22, 3, 2, True)
		  AddTerm(AddressOf GetA23, 0, 0, False)
		  AddTerm(AddressOf GetA24, 2, 2, False)
		  AddTerm(AddressOf GetA25, 3, 2, False)
		  AddTerm(AddressOf GetA26, 3, -2, False)
		  AddTerm(AddressOf GetA27, 1, 2, False)
		  AddTerm(AddressOf GetA28, 1, -2, False)
		  AddTerm(AddressOf GetA29, 2, -2, False)
		  AddTerm(AddressOf GetA30, 0, 0, False)
		  AddTerm(AddressOf GetA31, 3, 0, False)
		  AddTerm(AddressOf GetA32, 0, 2, False)
		  AddTerm(AddressOf GetA33, 2, 0, False)
		  AddTerm(AddressOf GetA34, 1, 0, False)
		  AddTerm(AddressOf GetA35, 1, 0, True)
		  AddTerm(AddressOf GetA36, 2, 0, True)
		  AddTerm(AddressOf GetA37, 3, 0, True)
		  AddTerm(AddressOf GetA38, 1, -2, True)
		  AddTerm(AddressOf GetA39, 2, -2, True)
		  AddTerm(AddressOf GetA40, 3, -2, True)
		  AddTerm(AddressOf GetA41, 0, 2, True)
		  AddTerm(AddressOf GetA42, 1, 2, True)
		  AddTerm(AddressOf GetA43, 2, 2, True)
		  AddTerm(AddressOf GetA44, 3, 2, True)
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  Super.Constructor(MyParameters) // Call the superclass
		  Cross = False  // This class is plus polarization
		  PNOrder = 3 // and for third post-Newtonian order
		  // This part of the constructor should set up any constants that the class might need
		  // to calculate the wave and its derivatives. Be sure to define the constants as
		  // properties of this particular subclass.
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA1(AP As AmplitudeParameters) As Double
		  Return AP.χsx*(2*AP.Cβ*AP.C2^2*AP.Sβ-AP.η*AP.Cβ*AP.C2^3*AP.Sβ)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA10(AP As AmplitudeParameters) As Double
		  Return AP.χsx*(10/3+1/3*AP.η)*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2+AP.χsz*(5+AP.η/2)*AP.C2*AP.Sβ^2*AP.S2^2
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA11(AP As AmplitudeParameters) As Double
		  Return AP.χsz*(3/2+AP.C2β/2-AP.η*(3/4+AP.C2β/4))*AP.C2*AP.S2^2+AP.χsx*(AP.η/2-1)*AP.C2*AP.S2β*AP.S2^2
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA12(AP As AmplitudeParameters) As Double
		  Return AP.χsx*(-11/16*AP.C2β*AP.S2-3/4*AP.S2^3-7/16*AP.C2β*AP.S6+AP.η*(11/32*AP.C2β*AP.S2+3/8*AP.S2^3+7/32*AP.C2β*AP.S6))+AP.χsz*(AP.S2β*AP.S2/2-AP.S2β*AP.S6/2+AP.η*(-1/4*AP.S2β*AP.S2+1/4*AP.S2β*AP.S6))
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA13(AP As AmplitudeParameters) As Double
		  Return AP.χsy*((15/8-3/8*AP.C2β+(9/8-5/8*AP.C2β)*AP.C4)*AP.S2+AP.η*(-15/16+3/16*AP.C2β+(-9/16+5/16*AP.C2β)*AP.C4)*AP.S2)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA14(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(AP.η-2)*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA15(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(3/4+AP.C2β/4-AP.η*(3/8+AP.C2β/8))*AP.S2^3
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA16(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(AP.C1*(5/2-11/6*AP.C2β+(15/2-25/6*AP.C2β)*AP.C2)*AP.S1^3+AP.η*AP.C1*(1/4-31/12*AP.C2β+(3/4-5/12*AP.C2β)*AP.C2)*AP.S1^3)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA17(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(-(7/3*AP.S2β+10/3*AP.C2*AP.S2β)*AP.S1^4-AP.η*(5/6*AP.S2β+1/3*AP.C2*AP.S2β)*AP.S1^4)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA18(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(5+5/3*AP.C2β+AP.η*(1/2+AP.C2β/6))*AP.C1*AP.S1^5
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA19(AP As AmplitudeParameters) As Double
		  Return -AP.χsy*(1/3+11/6*AP.η)*AP.Cβ*AP.Sβ*AP.S2^2
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA2(AP As AmplitudeParameters) As Double
		  Return AP.χsz*(AP.η*AP.C1^4*(-5/2-7/2*AP.C2β+(1/2+AP.C2β/6)*AP.C2)+AP.C1^4*(-3-AP.C2β+(5+5/3*AP.C2β)*AP.C4))+AP.χsx*(AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β)-AP.η*AP.C1^4*(19/6*AP.S2β+1/3*AP.C2*AP.S2β))
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA20(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(AP.η*AP.C1^3*(1/4-31/12*AP.C2β)+(-3/4+5/12*AP.C2β)*AP.C2)*AP.S1+AP.C1^3*(5/2-11/6*AP.C2β+(-15/2+25/6*AP.C2β)*AP.C2*AP.S1)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA21(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β)+AP.η*AP.C1^4*(5/6*AP.S2β-1/3*AP.C2*AP.S2β))
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA22(AP As AmplitudeParameters) As Double
		  Return AP.χsy*(AP.η*(1/2+AP.C2β/6)+5+5/3*AP.C2β)*AP.C1^5*AP.S1
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA23(AP As AmplitudeParameters) As Double
		  Return 2*Parameters.δ*AP.χax*AP.Cβ*AP.C2^3*AP.Sβ
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA24(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χaz*AP.C1^4*(-3-AP.C2β+(5+5/3*AP.C2β)*AP.C2)+AP.χax*AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β))
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA25(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*AP.χax*(5+5/3*AP.C2β)*AP.C1^5*AP.S1
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA26(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*AP.χax*(5+5/3*AP.C2β)*AP.C1*AP.S1^5
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA27(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χax*(3/2-13/6*AP.C2β+(-5/2+35/6*AP.C2β)*AP.C2)+AP.χaz*(-2*AP.S2β+20/3*AP.C2*AP.S2β))*AP.C1^3*AP.S1
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA28(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χax*(3/2-13/6*AP.C2β+(5/2-35/6*AP.C2β)*AP.C2)+AP.χaz*(-2*AP.S2β-20/3*AP.C2*AP.S2β))*AP.C1*AP.S1^3
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA29(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χaz*(3+AP.C2β+(5+5/3*AP.C2β)*AP.C2)*AP.S1^4-AP.χax*(7/2*AP.S2β+10/3*AP.C2*AP.S2β)*AP.S1^4)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA3(AP As AmplitudeParameters) As Double
		  Return AP.χsx*(AP.η*(1/2+AP.C2β/6)*AP.C1^5*AP.S1+(5+5/3*AP.C2β)*AP.C1^5*AP.S1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA30(AP As AmplitudeParameters) As Double
		  Return -3*Parameters.δ*AP.χaz*AP.C2*AP.Sβ^2*AP.S2^2
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA31(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*AP.χax*(3/4+AP.C2β/4)*AP.S2^3
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA32(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(10/3*AP.χax*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2+5*AP.χaz*AP.C2*AP.Sβ^2*AP.S2^2)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA33(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*AP.χaz*(3/2*AP.C2β/2)*AP.C2*AP.S2^2-AP.χax*AP.C2*AP.S2β*AP.S2^2
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA34(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χax*(-11/16*AP.C2β*AP.S2-3/4*AP.S2^3-7/16*AP.C2β*AP.S6)+AP.χaz*(AP.S2β*AP.S2/2-AP.S2β*AP.S6/2))
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA35(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χay*(15/8-3/8*AP.C2β+(9/8-5/8*AP.C2β)*AP.C4)*AP.S2)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA36(AP As AmplitudeParameters) As Double
		  Return -2*Parameters.δ*AP.χay*AP.Cβ*AP.C2*AP.Sβ*AP.S2^2
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA37(AP As AmplitudeParameters) As Double
		  
		  Return Parameters.δ*AP.χay*(3/4+AP.C2β/4)*AP.S2^3
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA38(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*AP.χay*AP.C1*(5/2-11/6*AP.C2β+(15/2-25/6*AP.C2β)*AP.C2)*AP.S1^3
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA39(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*AP.χay*(-7/3*AP.S2β-10/3*AP.C2*AP.S2β)*AP.S1^4
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA4(AP As AmplitudeParameters) As Double
		  Return AP.χsx*(AP.η*(1/2+AP.C2β/6)*AP.C1*AP.S1^5+(5+5/3*AP.C2β)*AP.C1*AP.S1^5)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA40(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*AP.χay*(5+5/3*AP.C2β)*AP.C1*AP.S1^5
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA41(AP As AmplitudeParameters) As Double
		  
		  Return -1/3*Parameters.δ*AP.χay*AP.Cβ*AP.Sβ*AP.S2^2
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA42(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*AP.χay*AP.C1^3*(5/2-11/6*AP.C2β+(-15/2+25/6*AP.C2β)*AP.C2)*AP.S1
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA43(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*AP.χay*AP.C1^4*(7/3*AP.S2β-10/3*AP.C2*AP.S2β)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA44(AP As AmplitudeParameters) As Double
		  
		  Return Parameters.δ*AP.χay*(5+5/3*AP.C2β)*AP.C1^5*AP.S1
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA5(AP As AmplitudeParameters) As Double
		  
		  Return AP.χsx*(AP.η*AP.C1^3*(-17/4+79/12*AP.C2β+(-1/4+7/12*AP.C2β)*AP.C2)*AP.S1+AP.C1^3*(3/2-13/6*AP.C2β+(-5/2+35/6*AP.C2β)*AP.C2)*AP.S1)+AP.χsz*(AP.η*AP.C1^3*(-7*AP.S2β+2/3*AP.C2*AP.S2β)*AP.S1+AP.C1^3*(-2*AP.S2β+20/3*AP.C2*AP.S2β)*AP.S1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA6(AP As AmplitudeParameters) As Double
		  Return AP.χsx*(AP.C1*(3/2-13/6*AP.C2β+(5/2-35/6*AP.C2β)*AP.C2)*AP.S1^3+AP.η*AP.C1*(-17/4+79/12*AP.C2β+(1/4-7/12*AP.C2β)*AP.C2)*AP.S1^3)+AP.χsz*(-AP.C1*(2*AP.S2β+20/3*AP.C2*AP.S2β)*AP.S1^3-AP.η*AP.C1*(7*AP.S2β+2/3*AP.C2*AP.S2β)*AP.S1^3)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA7(AP As AmplitudeParameters) As Double
		  Return AP.χsz*(AP.η*(5/2+7/2*AP.C2β+(1/2+AP.C2β/6)*AP.C2)*AP.S1^4+(3+AP.C2β+(5+5/3*AP.C2β)*AP.C2)*AP.S1^4)+AP.χsx*(-(7/3*AP.S2β+10/3*AP.C2*AP.S2β)*AP.S1^4+AP.η*(19/6*AP.S2β-1/3*AP.C2*AP.S2β)*AP.S1^4)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA8(AP As AmplitudeParameters) As Double
		  Return AP.χsz*(-3+3/2*AP.η)*AP.C2*AP.Sβ^2*AP.S2^2
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA9(AP As AmplitudeParameters) As Double
		  Return AP.χsx*(3/4+AP.C2β/4-AP.η*(3/8+AP.C2β/8))*AP.S2^3
		  
		  
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
