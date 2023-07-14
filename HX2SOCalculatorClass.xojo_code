#tag Class
Protected Class HX2SOCalculatorClass
Inherits HNCalculator
	#tag Event
		Sub GetTerms()
		  AddTerm(AddressOf GetA1, 1, 1, False)
		  AddTerm(AddressOf GetA2, 1, -1, False)
		  AddTerm(AddressOf GetA3, 1, -1, True)
		  AddTerm(AddressOf GetA4, 0,1, True)
		  AddTerm(AddressOf GetA5, 1, 1, True)
		  AddTerm(AddressOf GetA6, 1, 1, False)
		  AddTerm(AddressOf GetA7, 1, -1, False)
		  AddTerm(AddressOf GetA8, 1, -1, True)
		  AddTerm(AddressOf GetA9, 0, 1, True)
		  AddTerm(AddressOf GetA10,1, 1, True)
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  Super.Constructor(MyParameters) // Call the superclass
		  Cross = True  // This class is cross polarization
		  PNOrder = 2 // and for second post-Newtonian order
		  // This part of the constructor should set up any constants that the class might need
		  // to calculate the wave and its derivatives. Be sure to define the constants as
		  // properties of this particular subclass.
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA1(AP As AmplitudeParameters) As Double
		  Return AP.χay*(1/2+AP.C2/2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA10(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χsx*(AP.Cβ^2/2+AP.Cβ^2*AP.C2/2)+AP.χsz*(-AP.Cβ*AP.Sβ/2-AP.Cβ*AP.C2*AP.Sβ/2))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA2(AP As AmplitudeParameters) As Double
		  Return AP.χay*AP.S1^2
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA3(AP As AmplitudeParameters) As Double
		  Return AP.χax*(AP.Cβ^2/2-AP.Cβ^2*AP.C2/2)+AP.χaz*(-AP.Cβ*AP.Sβ/2+AP.Cβ*AP.C2*AP.Sβ/2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA4(AP As AmplitudeParameters) As Double
		  Return AP.χax*AP.Cβ*AP.Sβ*AP.S2-AP.χaz*AP.Sβ^2*AP.S2
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA5(AP As AmplitudeParameters) As Double
		  Return AP.χax*(AP.Cβ^2/2+AP.Cβ^2*AP.C2/2)+AP.χaz*(-AP.Cβ*AP.Sβ/2-AP.Cβ*AP.C2*AP.Sβ/2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA6(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χsy*(1/2+AP.C2/2))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA7(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χsy*AP.S1^2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA8(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χsx*(AP.Cβ^2/2-AP.Cβ^2*AP.C2/2)+AP.χsz*(-AP.Cβ*AP.Sβ/2+AP.Cβ*AP.C2*AP.Sβ/2))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetA9(AP As AmplitudeParameters) As Double
		  Return Parameters.δ*(AP.χsx*AP.Cβ*AP.Sβ*AP.S2-AP.χsz*AP.Sβ^2*AP.S2)
		  
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
