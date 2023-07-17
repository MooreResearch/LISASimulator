#tag Class
Protected Class HP1CalculatorClass
Inherits HNCalculator
	#tag Event
		Sub GetTerms()
		  AddTerm(AddressOf GetA1, 3, 3, False)
		  AddTerm(AddressOf GetA2, 1, 1, False)
		  AddTerm(AddressOf GetA3, 1, -1, False)
		  AddTerm(AddressOf GetA4, 3,1, False)
		  AddTerm(AddressOf GetA5, 1, 3, False)
		  AddTerm(AddressOf GetA6, 1, -3, False)
		  AddTerm(AddressOf GetA7, 3, -1, False)
		  AddTerm(AddressOf GetA8, 3, -3, False)
		  AddTerm(AddressOf GetA9, 0, 3, False)
		  AddTerm(AddressOf GetA10,2, 1, False)
		  AddTerm(AddressOf GetA11, 2, 3, False)
		  AddTerm(AddressOf GetA12, 2, -1, False)
		  AddTerm(AddressOf GetA13, 2, -3, False)
		  AddTerm(AddressOf GetA14, 0, 1, False)
		  
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  
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
