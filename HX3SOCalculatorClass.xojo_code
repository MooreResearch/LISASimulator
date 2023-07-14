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
