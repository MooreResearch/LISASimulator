#tag Class
Protected Class CurrentDerivativesClass
	#tag Method, Flags = &h0
		Function Operator_Add(RHS As CurrentDerivativesClass) As CurrentDerivativesClass
		  Var Result As New CurrentDerivativesClass  // Create an empty version of the class
		  Var info As Introspection.TypeInfo = Introspection.GetType(self) // Get information about this class
		  Var childProperties() As Introspection.PropertyInfo = info.GetProperties  // Get an array of the class properties
		  For Each p As Introspection.PropertyInfo In childProperties // iterate through the array
		    // add each value in this instance to the value in the right instance to get the result value
		    p.Value(Result) = p.Value(self).DoubleValue + p.Value(RHS).DoubleValue
		  Next
		  Return Result // Return the result
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_MultiplyRight(LHS As Double) As CurrentDerivativesClass
		  Var Result As New CurrentDerivativesClass  // Create a new empty version of this class
		  Var info As Introspection.TypeInfo = Introspection.GetType(self) // Get information about this class
		  Var childProperties() As Introspection.PropertyInfo = info.GetProperties // Get an array of information about the properties
		  For Each p As Introspection.PropertyInfo In childProperties // Iterate over all the properties
		    p.Value(Result) = LHS*p.Value(self).DoubleValue // multiply each value in self by the scalar and put it in the result
		  Next
		  Return Result // Return the result
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		DCosιDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCosιDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCosιDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCosιDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCosιDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCosιDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCosιDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCosιDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCosιDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχaxDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχayDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχazDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsxDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχsyDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DχszDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΦ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20z As Double
	#tag EndProperty


	#tag ViewBehavior
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
		#tag ViewProperty
			Name="DΨrDΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΦ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCosιDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCosιDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCosιDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCosιDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCosιDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCosιDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCosιDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCosιDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DCosιDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχaxDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχayDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχazDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsxDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχsyDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DχszDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
