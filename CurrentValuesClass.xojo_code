#tag Class
Protected Class CurrentValuesClass
	#tag Method, Flags = &h0
		Function Operator_Add(RHS As CurrentValuesClass) As CurrentValuesClass
		  Var Result As New CurrentValuesClass  // Create an empty version of the class
		  Var info As Introspection.TypeInfo = Introspection.GetType(self) // Get information about this class
		  Var childProperties() As Introspection.PropertyInfo = info.GetProperties
		  For Each p As Introspection.PropertyInfo In childProperties // iterate through the array
		    // add each value in this instance to the value in the right instance to get the result value
		    p.Value(Result) = p.Value(self).DoubleValue + p.Value(RHS).DoubleValue
		  Next
		  Return Result // Return the result
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_MultiplyRight(LHS As Double) As CurrentValuesClass
		  Var Result As New CurrentValuesClass  // Create a new empty version of the class
		  Var info As Introspection.TypeInfo = Introspection.GetType(self)  // Get the information about this class
		  Var childProperties() As Introspection.PropertyInfo = info.GetProperties  // Get an array of information about the properties
		  For Each p As Introspection.PropertyInfo In childProperties // Iterate over all the properties
		    p.Value(Result) = LHS*p.Value(self).DoubleValue // multiply each value in self by the scalar and put it in the result
		  Next
		  Return Result // Return the result
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		Cosι As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		α As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		τr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χax As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χay As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaz As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsx As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsy As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsz As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ψr As Double
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
			Name="V"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="α"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="cosι"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Ψr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χax"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χay"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaz"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsx"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsy"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsz"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
