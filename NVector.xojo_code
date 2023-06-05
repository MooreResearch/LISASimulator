#tag Class
Protected Class NVector
	#tag Method, Flags = &h0
		Sub Constructor(v() As Double)
		  // Constructor method for the NVector class.
		  // Parameters: v() is a 1D array containing the entries for the NVector.
		  
		  pData = v                            // stores the array v (the vector entries) as a property
		  
		  Var n As Integer = v.lastIndex + 1   // length of the vector is 1 + the largest index of v (since Xojo is 0-indexed)
		  pLength = n                          // stores length of vector as property
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ToString() As String
		  // Creates and returns a representation of the vector as a string.
		  
		  Var n As Integer = pLength - 1   // n is the largest (0-based) index of vector entries
		  
		  Var vecRep As String                                     // initialize string representation of vector
		  For i As Integer = 0 To n - 1                              // loop over all matrix vector
		    vecRep = vecRep + pData(i).ToString + "; "     // vector elements separated by semicolons
		  Next
		  vecRep = vecRep + pData(n).ToString + EndOfLine  // last element in row has no semicolon; include line break at end (for simpler code in print statements)
		  
		  return vecRep
		End Function
	#tag EndMethod


	#tag Note, Name = Description
		The NVector class contains methods for creating and manipulating real-valued vectors in Rn. 
		Class properties: 
		 - pData is a 1D array which stores the entries of the vector.
		 - pLength is an integer which stores the length of the vector (number of entries). 
		
		In the future, we will add subclasses that contain specific methods for vectors in R3 (e.g., cross
		products, conversion between Cartesian and spherical coordinates, etc.). 
		
		To add?: Dot product, addition/subtraction/scalar multiplication methods; R3 subclass
		
	#tag EndNote


	#tag Property, Flags = &h0
		pData() As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		pLength As Integer
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
			Name="pLength"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
