#tag Class
Protected Class IdentityMatrix
Inherits Matrix
	#tag Method, Flags = &h0
		Sub Constructor(n As Integer)
		  // Constructor for the IdentityMatrix class. Creates an identity matrix of the specified dimension.
		  // Parameters: n is the dimension of the identity matrix. (e.g.: Choosing n = 2 creates the 2 x 2 identity matrix.)
		  
		  pDim = n                  // stores dimension of matrix as property
		  pData.ResizeTo(n-1, n-1)  // resizes matrix data array appropriately (n-1 used because of 0-indexing)
		  
		  For i As Integer = 0 To n-1      // loop over matrix entries
		    For j As Integer = 0 To n-1
		      If i = j Then                    // if entry is on the diagonal...
		        pData(i, j) = 1                // set entry = 1
		      Else                             // otherwise...
		        pData(i, j) = 0                // set entry = 0
		      End If
		    Next
		  Next
		  
		End Sub
	#tag EndMethod


	#tag Note, Name = Description
		The IdentityMatrix class is a subclass of Matrix that allows for the easy creation of identity
		matrices (square matrices whose diagonal elements are all 1 and off-diagonal elements are all 0).
		Its Constructor method differs from the normal Matrix constructor method by taking only the
		dimension of the matrix, n, as a parameter (since this completely specifies the matrix) rather than
		requiring the full array of entries. 
		
		Currently, the IdentityMatrix class is only used in the NumInvert method code, though I anticipate
		it may be useful in future methods involving matrix calculations.
	#tag EndNote


	#tag ViewBehavior
		#tag ViewProperty
			Name="pDim"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
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
