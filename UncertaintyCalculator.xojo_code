#tag Class
Protected Class UncertaintyCalculator
	#tag Method, Flags = &h0
		Sub Arrange()
		  // This method removes the rows/columns of the parameters we don't want to solve for and fills in the matrix elements below the diagonal.
		  
		  // First, remove any row/column that we aren't solving for
		  Var j As Integer
		  Var numRem As Integer = 0  // store the number of rows/columns we've already removed
		  For i As Integer = 1 to 15
		    If Not solveList(i-1) Then  // if the parameter isn't to be solved for then:
		      j = i - numRem  // calculate the appropriate index
		      Y.RemoveInds(j,j)  // remove it
		      numRem = numRem + 1  // and update the number of rows removed
		    End If
		  Next
		  
		  
		  // Then, fill in the rest of the elements in the lower left (below diagonal)
		  For k As Integer = 0 To (14 - numRem)
		    For m As Integer = 0 to (14 - numRem)
		      Y.pData(m, k) = Y.pData(k, m)  // the matrix is symmetric
		    Next
		  Next
		  
		  MainWindow.arrangedMatrix = Y.ToString  // store the string of the matrix to be displayed for the user
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CalculateUncertainties()
		  // This method calculates each uncertainty of the parameters we're solving for.
		  
		  For i As Integer = 0 to 14
		    If solveList(i) Then  // if we are solving for the parameter
		      σ(i) = sqrt(Y.pdata(i,i)*Y.diagfactors(i))  // calculate the uncertainty and store it in an array
		    Else
		      σ(i) = 9.999e-99  // this is the placeholder for an uncertainty we aren't calculating
		    End If
		  Next
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(matrix(, ) As Double, mySolveList() As Boolean)
		  // The Uncertainty Calculator class performs the necessary operations to calculate parameter uncertainties. The appropriate order is
		  // to DiagNormalize, then Arrange, then InvertY, then Unarrange, and finally CalculateUncertainties. This class takes a 2D list (matrix) 
		  // and makes a Matrix out of it, as well as a solveList for determining which parameters to solve for. 
		  
		  Y = New Matrix(matrix)  // create a new Matrix out of the 2D list
		  
		  MainWindow.originalMatrix = Y.ToString  // store the string value of this matrix for UI display
		  
		  solveList = mysolveList  // store the solveList
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DiagNormalize()
		  // This method normalizes the matrix, making each diagonal element equal to one
		  
		  Y.DiagNormz(Y.pDim)  // normalize the matrix
		  
		  MainWindow.normalizedMatrix= Y.ToString // store this matrix as a string for UI display
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function InvertY() As Integer
		  // This method inverts the matrix. It returns 0 if the inversion is successful and the row of the failed part of the matrix inversion if unsuccessful.
		  
		  Var invertCheck As Integer = Y.LUInvert(Y.pDim)  // attempt to invert the matrix
		  
		  // if the matrix inversion worked, store the inverted matrix as a string for UI display
		  If invertCheck = 0 Then
		    MainWindow.invertedMatrix = Y.ToString
		  End If
		    
		  Return invertCheck
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Unarrange()
		  // This method puts the matrix back in 15x15 form, with placeholders in the rows/columns that we removed. 
		  
		  Var i, j, k, m As Integer
		  Var YY(14,14) As Double  // this is the "temporary" new matrix
		  
		  m = 0
		  For i = 0 to 14
		    If solveList(i) Then
		      m = m + 1
		    End If
		    
		    k = 0
		    For j = 0 to 14
		      If solveList(j) Then
		        k = k + 1
		      End If
		      If solveList(i) And solveList(j) Then
		        YY(i,j) = Y.pData(m-1,k-1)  // if we solved for this entry, place the appropriate matrix element here
		      Else
		        YY(i,j) = 9.99e-99  // if we didn't solve this entry, put in a placeholder value to signify such
		      End If
		    Next
		  Next
		  
		  Y.ResizeTo(14,14)  // make sure the Y matrix is the appropriate size for unarrangment
		  
		  // copy the temporary new matrix back into the Y matrix
		  For i = 0 to 14
		    For j = 0 to 14
		      Y.pData(i,j) = YY(i,j)
		    Next
		  Next
		  
		  MainWindow.unarrangedMatrix = Y.ToString  // store the matrix as a string for UI display
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		solveList() As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		Y As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		σ(14) As Double
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
	#tag EndViewBehavior
End Class
#tag EndClass
