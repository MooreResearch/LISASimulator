#tag Class
Protected Class UncertaintyCalculatorClass
	#tag Method, Flags = &h0
		Function Calculate(ATAMatrix As Matrix) As UncertaintyValuesClass
		  ATA = ATAMatrix  // Get a local reference to the matrix
		  GetYToSolve  // Get the submatrix we are actually going to solve for
		  InvertY // Invert the matrix
		  // Now we will complile uncertainty values.
		  Var k As Integer = 0  // Index to the actual row in the inverted matrix
		  Var s As String = "NAN"  
		  Var nan As Double = s.ToDouble  // This "not a number" will indicate an uncertainty not calculated
		  Var uncList(14) As Double  // create an uncertainty list
		  For j As Integer = 0 to 14
		    If SolveList(j) Then  // if we solved for this, get its value
		      uncList(j) = Sqrt(Y.pData(k,k))
		      k = k + 1   // and update the row number in the actual matrix
		    Else
		      UncList(j) = nan  // otherwise, the uncertainty is "not a number"
		    End If
		  Next
		  Var uv As New UncertaintyValuesClass // Get a new instance of the uncertainty values class
		  // Note that the order here is assumed to be that specified by the enumeration "Item"
		  uv.OfH0 = uncList(0)
		  uv.Ofδ = uncList(1)
		  uv.OfV0 = uncList(2)
		  uv.OfZ = uncList(3)
		  uv.Ofβ = uncList(4)
		  uv.Ofψ = uncList(5)
		  uv.Ofλ0 = uncList(6)
		  uv.OfΘ = uncList(7)
		  uv.OfΦ = uncList(8)
		  uv.Ofχ10x = uncList(9)
		  uv.Ofχ10y = uncList(10)
		  uv.Ofχ10z = uncList(11)
		  uv.Ofχ20x = uncList(12)
		  uv.Ofχ20y = uncList(13)
		  uv.Ofχ20z = uncList(14)
		  Return uv
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  Parameters = MyParameters
		  InitSolveList
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetYToSolve()
		  Var M(-1,-1) As Double
		  Var RowsToInclude As Integer = 0
		  For j As Integer = 0 to 14
		    If SolveList(j)Then RowsToInclude = RowsToInclude + 1
		  Next
		  If RowsToInclude = 0 Then Raise New RuntimeException("Nothing to Solve For")
		  Var n As Integer = RowsToInclude - 1
		  M.ResizeTo(n,n)
		  Var jj As Integer = 0
		  Var kk As Integer = 0
		  For j As Integer = 0 to 14
		    If SolveList(j) Then
		      kk = 0
		      For k As Integer = 0 to 14
		        If SolveList(k) Then
		          M(jj,kk) = ATA.pData(j,k)
		          kk = kk + 1
		        End If
		      Next
		      jj = jj + 1
		    End If
		  Next
		  Y = New Matrix(M)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitSolveList()
		  // This creates an array of items to solve for. This is the canonical order
		  // of items, by the way. This must be consistent with the order in the
		  // enumeration "Item."
		  SolveList(0) = Parameters.SolveForH0
		  SolveList(1) = Parameters.SolveForδ
		  SolveList(2) = Parameters.SolveForV0
		  SolveList(3) = Parameters.SolveForZ
		  SolveList(4) = Parameters.SolveForβ
		  SolveList(5) = Parameters.SolveForψ
		  SolveList(6) = Parameters.SolveForλ0
		  SolveList(7) = Parameters.SolveForΘ
		  SolveList(8) = Parameters.SolveForΦ
		  SolveList(9) = Parameters.SolveForχ10x
		  SolveList(10) = Parameters.SolveForχ10y
		  SolveList(11) = Parameters.SolveForχ10z
		  SolveList(12) = Parameters.SolveForχ20x
		  SolveList(13) = Parameters.SolveForχ20y
		  SolveList(14) = Parameters.SolveForχ20z
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InvertY()
		  // This method inverts the Y matrix or whatever submatrix we can invert
		  Var badRow As Integer
		  Do // We will keep trying to invert smaller and smaller matrices until we find one that we can
		    GetYToSolve  // Get the submatrix to actually solve for
		    badRow = Y.LUInvert(Y.PDim) - 1  // Get the row index for the bad row if any
		    Var k As Integer = 0  // This will be the row index in the actual matrix
		    If badRow <> -1 Then  // If we have a bad row
		      For j As Integer = 0 to 14   // Scan through the solve list
		        If SolveList(j) Then  // if we are solving for this item
		          If k = badRow Then  // and the k index is the same as the bad row
		            SolveList(j) = False  // then we are not going to solve for that item
		          Else  // Otherwise
		            k = k + 1  // update the row number for a good row
		          End If
		        End If
		      Next
		    End If
		    // Note that if we ever get to a matrix with zero size, a runtime exception will happen
		  Loop Until badRow = -1
		  // When we get here, the Y matrix should be inverted
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		ATA As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveList(14) As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		Y As Matrix
	#tag EndProperty


	#tag Enum, Name = Item, Type = Integer, Flags = &h0
		h0
		  δ
		  v0
		  z
		  β
		  ψ
		  λ0
		  Θ
		  Φ
		  χ10x
		  χ10y
		  χ10z
		  χ20x
		  χ20y
		χ20z
	#tag EndEnum


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
