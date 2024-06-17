#tag Class
Protected Class UncertaintyCalculatorClass
	#tag Method, Flags = &h0
		Sub Calculate(ATAMatrix As Matrix)
		  ATA = ATAMatrix  // Get a local reference to the matrix
		  Var SolveList() As Boolean = Parameters.SolveFor() // get a local reference to the solve list
		  FindBestInvertible // Find the version of the matrix that is invertible
		  // Now we will complile uncertainty values.
		  Var k As Integer = 0  // Index to the actual row in the inverted matrix
		  Var s As String = "Inf"  
		  Var inf As Double = s.ToDouble  // This "infinity" will indicate an uncertainty not calculated
		  s = "NAN"
		  Var nan As Double = s.ToDouble // This "nan" will indicate an uncertainy that is imaginary
		  Var uncList(14) As Double  // create an uncertainty list
		  For j As Integer = 0 to 14
		    If SolveList(j) Then  // if we solved for this, get its value
		      Var diagonalElement As Double = Y.pData(k,k)
		      If diagonalElement < 0 Then
		        uncList(j) = nan
		      Else
		        uncList(j) = Sqrt(diagonalElement)
		      End If
		      k = k + 1   // and update the row number in the actual matrix
		    Else
		      uncList(j) = inf  // otherwise, the uncertainty is "infinity"
		    End If
		  Next
		  Var degFromRad As Double = 180.0/Parameters.π
		  Var uncT1 As Double
		  Var uncT2 As Double
		  Var uv() As Double = Parameters.Uncertainties()
		  uv(Integer(CaseInfoClass.Param.M)) = uncList(Integer(CaseInfoClass.Param.M))*Parameters.M
		  uv(Integer(CaseInfoClass.Param.delta)) = uncList(Integer(CaseInfoClass.Param.delta))
		  uncT1 = Parameters.T0*uncList(Integer(CaseInfoClass.Param.M))  // portion of uncertainty of T0 from uncertainty in mass
		  uncT2 = -3.0*Parameters.T0*Parameters.V0*Parameters.V0*uncList(Integer(CaseInfoClass.Param.V0)) // portion from uncertainty in lnV0
		  uv(Integer(CaseInfoClass.Param.V0)) = Sqrt(uncT1*uncT1 + uncT2*uncT2) // This gives the total uncertainty
		  uv(Integer(CaseInfoClass.Param.R)) = uncList(Integer(CaseInfoClass.Param.R))*Parameters.R/Parameters.Year
		  uv(Integer(CaseInfoClass.Param.beta)) = uncList(Integer(CaseInfoClass.Param.beta))*degFromRad
		  uv(Integer(CaseInfoClass.Param.psi)) = uncList(Integer(CaseInfoClass.Param.psi))*degFromRad
		  uv(Integer(CaseInfoClass.Param.lambda0)) = uncList(Integer(CaseInfoClass.Param.lambda0))*degFromRad
		  uv(Integer(CaseInfoClass.Param.theta)) = uncList(Integer(CaseInfoClass.Param.theta))*degFromRad
		  uv(Integer(CaseInfoClass.Param.phi)) = uncList(Integer(CaseInfoClass.Param.phi))*degFromRad
		  uv(Integer(CaseInfoClass.Param.chi10x)) = uncList(Integer(CaseInfoClass.Param.chi10x))
		  uv(Integer(CaseInfoClass.Param.chi10y)) = uncList(Integer(CaseInfoClass.Param.chi10y))
		  uv(Integer(CaseInfoClass.Param.chi10z)) = uncList(Integer(CaseInfoClass.Param.chi10z))
		  uv(Integer(CaseInfoClass.Param.chi20x)) = uncList(Integer(CaseInfoClass.Param.chi20x))
		  uv(Integer(CaseInfoClass.Param.chi20y)) = uncList(Integer(CaseInfoClass.Param.chi20y))
		  uv(Integer(CaseInfoClass.Param.chi20z)) = uncList(Integer(CaseInfoClass.Param.chi20z))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseInfoClass)
		  Parameters = MyParameters
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub FindBestInvertible()
		  InvertY   // Try a first inversion
		  'Var LowestCondition As Double = Condition
		  'Var RowToOmit As Integer = -1
		  'If Condition > 1.0e12 Then
		  'For i As Integer = 0 to 14
		  'If SolveList(i) Then
		  'SolveList(i) = False   // Try not solving for this row
		  'InvertY
		  'If Condition < LowestCondition Then
		  'LowestCondition = Condition
		  'RowToOmit = i
		  'End If
		  'SolveList(i) = True   // Restore SolveList
		  'End If
		  'Next
		  'If RowToOmit > -1 Then
		  'SolveList(RowToOmit) = False
		  'InvertY
		  'End If
		  'End If
		  'If Condition > 1.0e12 Then
		  'If RowToOmit > -1 Then SolveList(RowToOmit) = True
		  'LowestCondition = Condition
		  'Var RowsToOmit(1) As Integer
		  'RowsToOmit(0) = -1
		  'For i As Integer = 0 to 14
		  'If SolveList(i) Then
		  'SolveList(i) = False
		  'For j As Integer = 0 to 14
		  'If j <> i And SolveList(j) Then
		  'SolveList(j) = False
		  'InvertY
		  'If Condition < LowestCondition Then
		  'LowestCondition = Condition
		  'RowsToOmit(0) = i
		  'RowsToOmit(1) = j
		  'End If
		  'SolveList(j) = True
		  'End If
		  'Next
		  'SolveList(i) = True
		  'End If
		  'Next
		  'If RowsToOmit(0) <> -1 Then
		  'SolveList(RowsToOmit(0)) = False
		  'SolveList(RowsToOmit(1)) = False
		  'InvertY
		  'End If
		  'End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetYToSolve()
		  Var M(-1,-1) As Double
		  Var Q(-1,-1) As Double
		  Var SolveList() As Boolean = Parameters.SolveFor()
		  
		  Var RowsToInclude As Integer = 0
		  For j As Integer = 0 to 14
		    If SolveList(j)Then RowsToInclude = RowsToInclude + 1
		  Next
		  If RowsToInclude = 0 Then Raise New RuntimeException("Nothing to Solve For")
		  Var n As Integer = RowsToInclude - 1
		  M.ResizeTo(n,n)
		  Q.ResizeTo(n,n)
		  
		  
		  ReDim DiagEntries(-1)
		  for s As integer = 0 to 14
		    DiagEntries.Add(ATA.pData(s,s))
		  next
		  
		  Var jj As Integer = 0
		  Var kk As Integer = 0
		  For j As Integer = 0 to 14
		    If SolveList(j) Then
		      kk = 0
		      For k As Integer = 0 to 14
		        If SolveList(k) Then
		          M(jj,kk) = ATA.pData(j,k) / (sqrt(DiagEntries(j)) *sqrt(DiagEntries(k)) )
		          Q(jj,kk) = ATA.pData(j,k) / (sqrt(DiagEntries(j)) *sqrt(DiagEntries(k)) )
		          kk = kk + 1
		        End If
		      Next
		      jj = jj + 1
		    End If
		  Next
		  
		  Y0Normalized = New Matrix(Q)
		  YNormalized = New Matrix(Q)
		  
		  If InvertNum = 0 then
		    Y0NormUnchanged = New Matrix(Q)
		  end if 
		  
		  
		  Y = New Matrix(M)
		  Y0 = New Matrix(M)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InvertY()
		  // This method inverts the Y matrix or whatever submatrix we can invert
		  Var badRow As Integer
		  Var SolveList() As Boolean = Parameters.SolveFor()
		  
		  InvertNum = 0
		  
		  Do // We will keep trying to invert smaller and smaller matrices until we find one that we can
		    GetYToSolve  // Get the submatrix to actually solve for
		    badRow = Y.LUInvert(Y.PDim) - 1  // Get the row index for the bad row if any
		    InvertNum = InvertNum + 1
		    Var k As Integer = 0  // This will be the row index in the actual matrix
		    If badRow <> -1 Then  // If we have a bad row
		      Var removedRow As integer = TrySubMatrices
		      
		      if removedRow = -1 then 
		        break
		      end if
		      
		      'For j As Integer = 0 to 14   // Scan through the solve list
		      'If SolveList(j) Then  // if we are solving for this item
		      'If k = badRow Then  // and the k index is the same as the bad row
		      'SolveList(j) = False  // then we are not going to solve for that item
		      'Exit // No need to go on
		      'Else  // Otherwise
		      'k = k + 1  // update the row number for a good row
		      'End If
		      'End If
		      'Next
		      
		      
		    End If
		    // Note that if we ever get to a matrix with zero size, a runtime exception will happen
		  Loop Until badRow = -1
		  
		  
		  // When we get here, the Y matrix should be inverted. Do a check:
		  YInvXY = Y*Y0
		  Condition = Y.EuclideanNorm*Y0.EuclideanNorm 
		  
		  
		  
		  
		  Var jj As Integer = 0
		  Var kk As Integer = 0
		  
		  For j As Integer = 0 to 14
		    If SolveList(j) Then
		      kk = 0
		      For k As Integer = 0 to 14
		        If SolveList(k) Then
		          YNormalized.pData(jj,kk) = Y.pdata(jj,kk)
		          Y.pData(jj,kk) = Y.pdata(jj,kk) / ( sqrt(DiagEntries(j)) *sqrt(DiagEntries(k)) )
		          Y0.pData(jj,kk) = Y0.pData(jj,kk) * sqrt(DiagEntries(j)) *sqrt(DiagEntries(k)) 
		          kk = kk + 1
		        End If
		      Next
		      jj = jj + 1
		    End If
		  Next
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function TrySubMatrices() As Integer
		  Var SolveList() As Boolean = Parameters.SolveFor()
		  Var i,j,n, badRow, Original as integer
		  
		  n = Y0NormUnchanged.pDim -1 
		  
		  Var WorstCondition As Double = 0 
		  
		  Var WorstRow As Integer = -1
		  
		  Var BadRows() As Integer 
		  
		  for i = 1 to n + 1
		    Var TempMatrix As New Matrix(Y0NormUnchanged.pData)
		    TempMatrix.RemoveRow(i)
		    badRow = TempMatrix.LUInvert(TempMatrix.pDim) - 1
		    if badRow = -1 then
		      BadRows.Add(i)
		      if TempMatrix.EuclideanNorm > WorstCondition then
		        WorstCondition = TempMatrix.EuclideanNorm
		        WorstRow = i
		      end if
		    end if 
		  next 
		  
		  SolveList(WorstRow - 1) = False
		  
		  return WorstRow
		  
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		ATA As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		Condition As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DiagEntries() As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		InvertNum As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseInfoClass
	#tag EndProperty

	#tag Property, Flags = &h0
		SubMatrix As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		Y As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		Y0 As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		Y0Normalized As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		Y0NormUnchanged As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		YInvXY As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		YNormalized As Matrix
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
			Name="Condition"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="InvertNum"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
