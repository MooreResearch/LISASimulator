#tag Class
Protected Class UncertaintyCalculatorClass
	#tag Method, Flags = &h0
		Function Calculate(ATAMatrix As Matrix, Θ As Double) As UncertaintyValuesClass
		  ATA = ATAMatrix  // Get a local reference to the matrix
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
		  Var uv As New UncertaintyValuesClass // Get a new instance of the uncertainty values class
		  // Note that the order here is assumed to be that specified by the enumeration "Item"
		  uv.OfM1 = uncList(0)*Sn2F0*Parameters.M1
		  uv.OfM2 = uncList(1)*Sn2F0*Parameters.M2
		  uv.OfF0 = 2*uncList(2)*Sn2F0*Parameters.F0*1000
		  uv.OfR = uncList(3)*Sn2F0*Parameters.R0*Parameters.Λ
		  uv.Ofβ = uncList(4)*Sn2F0*degFromRad
		  uv.Ofψ = uncList(5)*Sn2F0*degFromRad
		  uv.Ofλ0 = uncList(6)*Sn2F0*degFromRad
		  uv.OfΘ = uncList(7)*Sn2F0*degFromRad
		  uv.OfΦ = uncList(8)*Sn2F0*degFromRad
		  uv.Ofχ10x = uncList(9)*Sn2F0*degFromRad
		  uv.Ofχ10y = uncList(10)*Sn2F0*degFromRad
		  uv.Ofχ10z = uncList(11)*Sn2F0*degFromRad
		  uv.Ofχ20x = uncList(12)*Sn2F0*degFromRad
		  uv.Ofχ20y = uncList(13)*Sn2F0*degFromRad
		  uv.Ofχ20z = uncList(14)*Sn2F0*degFromRad
		  uv.OfΩ = Sin(Θ)*uv.OfΘ*uv.OfΦ/(12.566370614359172*degFromRad*degFromRad)
		  Return uv
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(MyParameters As CaseParametersClass)
		  Parameters = MyParameters
		  InitSolveList
		  Var V0 As Double = Parameters.V0
		  Var f0 As Double =  V0*V0*V0/(2*Parameters.π*Parameters.GM)*Parameters.IVOnePlusZ
		  Var Noise As New NoiseClass(Parameters.ΔT)
		  //  get the noise at various frequencies
		  // This is the noise at th initiale fundamental gravitational wave frequency
		  Sn2F0 = Sqrt(Noise.GetNoise(2*f0))
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub FindBestInvertible()
		  InvertY   // Try a first inversion
		  Var LowestCondition As Double = Condition
		  Var RowToOmit As Integer = -1
		  If Condition > 1.0e12 Then
		    For i As Integer = 0 to 14
		      If SolveList(i) Then
		        SolveList(i) = False   // Try not solving for this row
		        InvertY
		        If Condition < LowestCondition Then
		          LowestCondition = Condition
		          RowToOmit = i
		        End If
		        SolveList(i) = True   // Restore SolveList
		      End If
		    Next
		    If RowToOmit > -1 Then
		      SolveList(RowToOmit) = False
		      InvertY
		    End If
		  End If
		  If Condition > 1.0e12 Then
		    If RowToOmit > -1 Then SolveList(RowToOmit) = True
		    LowestCondition = Condition
		    Var RowsToOmit(1) As Integer
		    RowsToOmit(0) = -1
		    For i As Integer = 0 to 14
		      If SolveList(i) Then
		        SolveList(i) = False
		        For j As Integer = 0 to 14
		          If j <> i And SolveList(j) Then
		            SolveList(j) = False
		            InvertY
		            If Condition < LowestCondition Then
		              LowestCondition = Condition
		              RowsToOmit(0) = i
		              RowsToOmit(1) = j
		            End If
		            SolveList(j) = True
		          End If
		        Next
		        SolveList(i) = True
		      End If
		    Next
		    If RowsToOmit(0) <> -1 Then
		      SolveList(RowsToOmit(0)) = False
		      SolveList(RowsToOmit(1)) = False
		      InvertY
		    End If
		  End If
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
		  Y0 = New Matrix(M)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitSolveList()
		  // This creates an array of items to solve for. This is the canonical order
		  // of items, by the way. This must be consistent with the order in the
		  // enumeration "Item."
		  SolveList(0) = Parameters.SolveForM1
		  SolveList(1) = Parameters.SolveForM2
		  SolveList(2) = Parameters.SolveForF0
		  SolveList(3) = Parameters.SolveForΛ
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
		            Exit // No need to go on
		          Else  // Otherwise
		            k = k + 1  // update the row number for a good row
		          End If
		        End If
		      Next
		    End If
		    // Note that if we ever get to a matrix with zero size, a runtime exception will happen
		  Loop Until badRow = -1
		  // When we get here, the Y matrix should be inverted. Do a check:
		  YInvXY = Y*Y0
		  Condition = Y.EuclideanNorm*Y0.EuclideanNorm
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		ATA As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		Condition As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Sn2F0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveList(14) As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		Y As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		Y0 As Matrix
	#tag EndProperty

	#tag Property, Flags = &h0
		YInvXY As Matrix
	#tag EndProperty


	#tag Enum, Name = Item, Type = Integer, Flags = &h0
		h0
		  δ
		  F0
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
		#tag ViewProperty
			Name="Sn2F0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
	#tag EndViewBehavior
End Class
#tag EndClass
