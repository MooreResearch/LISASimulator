#tag Class
Class NecdetsClass
	#tag Method, Flags = &h0
		Sub Constructor()
		  // Initialize arrays if required
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetValues(Wavebuilders() As WaveBuilderClass, ep As Double)
		  If Wavebuilders.Ubound >= 1 Then
		    Var inv2ep As Double = 1/(2.0*ep)
		    For i As Integer = 0 To 250  // Assuming 250 elements
		      nA(1,i) = Wavebuilders(1).A(i)
		      nA(2,i) = Wavebuilders(2).A(i)
		      nW(1,i) = Wavebuilders(1).W(i)
		      nW(2,i) = Wavebuilders(2).W(i)
		    Next
		    nDVI(Dβ) = (Wavebuilders(1).SpinResults.V - Wavebuilders(2).SpinResults.V)*inv2ep
		    nDιI(Dβ) = (Wavebuilders(1).SpinResults.ι - Wavebuilders(2).SpinResults.ι)*inv2ep
		    nDαI(Dβ) = (Wavebuilders(1).SpinResults.α - Wavebuilders(2).SpinResults.α)*inv2ep
		    nDαI(Dβ) = (Wavebuilders(1).SpinResults.α - Wavebuilders(2).SpinResults.α)*inv2ep
		    nDχaxI(Dβ) = (Wavebuilders(1).SpinResults.χax - Wavebuilders(2).SpinResults.χax)*inv2ep
		    nDχayI(Dβ) = (Wavebuilders(1).SpinResults.χay - Wavebuilders(2).SpinResults.χay)*inv2ep
		    nDχazI(Dβ) = (Wavebuilders(1).SpinResults.χaz - Wavebuilders(2).SpinResults.χaz)*inv2ep
		    nDχsxI(Dβ) = (Wavebuilders(1).SpinResults.χsx - Wavebuilders(2).SpinResults.χsx)*inv2ep
		    nDχsyI(Dβ) = (Wavebuilders(1).SpinResults.χsy - Wavebuilders(2).SpinResults.χsy)*inv2ep
		    nDχszI(Dβ) = (Wavebuilders(1).SpinResults.χsz - Wavebuilders(2).SpinResults.χsz)*inv2ep
		    nDΨI(Dβ) = (Wavebuilders(1).SpinResults.Ψ - Wavebuilders(2).SpinResults.Ψ)*inv2ep
		    nDHDq(Dβ) = (WaveBuilders(1).H - WaveBuilders(2).H)*inv2ep
		    nV(1) = WaveBuilders(1).SpinResults.V
		    nV(2) = WaveBuilders(2).SpinResults.V
		    nι(1) = WaveBuilders(1).SpinResults.ι
		    nι(2) = WaveBuilders(2).SpinResults.ι
		    nα(1) = WaveBuilders(1).SpinResults.α
		    nα(2) = WaveBuilders(2).SpinResults.α
		  Else
		    Raise New RuntimeException("Not enough Wavebuilders instances.")
		  End If
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		nA(2,250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDHDq(14) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDVI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDιI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDαI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχaxI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχayI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχazI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχsxI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχsyI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDχszI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nDΨI(7) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nV(2) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nW(2,250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nι(2) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nα(2) As Double
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
