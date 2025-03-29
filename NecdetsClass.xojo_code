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
		    inv2ep = 1/(2.0*ep)
		    h0diff = 0.5 * WaveBuilders(0).Parameters.M * (1 - WaveBuilders(0).Parameters.δ * WaveBuilders(0).Parameters.δ) / WaveBuilders(0).Parameters.R
		    
		    
		    //A1 =  WaveBuilders(1).A
		    //A2 =  WaveBuilders(2).A
		    //W1 =  WaveBuilders(1).W
		    //W2 =  WaveBuilders(2).W
		    
		    Var hp1, hp2, hx1, hx2, dhp, dhx As Double
		    Static cross As Boolean = True
		    Static plus As Boolean = False
		    
		    // Get plus polarization product rule terms
		    hp1 = Wavebuilders(0).GetHSum(WaveBuilders(1).A, WaveBuilders(0).W, plus)
		    hp1 = (hp1 - Wavebuilders(0).GetHSum(WaveBuilders(2).A, WaveBuilders(0).W, plus))*inv2ep
		    hp2 = Wavebuilders(0).GetHSum(WaveBuilders(0).A, WaveBuilders(1).W, plus)
		    hp2 = (hp2 - Wavebuilders(0).GetHSum(WaveBuilders(0).A, WaveBuilders(2).W, plus))*inv2ep
		    dhp = hp1 + hp2
		    
		    // Get cross polarization product rule terms
		    hx1 = Wavebuilders(0).GetHSum(WaveBuilders(1).A, WaveBuilders(0).W, cross)
		    hx1 = (hx1 - Wavebuilders(0).GetHSum(WaveBuilders(2).A, WaveBuilders(0).W, cross))*inv2ep
		    hx2 = Wavebuilders(0).GetHSum(WaveBuilders(0).A, WaveBuilders(1).W, cross)
		    hx2 = (hx2 - Wavebuilders(0).GetHSum(WaveBuilders(0).A, WaveBuilders(2).W, plus))*inv2ep
		    dhx = hx1 + hx2
		    
		    // Perform the weighted sum of the polarizations
		    // Note that h0 also depends on delta, so we need to include its derivative
		    
		    //nDHDq(Dδ) = H0*(dhp*fp + dhx*fx) + dh0dδ*H/H0
		    nDHDq(Dδ) = WaveBuilders(0).H0*(dhp * WaveBuilders(0).FP + dhx * WaveBuilders(0).FX) + WaveBuilders(0).Dh0dδ * WaveBuilders(0).H / WaveBuilders(0).H0
		    
		    nDHDq(Dβ) = (WaveBuilders(1).H - WaveBuilders(2).H)*inv2ep
		    
		    nDVI(Dδ) = (Wavebuilders(1).SpinResults.V - Wavebuilders(2).SpinResults.V)*inv2ep
		    nDιI(Dδ) = (Wavebuilders(1).SpinResults.ι - Wavebuilders(2).SpinResults.ι)*inv2ep
		    nDαI(Dδ) = (Wavebuilders(1).SpinResults.α - Wavebuilders(2).SpinResults.α)*inv2ep
		    nDαI(Dδ) = (Wavebuilders(1).SpinResults.α - Wavebuilders(2).SpinResults.α)*inv2ep
		    nDχaxI(Dδ) = (Wavebuilders(1).SpinResults.χax - Wavebuilders(2).SpinResults.χax)*inv2ep
		    nDχayI(Dδ) = (Wavebuilders(1).SpinResults.χay - Wavebuilders(2).SpinResults.χay)*inv2ep
		    nDχazI(Dδ) = (Wavebuilders(1).SpinResults.χaz - Wavebuilders(2).SpinResults.χaz)*inv2ep
		    nDχsxI(Dδ) = (Wavebuilders(1).SpinResults.χsx - Wavebuilders(2).SpinResults.χsx)*inv2ep
		    nDχsyI(Dδ) = (Wavebuilders(1).SpinResults.χsy - Wavebuilders(2).SpinResults.χsy)*inv2ep
		    nDχszI(Dδ) = (Wavebuilders(1).SpinResults.χsz - Wavebuilders(2).SpinResults.χsz)*inv2ep
		    nDΨI(Dδ) = (Wavebuilders(1).SpinResults.Ψ - Wavebuilders(2).SpinResults.Ψ)*inv2ep
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
		A1(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		A2(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		h0diff As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		inv2ep As Double
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
		nι(2) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		nα(2) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		W1(250) As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		W2(250) As Double
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
			Name="inv2ep"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="h0diff"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
