#tag Class
Protected Class PhaseEvolverClass
	#tag Method, Flags = &h0
		Sub Constructor(TheParameters As CaseParametersClass, Dτ0 As Double)
		  Parameters = TheParameters   // Store a reference to the parameters
		  ΨrN = Parameters.λ0  // Set the initial phase
		  ΨrP = ΨrN   // The past phase is initially the same
		  Ψr0 = ΨrN   // The original value is also this
		  // All the DΨr/DWhatever derivatives are initially zero, so we don't need to set them
		  // Calculate some quantities so we don't have to calculate them or look them up later
		  InverseOnePlusZ = 1.0/(1.0 + Parameters.Z)
		  Dτr = Dτ0*(1.0 + Parameters.Z)
		  VeCosΘ = Cos(Parameters.Θ)*Parameters.Ve
		  VeSinΘ = Sin(Parameters.Θ)*Parameters.Ve
		  Φ = Parameters.Φ
		  V0 = Parameters.V0
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoStep(DτF As Double, DτP As Double, τr As Double)
		  // This method ASSUMES that one has done spin and v main and side cases and have set
		  // the properties V, VDot, Cosι, αDot, and all the derivatives dv/dq, daDot/dq, and dvDot/dq,
		  // where q = V0, δ, and the initial six spin variables.
		  // Calculate some useful stuff so that we don't have to do the calculations multiple times
		  Var DτRatio As Double = DτF/DτP
		  Var OneMinusRatio As Double = 1.0 - DτRatio
		  Var GMΩeτr As Double = Parameters.GMΩe*τr
		  Var SinOrbit As Double = Sin(GMΩeτr - Φ)
		  Var CosOrbit As Double = Cos(GMΩeτr - Φ)
		  Var LF As Double =2.0*(Log(V/V0) + 1.0)
		  Var LF2 As Double = V*V*LF
		  Var LF1 As Double = V*(2.0*LF + 1.0)*VDot
		  Var ΨrDot As Double = V - Cosι*αDot - LF2*VDot
		  Var StepFactor As Double = 2*DτF*(1.0 + VeSinΘ*SinOrbit)
		  
		  
		  // Calculate new past values using interpolation (note that this effectively does nothing if DτF/DτP = 1,
		  // but it is probably faster just to do the calculation
		  ΨrP = OneMinusRatio*ΨrN + DτRatio*ΨrP
		  DΨrDV0P = OneMinusRatio*DΨrDV0N + DτRatio*DΨrDV0P
		  DΨrDδP = OneMinusRatio*DΨrDδN + DτRatio*DΨrDδP
		  DΨrDΘP = OneMinusRatio*DΨrDΘN + DτRatio*DΨrDΘP
		  DΨrDΦP = OneMinusRatio*DΨrDΦN + DτRatio*DΨrDΦP
		  DΨrDχ10xP = OneMinusRatio*DΨrDχ10xN + DτRatio*DΨrDχ10xP
		  DΨrDχ10yP = OneMinusRatio*DΨrDχ10yN + DτRatio*DΨrDχ10yP
		  DΨrDχ10zP = OneMinusRatio*DΨrDχ10zN + DτRatio*DΨrDχ10zP
		  DΨrDχ20xP = OneMinusRatio*DΨrDχ20xN + DτRatio*DΨrDχ20xP
		  DΨrDχ20yP = OneMinusRatio*DΨrDχ20yN + DτRatio*DΨrDχ20yP
		  DΨrDχ20zP = OneMinusRatio*DΨrDχ20zN + DτRatio*DΨrDχ20zP
		  
		  // Now update the evolving phase value and its derivatives
		  ΨrF = ΨrP + StepFactor*ΨrDot
		  DΨrDZN = -InverseOnePlusZ*(ΨrN - Ψr0)
		  DΨrDΘF = DΨrDΘP + 2*DτF*VeCosΘ*SinOrbit*ΨrDot
		  DΨrDΦF = DΨrDΦP + 2*DτF*VeSinΘ*CosOrbit*ΨrDot
		  DΨrDV0F = DΨrDV0P + StepFactor*(DvDV0 - DCosιDV0*αDot - Cosι*DαDotDV0 - LF1*DvDV0 - LF2*DvDotDV0)
		  DΨrDδF = DΨrDδP + StepFactor*(DvDδ - DCosιDδ*αDot - Cosι*DαDotDδ - LF1*DvDδ - LF2*DvDotDδ)
		  DΨrDχ10xF = DΨrDχ10xP + StepFactor*(DvDχ10x - DCosιDχ10x*αDot - Cosι*DαDotDχ10x - LF1*DvDχ10x - LF2*DvDotDχ10x)
		  DΨrDχ10yF = DΨrDχ10yP + StepFactor*(DvDχ10y - DCosιDχ10y*αDot - Cosι*DαDotDχ10y - LF1*DvDχ10y - LF2*DvDotDχ10y)
		  DΨrDχ10zF = DΨrDχ10zP + StepFactor*(DvDχ10z - DCosιDχ10z*αDot - Cosι*DαDotDχ10z - LF1*DvDχ10z - LF2*DvDotDχ10z)
		  DΨrDχ20xF = DΨrDχ20xP + StepFactor*(DvDχ20x - DCosιDχ20x*αDot - Cosι*DαDotDχ20x - LF1*DvDχ20x - LF2*DvDotDχ20x)
		  DΨrDχ20yF = DΨrDχ20yP + StepFactor*(DvDχ20y - DCosιDχ20y*αDot - Cosι*DαDotDχ20y - LF1*DvDχ20y - LF2*DvDotDχ20y)
		  DΨrDχ20zF = DΨrDχ20zP + StepFactor*(DvDχ20z - DCosιDχ20z*αDot - Cosι*DαDotDχ20z - LF1*DvDχ20z - LF2*DvDotDχ20z)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub MakeFuturePresent()
		  ΨrN = ΨrF
		  DΨrDΘN = DΨrDΘF
		  DΨrDΦN = DΨrDΦF
		  DΨrDV0N = DΨrDV0F
		  DΨrDδN = DΨrDδF
		  DΨrDχ10xN = DΨrDχ10xF
		  DΨrDχ10yN = DΨrDχ10yF
		  DΨrDχ10zN = DΨrDχ10zF
		  DΨrDχ20xN = DΨrDχ20xF
		  DΨrDχ20yN = DΨrDχ20yF
		  DΨrDχ20zN = DΨrDχ20zF
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		Cosι As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DCosιDV0 As Double
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
		DvDotDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDotDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDotDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDotDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDotDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDotDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDotDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDotDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DvDV0 As Double
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
		DαDotDV0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDotDδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDotDχ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDotDχ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDotDχ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDotDχ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαdotDχ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DαDotDχ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Dτr As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDV0F As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDV0N As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDV0P As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDZN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDδF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDδN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDδP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΘF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΘN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΘP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΦF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΦN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDΦP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10xF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10xN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10xP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10yF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10yN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10yP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10zF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10zN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ10zP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20xF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20xN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20xP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20yF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20yN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20yP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20zF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20zN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DΨrDχ20zP As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		InverseOnePlusZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Parameters As CaseParametersClass
	#tag EndProperty

	#tag Property, Flags = &h0
		V As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VDot As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ve As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VeCosΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		VeSinΘ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		αDot As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Φ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ψr0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrF As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrN As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΨrP As Double
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
			Name="ΨrN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Cosι"
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
			Name="DvDotDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDotDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDotDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDotDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDotDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDotDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDotDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DvDotDχ20z"
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
			Name="DvDδ"
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
			Name="DαDotDV0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDotDδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDotDχ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDotDχ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDotDχ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDotDχ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαdotDχ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DαDotDχ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Dτr"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDV0F"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDV0N"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDV0P"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDZN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDδF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDδN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDδP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΘF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΘN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΘP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΦF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΦN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDΦP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10xF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10xN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10xP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10yF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10yN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10yP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10zF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10zN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ10zP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20xF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20xN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20xP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20yF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20yN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20yP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20zF"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20zN"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DΨrDχ20zP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="InverseOnePlusZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
			Name="V0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VDot"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Ve"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VeCosΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="VeSinΘ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="αDot"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Ψr0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΨrP"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Φ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
