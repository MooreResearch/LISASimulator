#tag Class
Protected Class CaseParametersClass
	#tag Method, Flags = &h0
		Function GetTweaked(Which As Item, ε As Double) As CaseParametersClass
		  // This method creates a parameter list where the specified parameter has
		  // been tweaked by the value ε to serve as a parameters list for side cases.
		  // For this method to work properly, the current parameter class must have been
		  // fleshed out by the CaseSupervisor.
		  Var CP As New CaseParametersClass
		  // Copy over parameters. Note that the SolveFor list is
		  // *not* copied, as that is only relevant to the base case.
		  CP.Detectors = Detectors
		  CP.F0 = F0
		  CP.GM = GM
		  CP.GMΩe = GMΩe
		  CP.M1 = M1
		  CP.M2 = M2
		  CP.PNOrder = PNOrder
		  CP.R = R
		  CP.R0 = R0
		  CP.RunDuration = RunDuration
		  CP.V0 = V0
		  CP.Ve = Ve
		  CP.Z = Z
		  CP.β = β
		  CP.δ = δ
		  CP.ΔT = ΔT
		  CP.η = η
		  CP.Θ = Θ
		  CP.Λ = Λ
		  CP.λ0 = λ0
		  CP.π = π
		  CP.ρ0 = ρ0
		  CP.Φ = Φ
		  CP.χ10x = χ10x
		  CP.χ10y = χ10y
		  CP.χ10z = χ10z
		  CP.χ20x = χ20x
		  CP.χ20y = χ20y
		  CP.χ20z = χ20z
		  CP.ψ = ψ
		  // Now tweak the appropriate item. Note that this method *assumes*
		  // that it will be called once with a positive value of ε and once with a
		  // negative value *of the same magnitude* when calculating InvDε,
		  // which is one over the value of the denominator for the centered difference
		  // when calculating derivatives from side cases. Note also that a nonzero
		  // value for this quantity indicates that it is a side case
		  Select Case Which
		  Case Item.M1
		    Var M1Tweaked As Double = M1*(1+ε)
		    Var InverseMTweaked As Double = 1.0/(M1Tweaked + M2)
		    CP.M1 = M1Tweaked
		    CP.δ = (M1Tweaked - M2)*InverseMTweaked
		    CP.η = M1Tweaked*M2*InverseMTweaked
		    CP.InvDε = 0.5/(M1*Abs(ε))
		  Case Item.M2
		    Var M2Tweaked As Double = M2*(1+ε)
		    Var InverseMTweaked As Double = 1.0/(M2Tweaked + M1)
		    CP.M2 = M2Tweaked
		    CP.δ = (M1 - M2Tweaked)*InverseMTweaked
		    CP.η = M1*M2Tweaked*InverseMTweaked
		    CP.InvDε = 0.5/(M2*Abs(ε))
		  Case Item.v0
		    CP.V0 = V0*(1+ε)
		    CP.InvDε = 0.5/(V0*Abs(ε))
		  Case Item.Λ
		    CP.R = Λ*(1 + ε)
		    CP.InvDε = 0.5/(Λ*Abs(ε))
		  Case Item.β
		    CP.β = β+ε
		    CP.InvDε = 0.5/Abs(ε)
		    UseBasePhase = True
		  Case Item.ψ
		    CP.ψ = ψ+ε
		    CP.InvDε = 0.5/Abs(ε)
		    UseBaseAmplitude = True
		    UseBasePhase = True
		  Case Item.Θ
		    CP.Θ = Θ+ε
		    CP.InvDε = 0.5/Abs(ε)
		    UseBaseAmplitude = True
		  Case Item.Φ
		    CP.Φ = Φ+ε
		    CP.InvDε = 0.5/Abs(ε)
		    UseBaseAmplitude = True
		  Case Item.χ10x
		    CP.χ10x = χ10x+ε
		    CP.InvDε = 0.5/Abs(ε)
		  Case Item.χ10y
		    CP.χ10y = χ10y+ε
		    CP.InvDε = 0.5/Abs(ε)
		  Case Item.χ10z
		    CP.χ10z = χ10z+ε
		    CP.InvDε = 0.5/Abs(ε)
		  Case Item.χ20x
		    CP.χ20x = χ20x+ε
		    CP.InvDε = 0.5/Abs(ε)
		  Case Item.χ20y
		    CP.χ20y = χ20y+ε
		    CP.InvDε = 0.5/Abs(ε)
		  Case Item.χ20z
		    CP.χ20z = χ20z+ε
		    CP.InvDε = 0.5/Abs(ε)
		  End Select
		  Return CP
		  
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		Detectors As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		F0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		GM As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		GMΩe As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		H0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		InvDε As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		M1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		M2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		PNOrder As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		R As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		R0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		RunDuration As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForH0 As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForV0 As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForZ As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForβ As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForδ As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForΘ As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForλ0 As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForΦ As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForχ10x As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForχ10y As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForχ10z As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForχ20x As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForχ20y As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForχ20z As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForψ As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		UseBaseAmplitude As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		UseBasePhase As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		V0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ve As Double = 0.993362e-5
	#tag EndProperty

	#tag Property, Flags = &h0
		Z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		δ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ΔT As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		η As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Λ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		λ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		π As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ρ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Φ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ10x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ10y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ10z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ20x As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ20y As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ20z As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ψ As Double
	#tag EndProperty


	#tag Enum, Name = Item, Type = Integer, Flags = &h0
		M1
		  M2
		  v0
		  Λ
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
			Name="Detectors"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="GMΩe"
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
			Name="Ve"
			Visible=false
			Group="Behavior"
			InitialValue="0.993362e-5"
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="δ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ΔT"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Θ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="λ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ρ0"
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
		#tag ViewProperty
			Name="χ10x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ10y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ10z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ20x"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ20y"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ20z"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="F0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="M1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="M2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="RunDuration"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="η"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="π"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForH0"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForV0"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForZ"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForβ"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForδ"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForΘ"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForλ0"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForΦ"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForχ10x"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForχ10y"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForχ10z"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForχ20x"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForχ20y"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForχ20z"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForψ"
			Visible=false
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="GM"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PNOrder"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="InvDε"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="R"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="R0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="UseBaseAmplitude"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="UseBasePhase"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Λ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
