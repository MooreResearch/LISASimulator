#tag Class
Protected Class CaseParametersClass
	#tag Method, Flags = &h0
		Function Clone() As CaseParametersClass
		  // This creates a clone of the current parameter list
		  Var CP As New CaseParametersClass
		  CP.Detectors = Detectors
		  CP.F0 = F0
		  CP.GMΩe = GMΩe
		  CP.H0 = H0
		  CP.M1 = M1
		  CP.M2 = M2
		  CP.R = R
		  CP.RunDuration = RunDuration
		  CP.V0 = V0
		  CP.Ve = Ve
		  CP.Z = Z
		  CP.β = β
		  CP.δ = δ
		  CP.η = η
		  CP.ΔT = ΔT
		  CP.Θ = Θ
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
		  return CP
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTweaked(Which As Item, ε As Double) As CaseParametersClass
		  // This method creates a parameter list where the specified parameter has
		  // been tweaked by the value ε.
		  // For this method to work properly, the current parameter class must have been
		  // fleshed out by the CaseSupervisor.
		  Var CP As CaseParametersClass = Clone
		  Select Case Which
		  Case Item.δ
		    CP.δ = δ+ε
		    CP.η = 0.25*(1.0-(δ+ε)^2)
		  Case Item.v0
		    CP.V0 = V0+V0*ε
		  Case Item.χ10x
		    CP.χ10x = χ10x+ε
		  Case Item.χ10y
		    CP.χ10y = χ10y+ε
		  Case Item.χ10z
		    CP.χ10z = χ10z+ε
		  Case Item.χ20x
		    CP.χ20x = χ20x+ε
		  Case Item.χ20y
		    CP.χ20y = χ20y+ε
		  Case Item.χ20z
		    CP.χ20z = χ20z+ε
		  End Select
		  
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
		v0
		  δ
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
			Name="H0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
			Name="R"
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
	#tag EndViewBehavior
End Class
#tag EndClass
