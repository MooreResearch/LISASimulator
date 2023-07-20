#tag Class
Protected Class AmplitudeParameters
	#tag Method, Flags = &h0
		Sub Constructor(Parameters As CaseParametersClass, Tweak As Item, Myε As Double)
		  // Initialize the fixed parameters
		  δ = Parameters.δ
		  η = Parameters.η
		  Var β As Double = Parameters.β
		  // Deal with tweaked cases
		  Select Case Tweak
		  Case Item.δ
		    δ = δ + Myε
		    η = 0.25*(1-δ*δ)
		    εδ = Myε
		  Case Item.β
		    β = β + Myε
		    εβ = Myε
		  Case Item.ι
		    ει = Myε
		  Case Item.χax
		    εχax = Myε
		  Case Item.χay
		    εχay = Myε
		  Case Item.χaz
		    εχaz = Myε
		  Case Item.χsx
		    εχsx = Myε
		  Case Item.χsy
		    εχsy = Myε
		  Case Item.χsz
		    εχsz = Myε
		  End Select
		  // Set up cosines and sines of β
		  Cβ = Cos(β)
		  Sβ = Sin(β)
		  C2β = Cos(2*β)
		  S2β = Sin(2*β)
		  C3β = Cos(3*β)
		  S3β = Sin(3*β)
		  C4β = Cos(4*β)
		  S4β = Sin(4*β)
		  C5β = Cos(5*β)
		  S5β = Sin(5*β)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Update(ι As Double, α As Double, Ψr As Double, χa As Vector, χs As Vector)
		  // Use this to update the amplitude parameters after every time step
		  // Add tweaking if there is any.
		  χsx = χs.X + εχsx
		  χsy = χs.Y + εχsy
		  χsz = χs.Z + εχsz
		  χax = χa.X + εχax
		  χay = χa.Y + εχsy
		  χaz = χa.Z + εχsz
		  
		  // Calculate trig functions based on iota
		  C2 =  Cos(ι+ει)
		  S1 = Sin(0.5*(ι+ει))
		  C1 = Cos(0.5*(ι+ει))
		  Var C13 As Double = C1*C1*C1
		  Var C15 As Double = C13*C1*C1
		  Var C17 As Double = C13*C13*C1
		  C3 = -3*C1 + 4*C13
		  C4 = 2*C2*C2-1.0
		  C5 = 5*C1 - 20*C13 + 16*C15
		  C6 = 2*C3*C3-1.0
		  C7 = 64*C17 - 112*C15 + 56*C13 - 7*C1
		  C8 = 2*C4*C4 - 1.0
		  C9 = -3*C3 + 4*C3*C3*C3
		  C10 = 2*C5*C5-1
		  Var S13 As Double = S1*S1*S1
		  Var S15 As Double = S13*S1*S1
		  Var S17 As Double = S13*S13*S1
		  S2 = 2*C1*S1
		  S3 = 3*S1 - 4*S13
		  S4 = 2*C2*S2
		  S5 = 5*S1 - 20*S13 + 16*S15
		  S6 = 2*C3*S3
		  S7 = 7*S1 - 56*s13 +112*S15 - 64*S17
		  S8 = 2*C4*S4
		  S9 = 3*S3 - 4*S3*S3*S3
		  S10 = 2*C5*S5
		  
		  // Calculate trig functions based on alpha and Ψr
		  Cα = Cos(α)
		  C2α = 2*Cα*Cα-1.0
		  C3α = 4*Cα*Cα*Cα - 3*Cα
		  C4α = 2*C2α*C2α - 1.0
		  C5α = 16*Cα*Cα*Cα*Cα*Cα - 20*Cα*Cα*Cα + 5*Cα
		  Sα = Sin(α)
		  S2α = 2*Sα*Cα
		  S3α = 3*Sα - 4*Sα*Sα*Sα
		  S4α = 2*S2α*C2α
		  S5α = 5*Sα - 20*Sα*Sα*Sα + 16*Sα*Sα*Sα*Sα*Sα
		  CΨ = Cos(Ψr)
		  C2Ψ = 2*CΨ*CΨ-1.0
		  C3Ψ = 4*CΨ*CΨ*CΨ - 3*CΨ
		  C4Ψ = 2*C2Ψ*C2Ψ - 1.0
		  C5Ψ = 16*CΨ*CΨ*CΨ*CΨ*CΨ - 20*CΨ*CΨ*CΨ + 5*CΨ
		  SΨ = Sin(Ψr)
		  S2Ψ = 2*SΨ*CΨ
		  S3Ψ = 3*SΨ - 4*SΨ*SΨ*SΨ
		  S4Ψ = 2*S2Ψ*C2Ψ
		  S5α = 5*SΨ - 20*SΨ*SΨ*SΨ + 16*SΨ*SΨ*SΨ*SΨ*SΨ
		  
		  
		  
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		C1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C10 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C2α As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C2Ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C3α As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C3β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C3Ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C4α As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C4β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C4Ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C5α As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C5β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C5Ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C8 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Cα As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Cβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		CΨ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S10 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S2α As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S2Ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S3α As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S3β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S3Ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S4α As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S4β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S4Ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S5α As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S5β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S5Ψ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S6 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S7 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S8 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S9 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sα As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Sβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		SΨ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		δ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ει As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		εβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		εδ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		εχax As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		εχay As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		εχaz As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		εχsx As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		εχsy As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		εχsz As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		η As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χax As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χay As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χaz As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsx As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsy As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χsz As Double
	#tag EndProperty


	#tag Enum, Name = Item, Type = Integer, Flags = &h0
		None
		  δ
		  ι
		  β
		  χax
		  χay
		  χaz
		  χsx
		  χsy
		χsz
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
			Name="δ"
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
			Name="Cβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C2β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C3β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C4β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C5β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S2β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S3β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S4β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S5β"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S3"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S4"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S5"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S6"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χax"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χay"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χaz"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsx"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsy"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χsz"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C10"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C7"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C8"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S10"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S7"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S8"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S9"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Sα"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S2α"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S3α"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S4α"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="S5α"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Cα"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C2α"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C3α"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C4α"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C5α"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CΨ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C2Ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C3Ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C4Ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="C5Ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SΨ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
