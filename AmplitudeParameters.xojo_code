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
		  Case Item.Cosι
		    εCosι = Myε
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
		Sub Update(Cosι As Double, χs As Vector, χa As Vector)
		  // Use this to update the amplitude parameters after every time step
		  // Add tweaking if there is any
		  χsx = χs.X + εχsx
		  χsy = χs.Y + εχsy
		  χsz = χs.Z + εχsz
		  χax = χa.X + εχax
		  χay = χa.Y + εχsy
		  χaz = χa.Z + εχsz
		  C2 = Cosι + εCosι
		  C1 = Sqrt(0.5*(1+C2))
		  Var C13 As Double = C1*C1*C1
		  Var C15 As Double = C13*C1*C1
		  Var C17 As Double = C13*C13*C1
		  C3 = -3*C1 + 4*C13
		  C4 = 2*C2*C2-1
		  C5 = 5*C1 - 20*C13 + 16*C15
		  C6 = 2*C3*C3-1
		  C7 = 64*C17 - 112*C15 + 56*C13 - 7*C1
		  C8 = 2*C4*C4 - 1
		  C9 = -3*C3 + 4*C3*C3*C3
		  C10 = 2*C5*C5-1
		  S1 = Sqrt(0.5*(1-C2))
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
		C2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C3β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C4β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		C5β As Double
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
		Cβ As Double
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
		S2β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S3 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S3β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S4 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S4β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S5 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		S5β As Double
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
		Sβ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		β As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		δ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		εCosι As Double
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
		  Cosι
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
			Name="β"
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
			Name="εχax"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="εCosι"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="εχay"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="εχaz"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="εχsx"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="εχsy"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="εχsz"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="εβ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="εδ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
