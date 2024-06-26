#tag Class
Protected Class CaseInfoClass
	#tag Method, Flags = &h0
		Function Clone() As CaseInfoClass
		  Var P As New CaseInfoClass
		  P.Detectors = Detectors
		  P.DZDR = DZDR
		  P.GM = GM
		  P.GMΩe = GMΩe
		  P.M = M
		  P.OneI1pZ = OneI1pZ
		  P.PNOrder = PNOrder
		  P.R = R
		  P.RunDuration = RunDuration
		  P.SolveFor.ResizeTo(SolveFor.LastIndex)
		  For i As Integer = 0 to SolveFor.LastIndex
		    P.SolveFor(i) = SolveFor(i)
		  Next
		  P.T0 = T0
		  P.Ve = Ve
		  P.Year = Year
		  P.Z = Z
		  P.β = β
		  P.δ = δ
		  P.ΔT = ΔT
		  P.Θ = Θ
		  P.λ0 = λ0
		  P.π = π
		  P.ρ0 = ρ0
		  P.τc = τc
		  P.Φ = Φ
		  P.χ1 = χ1
		  P.θ1 = θ1
		  P.φ1 = φ1
		  P.χ2 = χ2
		  P.θ2 = θ2
		  P.φ2 = φ2
		  P.ψ = ψ
		  Return P
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub FinishConstruction()
		  // This method takes the information provided by the main window and fleshes out the parameter list.
		  // This assumes that M is in solar masses, T0 (the binary orbital period in the solar-system frame) is in s,
		  // R in lightyears, and angles in degrees.
		  // The spin variables are assumed to be already unitless (in units of the star's squared  mass).
		  // The run duration is assumed to be in years, but the step time is in seconds.
		  
		  Uncertainties.ResizeTo(14)  // We have 15 fundamental parameters
		  Year = 3.15576e7  // length of the year in seconds
		  GM = 4.9267e-6*M   // the mass of the sun in seconds times the mass of the system in solar masses
		  Ve = 9.936e-5   //Average orbital speed of the LISA detector in units of c
		  GMΩe = GM*1.99213231e-7 //Unitless value of LISA's orbital frequency
		  R = R*Year // get R in seconds
		  Var universe As New UniverseClass(R) // Create a universe class to solve the Z(R) problem
		  Z = universe.GetZ // get the Z value for the given value of R
		  DZDR = universe.GetDZDR // get the derivative of Z with respect to R
		  OneI1pZ = 1.0/(1.0 + Z)
		  π = 3.14159265358979324  // record the value of pi so that we only have to define it once
		  
		  // convert all angles from radians to degrees
		  Var radiansFromDegrees As Double = π/180.0
		  β = radiansFromDegrees*β
		  ψ = radiansFromDegrees*ψ
		  λ0 = radiansFromDegrees*λ0
		  ρ0 = radiansFromDegrees*ρ0
		  Θ = radiansFromDegrees*Θ
		  Φ = radiansFromDegrees*Φ
		  θ1 = radiansFromDegrees*θ1
		  φ1 = radiansFromDegrees*φ1
		  θ2 = radiansFromDegrees*θ2
		  φ2 = radiansFromDegrees*φ2
		  
		  // Calculate τc. it doesn't matter if this is completely accurate, as τc becomes the fundamental parameter,
		  // and its connection to T0 is only to allow the user to enter something more intuitive than the time to coalescence.
		  Var v0 As Double = Pow(GM*2.0*π*(1.0 + Z)/T0,1/3)
		  Var η As Double = 0.25*(1.0 - δ*δ)
		  Var Σℓ As Double = 0.5*((1.0-δ)*χ2*Cos(θ2) - (1.0+δ)*χ1*Cos(θ1))
		  Var Sℓ As Double = 0.25*((1.0+δ)*(1.0+δ)*χ1*Cos(θ1) + (1.0+δ)*(1.0+δ)*χ2*Cos(θ2))
		  Var T2 As Double = 32/3*(743/2688+11/32*η)
		  Var T3 As Double = 64/3*(47/40*Sℓ + δ*15/32*Σℓ-3/10*π)
		  Var T4 As Double = 64*(743/2688 + 11/32*η)^2 - 128/9*(1855099/14450688 + 56975/258048*η - 371/2048*η*η)
		  τc = 5/(256*η*v0^8)*(1.0 + T2*v0^2 + T3*v0^3 + T4*v0^4)
		  
		  // Initialize the DataWriter class
		  DataWriter = New DataWriterClass
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		DataWriter As DataWriterClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Detectors As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		DZDR As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		FromFile As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		GM As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		GMΩe As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		M As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		OneI1pZ As Double
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
		SolveFor() As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		T0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Uncertainties() As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Ve As Double = 0.993362e-5
	#tag EndProperty

	#tag Property, Flags = &h0
		Year As Double
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
		Θ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		θ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		θ2 As Double
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
		τc As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Φ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		φ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		φ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ1 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		χ2 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ψ As Double
	#tag EndProperty


	#tag Enum, Name = Param, Type = Integer, Flags = &h0
		M
		  delta
		  tauc
		  R
		  beta
		  psi
		  lambda0
		  theta
		  phi
		  chi1
		  theta1
		  phi1
		  chi2
		  theta2
		  phi2
		NItems
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
			Name="χ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="θ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="φ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="χ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="θ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="φ2"
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
			Name="T0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="M"
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
			Name="π"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
			Name="R"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="OneI1pZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DZDR"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Year"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="τc"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
