#tag Class
Protected Class CaseParametersClass
	#tag Method, Flags = &h0
		Function Clone() As CaseParametersClass
		  Var P As New CaseParametersClass
		  P.Detectors = Detectors
		  P.DZDR = DZDR
		  P.GM = GM
		  P.GMΩe = GMΩe
		  P.IVOnePlusZ = IVOnePlusZ
		  P.M = M
		  P.PNOrder = PNOrder
		  P.R = R
		  P.RunDuration = RunDuration
		  P.SolveForM = SolveForM
		  P.SolveForδ = SolveForδ
		  P.SolveForR = SolveForR
		  P.SolveForV0 = SolveForV0
		  P.SolveForβ = SolveForβ
		  P.SolveForΘ = SolveForΘ
		  P.SolveForλ0 = SolveForλ0
		  P.SolveForΦ = SolveForΦ
		  P.SolveForχ1 = SolveForχ1
		  P.SolveForχ10x = SolveForχ10x
		  P.SolveForχ10y = SolveForχ10y
		  P.SolveForχ10z = SolveForχ10z
		  P.SolveForχ2 = SolveForχ2
		  P.SolveForχ20x = SolveForχ20x
		  P.SolveForχ20y = SolveForχ20y
		  P.SolveForχ20z = SolveForχ20z
		  P.SolveForψ = SolveForψ
		  P.T0 = T0
		  P.V0 = V0
		  P.Ve = Ve
		  P.Year = Year
		  P.Z = Z
		  P.β = β
		  P.δ = δ
		  P.ΔT = ΔT
		  P.Θ = Θ
		  P.Λ = Λ
		  P.λ0 = λ0
		  P.π = π
		  P.ρ0 = ρ0
		  P.Φ = Φ
		  P.χ10x = χ10x
		  P.χ10y = χ10y
		  P.χ10z = χ10z
		  P.χ20x = χ20x
		  P.χ20y = χ20y
		  P.χ20z = χ20z
		  P.ψ = ψ
		  Return P
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub FinishConstruction()
		  // This method takes the information provided by the main window and fleshes out the parameter list.
		  // This assumes that M is in solar masses, F0 (the leading-order gravitational wave frequency
		  // in the detector frame) is in mHz, R in lightyears, and angles in degrees.
		  // The spin variables are assumed to be already unitless (in units of the star's squared  mass).
		  // The run duration is assumed to be in years, but the step time is in seconds.
		  
		  Year = 3.15576e7  // length of the year in seconds
		  GM = 4.9267e-6*M   // the mass of the sun in seconds times the mass of the system in solar masses
		  R = R*Year // get R in seconds
		  Var universe As New UniverseClass(R) // Create a universe class to solve the Z(R) problem
		  Z = universe.GetZ // get the Z value for the given value of R
		  DZDR = universe.GetDZDR // get the derivative of Z with respect to R
		  IVOnePlusZ = 1.0/(1.0 + Z)
		  π = 3.14159265358979324  // record the value of pi so that we only have to define it once
		  V0 = Pow(GM*2.0*π*(1.0 + Z)/T0,1/3)  // Initialize V0
		  // convert all angles from radians to degrees
		  Var radiansFromDegrees As Double = π/180.0
		  β = radiansFromDegrees*β
		  ψ = radiansFromDegrees*ψ
		  λ0 = radiansFromDegrees*λ0
		  ρ0 = radiansFromDegrees*ρ0
		  Θ = radiansFromDegrees*Θ
		  Φ = radiansFromDegrees*Φ
		  Ve = 9.936e-5   //Average orbital speed of the LISA detector
		  GMΩe = GM*1.99213231e-7 //Unitless value of LISA's orbital frequency
		  // If we are generating a solution for any component of spin 1, we must generate the solution for all
		  SolveForχ1 = SolveForχ10x or SolveForχ10y or SolveForχ10z
		  // the same for spin 2
		  SolveForχ2 = SolveForχ20x or SolveForχ20y or SolveForχ20z
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		Detectors As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		DZDR As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		GM As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		GMΩe As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		IVOnePlusZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		M As Double
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
		SolveForM As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForR As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		SolveForV0 As Boolean = True
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
		SolveForχ1 As Boolean
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
		SolveForχ2 As Boolean
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
		T0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		V0 As Double
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
		M
		  delta
		  T0
		  R
		  beta
		  psi
		  lambda0
		  theta
		  phi
		  chi10x
		  chi10y
		  chi10z
		  chi20x
		  chi20y
		chi20z
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
			Name="SolveForM"
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
			Name="SolveForR"
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
			Name="R"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
		#tag ViewProperty
			Name="IVOnePlusZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForχ1"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="SolveForχ2"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
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
	#tag EndViewBehavior
End Class
#tag EndClass
