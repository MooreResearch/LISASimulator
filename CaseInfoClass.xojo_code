#tag Class
Protected Class CaseInfoClass
	#tag Method, Flags = &h21
		Private Sub CalculateH0()
		  H0 = 0.5*GM*(1.0 + δ*δ)/R // overall amplitude of the wave
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub CalculateMStuff()
		  // This method calculates values of GM, GMΩe, and H0, assuming M is in solar masses
		  
		  GM = 4.9267e-6*M   // the mass of the sun in seconds times the mass of the system in solar masses
		  GMΩe = GM*1.99213231e-7 //Unitless value of LISA's orbital frequency
		  CalculateH0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub CalculateRStuff()
		  // This calculates various values that depend on R
		  Var universe As New UniverseClass(R) // Create a universe class to solve the Z(R) problem
		  Z = universe.GetZ // get the Z value for the given value of R
		  DZDR = universe.GetDZDR // get the derivative of Z with respect to R
		  OneI1pZ = 1.0/(1.0 + Z)
		  CalculateH0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub Calculateτc()
		  // Calculate τc. it doesn't matter if this is completely accurate, as τc becomes the fundamental parameter,
		  // and its connection to T0 is only to allow the user to enter something more intuitive than the time to coalescence.
		  Var v0 As Double = Pow(GM*2.0*π/T0,1/3)
		  Var η As Double = 0.25*(1.0 - δ*δ)
		  Var Σℓ As Double = 0.5*((1.0-δ)*χ2*Cos(θ2) - (1.0+δ)*χ1*Cos(θ1))
		  Var Sℓ As Double = 0.25*((1.0+δ)*(1.0+δ)*χ1*Cos(θ1) + (1.0+δ)*(1.0+δ)*χ2*Cos(θ2))
		  Var T2 As Double = 32/3*(743/2688+11/32*η)
		  Var T3 As Double = 64/3*(47/40*Sℓ + δ*15/32*Σℓ-3/10*π)
		  Var T4 As Double = 64*(743/2688 + 11/32*η)^2 - 128/9*(1855099/14450688 + 56975/258048*η - 371/2048*η*η)
		  τc = 5/(256*η*v0^8)*(1.0 + T2*v0^2 + T3*v0^3 + T4*v0^4)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function Clone() As CaseInfoClass
		  // This generates a clone of the base parameter class.
		  // Note that Ve and Year do not need to be cloned, because they
		  // have defined default values. Note also that IsBaseCase, Vars2Plot,
		  // DataToFlie, and εValues are not cloned, because clones will be used
		  // only to create side cases.
		  
		  Var c As New CaseInfoClass
		  c.Detector = Detector
		  c.DetΘ0 = DetΘ0
		  c.Detρ0 = Detρ0
		  c.DetΦ0 = DetΦ0
		  c.DZDR = DZDR
		  c.GM = GM
		  c.GMΩe = GMΩe
		  c.H0 = H0
		  c.ID = ID
		  c.M = M
		  c.OneI1pZ = OneI1pZ
		  c.PNForA = PNForA
		  c.PNForΨ = PNForΨ
		  c.R = R
		  c.RunDuration = RunDuration
		  c.T0 = T0
		  c.Ve = Ve
		  c.Year = Year
		  c.Z = Z
		  c.β = β
		  c.δ = δ
		  c.ΔT = ΔT
		  c.Θ = Θ
		  c.θ1 = θ1
		  c.θ2 = θ2
		  c.λ0 = λ0
		  c.ρ0 = ρ0
		  c.τc = τc
		  c.Φ = Φ
		  c.φ1 = φ1
		  c.φ2 = φ2
		  c.χ1 = χ1
		  c.χ2 = χ2
		  c.ψ = ψ
		  Return c
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(Parameters as String, Epsilons as String, VarsToSave() as PlotItemClass, DataDestination as Boolean)
		  Var radiansFromDegrees As Double = 180.0/π
		  Var p As String = Parameters.ReplaceAll(" ", "") // clear out any spaces
		  Var params() As String = p.Split(",")
		  χ1 = params(ixχ1).ToDouble
		  θ1 = params(ixθ1).ToDouble * radiansFromDegrees
		  φ1 = params(ixφ1).ToDouble * radiansFromDegrees
		  χ2 = params(ixχ2).ToDouble
		  θ2 = params(ixθ2).ToDouble * radiansFromDegrees
		  φ2 = params(ixφ2).ToDouble * radiansFromDegrees
		  δ = params(ixδ).ToDouble
		  T0 = params(ixT).ToDouble
		  λ0 = params(ixλ0).ToDouble * radiansFromDegrees
		  M = params(ixM).ToDouble
		  β = params(ixβ).ToDouble * radiansFromDegrees
		  ψ = params(ixψ).ToDouble * radiansFromDegrees
		  R = params(ixR).ToDouble * Year
		  Θ = params(ixΘ).ToDouble * radiansFromDegrees
		  Φ = params(ixΦ).ToDouble * radiansFromDegrees
		  ΔT = params(ixΔT).ToDouble
		  RunDuration = params(ixDur).ToDouble * Year
		  PNForA = params(ixPNA).ToInteger
		  PNForΨ = params(ixPNΨ).ToInteger
		  Detector = New Fixed20 // Set up eventually from params(ixDet)
		  DetΘ0 = params(ixDetΘ0).ToDouble * radiansFromDegrees
		  DetΦ0 = params(ixDetΦ0).ToDouble * radiansFromDegrees
		  Detρ0 = params(ixDetρ0).ToDouble * radiansFromDegrees
		  ID = params(ixID)
		  CalculateMStuff // Calculate parameter values that depend on M
		  CalculateRStuff // Calculate quantities that depend on R
		  Calculateτc // Calculate τc from M, T0, χ1, χ2, θ1, θ2
		  IsBaseCase = True  // This is the base case
		  
		  // Get the epsilon values for this case
		  Var ep As String = Epsilons.ReplaceAll(" ", "")
		  Var epstrings() As String = ep.Split(",")
		  Var epValues() As Double
		  For i As Integer = 0 to LastParamIndex
		    Var ε As String = epstrings(i)
		    If ε.IsEmpty Then ε = εDefault  // empty field gets replaced with default
		    epValues.Add(ε.ToDouble)
		  Next
		  εValues = epValues
		  
		  // Get list of plot items
		  Vars2Save = VarsToSave
		  
		  // Set where plot items are to be saved
		  DataToFile = DataDestination
		  
		  // Initialize the detector
		  Detector.Initialize(me)
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetDataDestination() As Boolean
		  Return DataToFile
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetTweakedClone(PIndex As Integer, Sign As Integer) As CaseInfoClass
		  // This creates a new side case from the base class and tweaks the
		  // the specified parameter
		  Var c As CaseInfoClass = Clone
		  Var myε As Double = εValues(PIndex)*Sign
		  If myε <> 0.0 Then
		    Select Case PIndex
		    Case Dχ1
		      c.χ1 = c.χ1 + myε
		    Case Dθ1
		      c.θ1 = c.θ1 + myε
		    Case Dφ1
		      c.φ1 = c.φ1 + myε
		    Case Dχ2
		      c.χ2 = c.χ2 + myε
		    Case Dθ2
		      c.θ2 = c.θ2 + myε
		    Case Dφ2
		      c.φ2 = c.φ2 + myε
		    Case Dδ
		      c.δ = c.δ + myε
		      CalculateH0
		    Case Dlnτc
		      c.τc = c.τc*(1.0 + myε)
		    Case Dλ0
		      c.λ0 = c.λ0 + myε
		    Case DlnM
		      c.M = c.M*(1.0 + myε)
		      CalculateMStuff
		    Case Dβ
		      c.β = c.β + myε
		    Case Dψ
		      c.ψ = c.ψ + myε
		    Case DlnR
		      c.R = c.R*(1.0 + myε)
		      CalculateRStuff
		    Case DΘ
		      c.Θ = c.Θ + myε
		    Case DΦ
		      c.Φ = c.Φ + myε
		    End Select
		  End If
		  Return c
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetVars2Save() As PlotItemClass()
		  Return Vars2Save
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetεVals() As Double()
		  Return εValues
		End Function
	#tag EndMethod


	#tag Property, Flags = &h21
		Private DataToFile As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		Detector As DetectorInterface
	#tag EndProperty

	#tag Property, Flags = &h0
		DetΘ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		Detρ0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		DetΦ0 As Double
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
		H0 As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		ID As String = "Disp"
	#tag EndProperty

	#tag Property, Flags = &h0
		IsBaseCase As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h0
		M As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		OneI1pZ As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		PNForA As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		PNForΨ As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		R As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		RunDuration As Double
	#tag EndProperty

	#tag Property, Flags = &h0
		T0 As Double
	#tag EndProperty

	#tag Property, Flags = &h21
		Private Vars2Save() As PlotItemClass
	#tag EndProperty

	#tag Property, Flags = &h0
		Ve As Double = 0.993362e-5
	#tag EndProperty

	#tag Property, Flags = &h0
		Year As Double = 31556952.0
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

	#tag Property, Flags = &h21
		Private εValues() As Double
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
			Name="RunDuration"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ID"
			Visible=false
			Group="Behavior"
			InitialValue="False"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="DetΘ0"
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
			Name="GM"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
			Name="IsBaseCase"
			Visible=false
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
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
			Name="OneI1pZ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PNForA"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="PNForΨ"
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
			Name="T0"
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
			Name="Year"
			Visible=false
			Group="Behavior"
			InitialValue="31556952.0"
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
			Name="θ1"
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
			Name="τc"
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
			Name="φ1"
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
			Name="χ1"
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
			Name="ψ"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Detρ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DetΦ0"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Double"
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
	#tag EndViewBehavior
End Class
#tag EndClass
